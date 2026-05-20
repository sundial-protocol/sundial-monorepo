import { fromHex } from "@lucid-evolution/lucid";
import { Chunk, Effect, Metric, pipe, Queue, Ref, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { ProcessedTx, breakDownTx } from "@/utils.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Database } from "@/services/database.js";

const txQueueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "Tx queue size sampled before each drain cycle",
  bigint: true,
}).register();

const txQueuePeakSizeGauge = Metric.gauge("tx_queue_peak_size", {
  description:
    "High-water mark of tx queue size since fiber startup; never decreases on drain so Prometheus scrapes capture burst spikes between polling cycles",
  bigint: true,
}).register();

const txMempoolAcceptedCounter = Metric.counter(
  "tx_submissions_mempool_accepted",
  {
    description:
      "A counter for tracking L2 transactions durably inserted into the mempool after CBOR deserialization and semantic breakdown",
    bigint: true,
    incremental: true,
  },
).register();

const txProcessingFailedCounter = Metric.counter(
  "tx_submissions_processing_failed",
  {
    description:
      "A counter for tracking L2 transaction processing batch failures (CBOR deserialization or mempool insertion errors)",
    bigint: true,
    incremental: true,
  },
).register();

export const txQueueProcessorMetrics = {
  txQueueSizeGauge,
  txQueuePeakSizeGauge,
  txMempoolAcceptedCounter,
  txProcessingFailedCounter,
} as const;

export const txQueueProcessorAction = (
  txQueue: Queue.Dequeue<string>,
  withMonitoring?: boolean,
  peakRef?: Ref.Ref<bigint>,
): Effect.Effect<
  void,
  DatabaseError | SDK.CmlDeserializationError | SDK.DataCoercionError,
  Database
> =>
  Effect.gen(function* () {
    const queueSize = yield* txQueue.size;

    if (withMonitoring) {
      yield* Metric.set(txQueueSizeGauge, BigInt(queueSize));
      if (peakRef !== undefined) {
        const newPeak = yield* Ref.updateAndGet(peakRef, (prev) =>
          prev >= BigInt(queueSize) ? prev : BigInt(queueSize),
        );
        yield* Metric.set(txQueuePeakSizeGauge, newPeak);
      }
    }

    const txStringsChunk: Chunk.Chunk<string> = yield* Queue.takeAll(txQueue);
    const txStrings = Chunk.toReadonlyArray(txStringsChunk);
    const processedTxs: ProcessedTx[] = yield* Effect.forEach(txStrings, (tx) =>
      Effect.gen(function* () {
        return yield* breakDownTx(fromHex(tx));
      }),
    );
    const insertedTxCount = yield* MempoolDB.insertMultiple(processedTxs);
    if (withMonitoring && insertedTxCount > 0) {
      yield* Metric.incrementBy(
        txMempoolAcceptedCounter,
        BigInt(insertedTxCount),
      );
    }
  }).pipe(
    Effect.tapErrorCause(() =>
      withMonitoring
        ? Metric.increment(txProcessingFailedCounter)
        : Effect.void,
    ),
  );

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueue: Queue.Dequeue<string>,
  withMonitoring?: boolean,
): Effect.Effect<void, never, Database> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
      const peakRef = yield* Ref.make(0n);
      if (withMonitoring) {
        // Ensure metric series are initialized before the first queue sample.
        yield* Metric.set(txQueueSizeGauge, 0n);
        yield* Metric.set(txQueuePeakSizeGauge, 0n);
        yield* Metric.incrementBy(txMempoolAcceptedCounter, 0n);
        yield* Metric.incrementBy(txProcessingFailedCounter, 0n);
      }
      yield* Effect.repeat(
        txQueueProcessorAction(
          txQueue,
          withMonitoring,
          withMonitoring ? peakRef : undefined,
        ).pipe(Effect.catchAllCause(Effect.logWarning)),
        schedule,
      );
    }),
  );
