import { fromHex } from "@lucid-evolution/lucid";
import { Chunk, Effect, Metric, pipe, Queue, Ref, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { breakDownTx } from "@/utils.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

const TX_QUEUE_PERSIST_CHUNK_SIZE = 100;

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
      "A counter for tracking L2 transaction processing failures: incremented once per malformed-CBOR tx rejected during deserialization, and once per batch on mempool insertion errors",
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
  txQueueDrainBatchSize: number,
  withMonitoring?: boolean,
  peakRef?: Ref.Ref<bigint>,
): Effect.Effect<void, DatabaseError, Database> =>
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

    const txStringsChunk: Chunk.Chunk<string> = yield* txQueue.takeUpTo(
      txQueueDrainBatchSize,
    );
    const txStrings = Chunk.toReadonlyArray(txStringsChunk);
    let insertedTxCount = 0;

    for (
      let startIndex = 0;
      startIndex < txStrings.length;
      startIndex += TX_QUEUE_PERSIST_CHUNK_SIZE
    ) {
      const txStringsPersistChunk = txStrings.slice(
        startIndex,
        startIndex + TX_QUEUE_PERSIST_CHUNK_SIZE,
      );
      const [malformedErrors, processedTxs] = yield* Effect.partition(
        txStringsPersistChunk,
        (tx) => breakDownTx(fromHex(tx)),
      );
      for (const error of malformedErrors) {
        yield* Effect.logWarning(
          `Dropping malformed tx; CBOR deserialization failed: ${error.message}`,
        );
      }
      if (withMonitoring && malformedErrors.length > 0) {
        yield* Metric.incrementBy(
          txProcessingFailedCounter,
          BigInt(malformedErrors.length),
        );
      }
      if (processedTxs.length > 0) {
        insertedTxCount += yield* MempoolDB.insertMultiple(processedTxs);
      }
    }

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
  txQueueDrainBatchSize: number,
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
          txQueueDrainBatchSize,
          withMonitoring,
          withMonitoring ? peakRef : undefined,
        ).pipe(Effect.catchAllCause(Effect.logWarning)),
        schedule,
      );
    }),
  );
