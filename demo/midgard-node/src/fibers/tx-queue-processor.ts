import { fromHex } from "@lucid-evolution/lucid";
import { Effect, Metric, pipe, Ref, Schedule } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { MempoolDB } from "@/database/index.js";
import { breakDownTx, ProcessedTx } from "@/utils.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  Database,
  TxIngressMessage,
  TxIngressQueue,
  TxIngressQueueError,
} from "@/services/index.js";

const TX_QUEUE_PERSIST_CHUNK_SIZE = 100;

const txStreamDepthGauge = Metric.gauge("tx_stream_depth", {
  description:
    "Tx ingress Redis stream depth sampled before each consume cycle",
  bigint: true,
}).register();

const txStreamPeakDepthGauge = Metric.gauge("tx_stream_depth_peak", {
  description:
    "High-water mark of Redis stream depth since fiber startup to preserve burst visibility between scrapes",
  bigint: true,
}).register();

const txStreamPendingGauge = Metric.gauge("tx_stream_pending", {
  description: "Pending entry count in the Redis consumer group for tx ingress",
  bigint: true,
}).register();

const txStreamConsumerLagGauge = Metric.gauge("tx_stream_consumer_lag", {
  description:
    "Approximate number of stream entries that are not currently pending in the consumer group",
  bigint: true,
}).register();

const txStreamAckCounter = Metric.counter("tx_stream_ack_total", {
  description: "Acknowledged tx ingress messages after durable mempool insert",
  bigint: true,
  incremental: true,
}).register();

const txStreamProcessingFailCounter = Metric.counter("tx_stream_fail_total", {
  description:
    "Tx ingress message processing failures before durable insert, including malformed CBOR and persistence failures",
  bigint: true,
  incremental: true,
}).register();

const txStreamRetryCounter = Metric.counter("tx_stream_retry_total", {
  description:
    "Tx ingress messages left pending for retry because delivery attempts have not reached dead-letter threshold",
  bigint: true,
  incremental: true,
}).register();

const txStreamDeadLetterCounter = Metric.counter(
  "tx_stream_dead_letter_total",
  {
    description:
      "Tx ingress messages moved to the dead-letter stream after exceeding max delivery attempts",
    bigint: true,
    incremental: true,
  },
).register();

const txMempoolAcceptedCounter = Metric.counter(
  "tx_submissions_mempool_accepted",
  {
    description:
      "A counter for tracking L2 transactions durably inserted into the mempool after CBOR deserialization and semantic breakdown",
    bigint: true,
    incremental: true,
  },
).register();

export const txQueueProcessorMetrics = {
  txStreamDepthGauge,
  txStreamPeakDepthGauge,
  txStreamPendingGauge,
  txStreamConsumerLagGauge,
  txStreamAckCounter,
  txStreamProcessingFailCounter,
  txStreamRetryCounter,
  txStreamDeadLetterCounter,
  txMempoolAcceptedCounter,
} as const;

const partitionMessages = (
  messages: readonly TxIngressMessage[],
  chunkSize: number,
): readonly (readonly TxIngressMessage[])[] => {
  if (messages.length === 0) {
    return [];
  }

  const chunks: TxIngressMessage[][] = [];
  for (
    let startIndex = 0;
    startIndex < messages.length;
    startIndex += chunkSize
  ) {
    chunks.push(messages.slice(startIndex, startIndex + chunkSize));
  }
  return chunks;
};

const registerFailedMessage = (
  message: TxIngressMessage,
  reason: string,
  withMonitoring?: boolean,
) =>
  Effect.gen(function* () {
    const txIngressQueue = yield* TxIngressQueue;
    if (withMonitoring) {
      yield* Metric.increment(txStreamProcessingFailCounter);
    }

    const disposition = yield* txIngressQueue.handleFailedMessage(
      message,
      reason,
    );
    if (!withMonitoring) {
      return;
    }

    if (disposition === "dead_lettered") {
      yield* Metric.increment(txStreamDeadLetterCounter);
    } else {
      yield* Metric.increment(txStreamRetryCounter);
    }
  });

export const txQueueProcessorAction = (
  txQueueDrainBatchSize: number,
  txParseConcurrency: number,
  streamBlockMs: number,
  withMonitoring?: boolean,
  peakRef?: Ref.Ref<bigint>,
): Effect.Effect<
  void,
  | DatabaseError
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | TxIngressQueueError,
  Database | TxIngressQueue
> =>
  Effect.gen(function* () {
    const txIngressQueue = yield* TxIngressQueue;

    const snapshot = yield* txIngressQueue.snapshotMetrics;
    if (withMonitoring) {
      yield* Metric.set(txStreamDepthGauge, BigInt(snapshot.streamDepth));
      yield* Metric.set(txStreamPendingGauge, BigInt(snapshot.pendingCount));
      yield* Metric.set(txStreamConsumerLagGauge, BigInt(snapshot.lagCount));
      if (peakRef !== undefined) {
        const newPeak = yield* Ref.updateAndGet(peakRef, (prev) =>
          prev >= BigInt(snapshot.streamDepth)
            ? prev
            : BigInt(snapshot.streamDepth),
        );
        yield* Metric.set(txStreamPeakDepthGauge, newPeak);
      }
    }

    const messages = yield* txIngressQueue.consumeBatch(
      txQueueDrainBatchSize,
      streamBlockMs,
    );

    if (messages.length === 0) {
      return;
    }

    const messageChunks = partitionMessages(
      messages,
      TX_QUEUE_PERSIST_CHUNK_SIZE,
    );

    for (const messageChunk of messageChunks) {
      const parsedOutcomes = yield* Effect.forEach(
        messageChunk,
        (message) =>
          breakDownTx(fromHex(message.txCbor)).pipe(
            Effect.either,
            Effect.map((parsed) => ({ message, parsed })),
          ),
        {
          concurrency: txParseConcurrency,
        },
      );

      const validMessages: TxIngressMessage[] = [];
      const processedTxs: ProcessedTx[] = [];

      for (const outcome of parsedOutcomes) {
        if (outcome.parsed._tag === "Left") {
          const error = outcome.parsed.left;
          yield* Effect.logWarning(
            `Dropping malformed tx for stream message ${outcome.message.id}; CBOR deserialization failed: ${error.message}`,
          );
          yield* registerFailedMessage(
            outcome.message,
            `malformed_cbor:${error.message}`,
            withMonitoring,
          );
          continue;
        }

        validMessages.push(outcome.message);
        processedTxs.push(outcome.parsed.right);
      }

      if (processedTxs.length === 0) {
        continue;
      }

      const persistResult = yield* Effect.either(
        MempoolDB.insertMultiple(processedTxs),
      );
      if (persistResult._tag === "Left") {
        const reason = `mempool_insert_failed:${persistResult.left.message}`;
        yield* Effect.logWarning(
          `Failed to persist ${processedTxs.length} tx(s) from stream chunk; keeping entries pending for retry: ${persistResult.left.message}`,
        );
        yield* Effect.forEach(
          validMessages,
          (message) => registerFailedMessage(message, reason, withMonitoring),
          { discard: true },
        );
        continue;
      }

      const ackedCount = yield* txIngressQueue.ack(
        validMessages.map((m) => m.id),
      );
      if (withMonitoring && ackedCount > 0) {
        yield* Metric.incrementBy(txStreamAckCounter, BigInt(ackedCount));
      }

      if (withMonitoring && persistResult.right > 0) {
        yield* Metric.incrementBy(
          txMempoolAcceptedCounter,
          BigInt(persistResult.right),
        );
      }
    }
  });

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueueDrainBatchSize: number,
  txParseConcurrency: number,
  streamBlockMs: number,
  withMonitoring?: boolean,
): Effect.Effect<void, never, Database | TxIngressQueue> =>
  pipe(
    Effect.gen(function* () {
      const txIngressQueue = yield* TxIngressQueue;
      yield* txIngressQueue.ensureConsumerGroup;
      yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
      const peakRef = yield* Ref.make(0n);

      if (withMonitoring) {
        yield* Metric.set(txStreamDepthGauge, 0n);
        yield* Metric.set(txStreamPeakDepthGauge, 0n);
        yield* Metric.set(txStreamPendingGauge, 0n);
        yield* Metric.set(txStreamConsumerLagGauge, 0n);
        yield* Metric.incrementBy(txStreamAckCounter, 0n);
        yield* Metric.incrementBy(txStreamProcessingFailCounter, 0n);
        yield* Metric.incrementBy(txStreamRetryCounter, 0n);
        yield* Metric.incrementBy(txStreamDeadLetterCounter, 0n);
        yield* Metric.incrementBy(txMempoolAcceptedCounter, 0n);
      }

      yield* Effect.repeat(
        txQueueProcessorAction(
          txQueueDrainBatchSize,
          txParseConcurrency,
          streamBlockMs,
          withMonitoring,
          withMonitoring ? peakRef : undefined,
        ).pipe(Effect.catchAllCause(Effect.logWarning)),
        schedule,
      );
    }),
  ).pipe(Effect.catchAllCause(Effect.logWarning));
