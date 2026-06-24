import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Metric, Ref } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { metricDelta } from "./harness/metric-snapshot.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";

const parseTxCborInWorkerPoolFn = vi.hoisted(() => vi.fn());
const mempoolValidateAndInsertFn = vi.hoisted(() => vi.fn());

vi.mock("@/fibers/tx-parse-worker-pool.js", () => ({
  parseTxCborInWorkerPool: parseTxCborInWorkerPoolFn,
}));

vi.mock("@/database/index.js", () => ({
  MempoolDB: {
    validateAndInsertMultiple: mempoolValidateAndInsertFn,
  },
}));

import {
  txQueueProcessorAction,
  txQueueProcessorMetrics,
} from "@/fibers/tx-queue-processor.js";
import {
  TxIngressQueue,
  TxIngressQueueService,
} from "@/services/tx-ingress-queue.js";

const sqlHarness = createMockSqlHarness();
const testLayer = Layer.mergeAll(sqlHarness.layer, makeTestNodeConfigLayer());

const fakeProcessedTx = {
  txId: Buffer.alloc(32, 0xaa),
  txCbor: Buffer.alloc(10, 0xbb),
  spent: [],
  produced: [],
};

const readAckCounter = Metric.value(txQueueProcessorMetrics.txStreamAckCounter);
const readRetryCounter = Metric.value(
  txQueueProcessorMetrics.txStreamRetryCounter,
);
const readDeadLetterCounter = Metric.value(
  txQueueProcessorMetrics.txStreamDeadLetterCounter,
);
const readFailCounter = Metric.value(
  txQueueProcessorMetrics.txStreamProcessingFailCounter,
);
const readPeakGauge = Metric.value(
  txQueueProcessorMetrics.txStreamPeakDepthGauge,
);

const makeQueueStub = (
  messages: readonly {
    id: string;
    txCbor: string;
    deliveryCount: number;
  }[],
  snapshot = { streamDepth: 0, pendingCount: 0, lagCount: 0 },
): TxIngressQueueService & {
  consumeBatchSpy: ReturnType<typeof vi.fn>;
  ackSpy: ReturnType<typeof vi.fn>;
  handleFailedSpy: ReturnType<typeof vi.fn>;
} => {
  const consumeBatchSpy = vi.fn(
    (_maxCount: number, _blockMs: number, _consumerName?: string) =>
      Effect.succeed(messages),
  );
  const ackSpy = vi.fn((ids: readonly string[]) => Effect.succeed(ids.length));
  const handleFailedSpy = vi.fn(() => Effect.succeed("retry" as const));

  return {
    enqueue: (_txCbor: string) => Effect.succeed("1-0"),
    rawXadd: (_txCbor, callback) => callback(null, "1-0"),
    ensureConsumerGroup: Effect.void,
    consumeBatch: consumeBatchSpy,
    ack: ackSpy,
    handleFailedMessage: handleFailedSpy,
    refreshSnapshotMetrics: Effect.succeed(snapshot),
    snapshotMetrics: Effect.succeed(snapshot),
    clear: Effect.void,
    consumeBatchSpy,
    ackSpy,
    handleFailedSpy,
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  parseTxCborInWorkerPoolFn.mockReturnValue(Effect.succeed(fakeProcessedTx));
  mempoolValidateAndInsertFn.mockImplementation((candidates: unknown[]) =>
    Effect.succeed({
      acceptedMessages: (candidates as { message: { id: string } }[]).map(
        (candidate) => candidate.message,
      ),
      insertedCount: (candidates as unknown[]).length,
      rejected: [],
    }),
  );
});

describe("txQueueProcessorAction", () => {
  it.effect(
    "passes configured drain batch size and block timeout to ingress queue",
    () =>
      Effect.gen(function* () {
        const queue = makeQueueStub([
          { id: "1-0", txCbor: "aa", deliveryCount: 1 },
        ]);

        yield* txQueueProcessorAction(500, 8, 1000, true).pipe(
          Effect.provideService(TxIngressQueue, queue),
        );

        expect(queue.consumeBatchSpy).toHaveBeenCalledWith(
          500,
          1000,
          "midgard-tx-processor",
        );
        expect(mempoolValidateAndInsertFn).toHaveBeenCalledOnce();
        expect(queue.ackSpy).toHaveBeenCalledWith(["1-0"]);
      }).pipe(Effect.provide(testLayer)),
  );

  it.effect("acks successfully persisted stream messages", () =>
    Effect.gen(function* () {
      const queue = makeQueueStub([
        { id: "1-0", txCbor: "aa", deliveryCount: 1 },
        { id: "2-0", txCbor: "bb", deliveryCount: 1 },
      ]);

      const ackDelta = yield* metricDelta(
        readAckCounter,
        txQueueProcessorAction(100, 4, 1, true).pipe(
          Effect.provideService(TxIngressQueue, queue),
        ),
        (state) => state.count,
      );

      expect(ackDelta).toBe(2n);
      expect(mempoolValidateAndInsertFn).toHaveBeenCalledOnce();
      expect(queue.ackSpy).toHaveBeenCalledWith(["1-0", "2-0"]);
      expect(queue.handleFailedSpy).not.toHaveBeenCalled();
    }).pipe(Effect.provide(testLayer)),
  );

  it.effect("keeps semantically rejected txs pending for retry and acks only accepted txs", () =>
    Effect.gen(function* () {
      const queue = makeQueueStub([
        { id: "1-0", txCbor: "aa", deliveryCount: 1 },
        { id: "2-0", txCbor: "bb", deliveryCount: 1 },
      ]);
      mempoolValidateAndInsertFn.mockReturnValue(
        Effect.succeed({
          acceptedMessages: [{ id: "1-0", txCbor: "aa", deliveryCount: 1 }],
          insertedCount: 1,
          rejected: [
            {
              message: { id: "2-0", txCbor: "bb", deliveryCount: 1 },
              reason: "validation_rejected:E_INPUT_NOT_FOUND",
            },
          ],
        }),
      );

      yield* txQueueProcessorAction(100, 4, 1, true).pipe(
        Effect.provideService(TxIngressQueue, queue),
      );

      expect(queue.ackSpy).toHaveBeenCalledWith(["1-0"]);
      expect(queue.handleFailedSpy).toHaveBeenCalledTimes(1);
      expect(queue.handleFailedSpy).toHaveBeenCalledWith(
        { id: "2-0", txCbor: "bb", deliveryCount: 1 },
        "validation_rejected:E_INPUT_NOT_FOUND",
      );
    }).pipe(Effect.provide(testLayer)),
  );

  it.effect("marks malformed txs as failed and retries", () =>
    Effect.gen(function* () {
      parseTxCborInWorkerPoolFn.mockReturnValue(
        Effect.fail(
          new SDK.CmlDeserializationError({
            message: "deserialization error",
            cause: "invalid tx cbor",
          }),
        ),
      );
      const queue = makeQueueStub([
        { id: "1-0", txCbor: "aa", deliveryCount: 1 },
      ]);

      const failBefore = yield* readFailCounter;
      const retryBefore = yield* readRetryCounter;

      yield* txQueueProcessorAction(100, 4, 1, true).pipe(
        Effect.provideService(TxIngressQueue, queue),
      );

      const failAfter = yield* readFailCounter;
      const retryAfter = yield* readRetryCounter;
      const failDelta = failAfter.count - failBefore.count;
      const retryDelta = retryAfter.count - retryBefore.count;

      expect(failDelta).toBe(1n);
      expect(retryDelta).toBe(1n);
      expect(queue.ackSpy).not.toHaveBeenCalled();
      expect(queue.handleFailedSpy).toHaveBeenCalledTimes(1);
    }).pipe(Effect.provide(testLayer)),
  );

  it.effect("dead-letters failed messages when queue adapter says so", () =>
    Effect.gen(function* () {
      parseTxCborInWorkerPoolFn.mockReturnValue(
        Effect.fail(
          new SDK.CmlDeserializationError({
            message: "deserialization error",
            cause: "invalid tx cbor",
          }),
        ),
      );
      const queue = makeQueueStub([
        { id: "1-0", txCbor: "aa", deliveryCount: 10 },
      ]);
      queue.handleFailedSpy.mockImplementation(() =>
        Effect.succeed("dead_lettered" as const),
      );

      const deadLetterDelta = yield* metricDelta(
        readDeadLetterCounter,
        txQueueProcessorAction(100, 4, 1, true).pipe(
          Effect.provideService(TxIngressQueue, queue),
        ),
        (state) => state.count,
      );

      expect(deadLetterDelta).toBe(1n);
      expect(queue.handleFailedSpy).toHaveBeenCalledTimes(1);
    }).pipe(Effect.provide(testLayer)),
  );

  it.effect("tracks stream depth peak from metrics snapshots", () =>
    Effect.gen(function* () {
      const queue = makeQueueStub([], {
        streamDepth: 3,
        pendingCount: 1,
        lagCount: 2,
      });
      const peakRef = yield* Ref.make(0n);

      yield* txQueueProcessorAction(100, 4, 1, true, peakRef).pipe(
        Effect.provideService(TxIngressQueue, queue),
      );

      const peak = yield* readPeakGauge;
      expect(peak.value).toBe(3n);
    }).pipe(Effect.provide(testLayer)),
  );
});
