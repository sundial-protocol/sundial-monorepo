import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Metric, Ref } from "effect";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { metricDelta } from "./harness/metric-snapshot.js";

const breakDownTxFn = vi.hoisted(() => vi.fn());
const mempoolInsertFn = vi.hoisted(() => vi.fn());

vi.mock("@/utils.js", () => ({
  breakDownTx: breakDownTxFn,
}));

vi.mock("@/database/index.js", () => ({
  MempoolDB: {
    insertMultiple: mempoolInsertFn,
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
  ackSpy: ReturnType<typeof vi.fn>;
  handleFailedSpy: ReturnType<typeof vi.fn>;
} => {
  const ackSpy = vi.fn((ids: readonly string[]) => Effect.succeed(ids.length));
  const handleFailedSpy = vi.fn(() => Effect.succeed("retry" as const));

  return {
    enqueue: (_txCbor: string) => Effect.succeed("1-0"),
    ensureConsumerGroup: Effect.void,
    consumeBatch: (_maxCount: number, _blockMs: number) =>
      Effect.succeed(messages),
    ack: ackSpy,
    handleFailedMessage: handleFailedSpy,
    snapshotMetrics: Effect.succeed(snapshot),
    ackSpy,
    handleFailedSpy,
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  breakDownTxFn.mockReturnValue(Effect.succeed(fakeProcessedTx));
  mempoolInsertFn.mockImplementation((processedTxs: unknown[]) =>
    Effect.succeed(processedTxs.length),
  );
});

describe("txQueueProcessorAction", () => {
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
      expect(mempoolInsertFn).toHaveBeenCalledOnce();
      expect(queue.ackSpy).toHaveBeenCalledWith(["1-0", "2-0"]);
      expect(queue.handleFailedSpy).not.toHaveBeenCalled();
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("marks malformed txs as failed and retries", () =>
    Effect.gen(function* () {
      breakDownTxFn.mockReturnValue(
        Effect.fail(new Error("deserialization error")),
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
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("dead-letters failed messages when queue adapter says so", () =>
    Effect.gen(function* () {
      breakDownTxFn.mockReturnValue(
        Effect.fail(new Error("deserialization error")),
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
    }).pipe(Effect.provide(sqlHarness.layer)),
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
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});
