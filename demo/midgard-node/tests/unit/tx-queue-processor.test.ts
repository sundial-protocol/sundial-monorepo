import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Metric, Queue, Ref } from "effect";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { metricDelta } from "./harness/metric-snapshot.js";

// Hoisted so mock factories can reference these fns before imports resolve.
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

// Import after mocks are set up.
import {
  txQueueProcessorAction,
  txQueueProcessorMetrics,
} from "@/fibers/tx-queue-processor.js";

const sqlHarness = createMockSqlHarness();

const fakeProcessedTx = {
  txId: Buffer.alloc(32, 0xaa),
  txCbor: Buffer.alloc(10, 0xbb),
  spent: [],
  produced: [],
};
const TEST_DRAIN_BATCH_SIZE = 10;

const readMempoolAcceptedCounter = Metric.value(
  txQueueProcessorMetrics.txMempoolAcceptedCounter,
);
const readProcessingFailedCounter = Metric.value(
  txQueueProcessorMetrics.txProcessingFailedCounter,
);
const readQueueSizeGauge = Metric.value(
  txQueueProcessorMetrics.txQueueSizeGauge,
);
const readQueuePeakSizeGauge = Metric.value(
  txQueueProcessorMetrics.txQueuePeakSizeGauge,
);

const enqueue = (queue: Queue.Enqueue<string>, txs: ReadonlyArray<string>) =>
  Effect.forEach(txs, (tx) => Queue.offer(queue, tx), { discard: true });

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  breakDownTxFn.mockReturnValue(Effect.succeed(fakeProcessedTx));
  mempoolInsertFn.mockImplementation((processedTxs: unknown[]) =>
    Effect.succeed(processedTxs.length),
  );
});

describe("txQueueProcessorAction — tx_submissions_mempool_accepted counter", () => {
  it.effect.each([
    {
      name: "increments by batch count on successful processing",
      txs: ["deadbeef", "cafebabe"],
      withMonitoring: true,
      expectedDelta: 2n,
    },
    {
      name: "does not increment on empty queue",
      txs: [],
      withMonitoring: true,
      expectedDelta: 0n,
    },
    {
      name: "does not increment when monitoring is disabled",
      txs: ["deadbeef"],
      withMonitoring: false,
      expectedDelta: 0n,
    },
  ])("$name", ({ txs, withMonitoring, expectedDelta }) =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, txs);
      const delta = yield* metricDelta(
        readMempoolAcceptedCounter,
        txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, withMonitoring),
        (state) => state.count,
      );
      expect(delta).toBe(expectedDelta);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — tx_submissions_processing_failed counter", () => {
  it.effect.each([
    {
      // breakDownTx typed failure is caught by Effect.partition; action
      // succeeds, counter incremented once per malformed tx.
      name: "increments when breakDownTx fails",
      configure: () =>
        breakDownTxFn.mockReturnValue(
          Effect.fail(new Error("deserialization error")),
        ),
      withMonitoring: true,
      catchFailure: false,
      expectedDelta: 1n,
    },
    {
      name: "increments when MempoolDB.insertMultiple fails",
      configure: () =>
        mempoolInsertFn.mockReturnValue(
          Effect.die(new Error("db insertion error")),
        ),
      withMonitoring: true,
      catchFailure: true,
      expectedDelta: 1n,
    },
    {
      name: "does not increment when monitoring is disabled",
      configure: () =>
        breakDownTxFn.mockReturnValue(
          Effect.fail(new Error("deserialization error")),
        ),
      withMonitoring: false,
      catchFailure: false,
      expectedDelta: 0n,
    },
    {
      name: "does not increment on successful processing",
      configure: () => void 0,
      withMonitoring: true,
      catchFailure: false,
      expectedDelta: 0n,
    },
  ])("$name", ({ configure, withMonitoring, catchFailure, expectedDelta }) =>
    Effect.gen(function* () {
      configure();
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["deadbeef"]);

      const action = catchFailure
        ? txQueueProcessorAction(
            queue,
            TEST_DRAIN_BATCH_SIZE,
            4,
            withMonitoring,
          ).pipe(Effect.catchAllCause(() => Effect.void))
        : txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, withMonitoring);

      const delta = yield* metricDelta(
        readProcessingFailedCounter,
        action,
        (state) => state.count,
      );

      expect(delta).toBe(expectedDelta);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — tx_queue_peak_size gauge", () => {
  it.effect("sets peak to current queue size on first call", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["deadbeef", "cafebabe"]);

      const peakRef = yield* Ref.make(0n);
      yield* txQueueProcessorAction(
        queue,
        TEST_DRAIN_BATCH_SIZE,
        4,
        true,
        peakRef,
      );

      const peak = yield* readQueuePeakSizeGauge;
      expect(peak.value).toBe(2n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect(
    "peak is monotonically non-decreasing across calls (does not drop on drain)",
    () =>
      Effect.gen(function* () {
        const queue = yield* Queue.bounded<string>(10);
        yield* enqueue(queue, ["deadbeef", "cafebabe", "f00dface"]);

        const peakRef = yield* Ref.make(0n);

        yield* txQueueProcessorAction(
          queue,
          TEST_DRAIN_BATCH_SIZE,
          4,
          true,
          peakRef,
        );
        const peakAfterFirstDrain = yield* readQueuePeakSizeGauge;
        expect(peakAfterFirstDrain.value).toBe(3n);

        yield* txQueueProcessorAction(
          queue,
          TEST_DRAIN_BATCH_SIZE,
          4,
          true,
          peakRef,
        );
        const peakAfterSecondDrain = yield* readQueuePeakSizeGauge;
        expect(peakAfterSecondDrain.value).toBe(3n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("peak grows when a later cycle has a larger backlog", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["deadbeef"]);

      const peakRef = yield* Ref.make(0n);

      yield* txQueueProcessorAction(
        queue,
        TEST_DRAIN_BATCH_SIZE,
        4,
        true,
        peakRef,
      );
      const peakAfterSmallBatch = yield* readQueuePeakSizeGauge;
      expect(peakAfterSmallBatch.value).toBe(1n);

      yield* enqueue(queue, ["aabbccdd", "11223344", "55667788"]);

      yield* txQueueProcessorAction(
        queue,
        TEST_DRAIN_BATCH_SIZE,
        4,
        true,
        peakRef,
      );
      const peakAfterLargeBatch = yield* readQueuePeakSizeGauge;
      expect(peakAfterLargeBatch.value).toBe(3n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does not update peak when monitoring is disabled", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["deadbeef"]);

      const peakRef = yield* Ref.make(0n);
      const delta = yield* metricDelta(
        readQueuePeakSizeGauge,
        txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, false, peakRef),
        (state) => state.value,
      );

      expect(delta).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect(
    "peak gauge retains high-water mark when a subsequent cycle sees an empty queue",
    () =>
      Effect.gen(function* () {
        const queue = yield* Queue.bounded<string>(10);
        yield* enqueue(queue, ["deadbeef", "cafebabe"]);

        const peakRef = yield* Ref.make(0n);

        yield* txQueueProcessorAction(
          queue,
          TEST_DRAIN_BATCH_SIZE,
          4,
          true,
          peakRef,
        );
        const sizeAfterFirst = yield* readQueueSizeGauge;
        const peakAfterFirst = yield* readQueuePeakSizeGauge;
        expect(sizeAfterFirst.value).toBe(2n);
        expect(peakAfterFirst.value).toBe(2n);

        yield* txQueueProcessorAction(
          queue,
          TEST_DRAIN_BATCH_SIZE,
          4,
          true,
          peakRef,
        );
        const sizeAfterSecond = yield* readQueueSizeGauge;
        const peakAfterSecond = yield* readQueuePeakSizeGauge;
        expect(sizeAfterSecond.value).toBe(0n);
        expect(peakAfterSecond.value).toBe(2n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — per-tx isolation (H-07)", () => {
  it.effect(
    "inserts valid txs and skips only the malformed one in the same batch",
    () =>
      Effect.gen(function* () {
        breakDownTxFn
          .mockReturnValueOnce(Effect.fail(new Error("bad cbor")))
          .mockReturnValueOnce(Effect.succeed(fakeProcessedTx))
          .mockReturnValueOnce(Effect.succeed(fakeProcessedTx));

        const queue = yield* Queue.bounded<string>(10);
        yield* enqueue(queue, ["badtx", "goodtx1", "goodtx2"]);

        const acceptedDelta = yield* metricDelta(
          readMempoolAcceptedCounter,
          txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, true),
          (state) => state.count,
        );

        expect(mempoolInsertFn).toHaveBeenCalledOnce();
        expect(mempoolInsertFn).toHaveBeenCalledWith([
          fakeProcessedTx,
          fakeProcessedTx,
        ]);
        expect(acceptedDelta).toBe(2n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect(
    "increments processing-failed counter only for the malformed tx, not the valid ones",
    () =>
      Effect.gen(function* () {
        breakDownTxFn
          .mockReturnValueOnce(Effect.fail(new Error("bad cbor")))
          .mockReturnValueOnce(Effect.succeed(fakeProcessedTx));

        const queue = yield* Queue.bounded<string>(10);
        yield* enqueue(queue, ["badtx", "goodtx"]);

        const failedDelta = yield* metricDelta(
          readProcessingFailedCounter,
          txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, true),
          (state) => state.count,
        );

        expect(failedDelta).toBe(1n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does not call insertMultiple when all txs in a batch are malformed", () =>
    Effect.gen(function* () {
      breakDownTxFn.mockReturnValue(Effect.fail(new Error("bad cbor")));

      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["badtx1", "badtx2"]);

      yield* txQueueProcessorAction(queue, TEST_DRAIN_BATCH_SIZE, 4, true);

      expect(mempoolInsertFn).not.toHaveBeenCalled();
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — bounded per-tick draining", () => {
  it.effect("drains only up to TX_QUEUE_DRAIN_BATCH_SIZE per cycle", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* enqueue(queue, ["a1", "b2", "c3"]);

      const firstDelta = yield* metricDelta(
        readMempoolAcceptedCounter,
        txQueueProcessorAction(queue, 2, 4, true),
        (state) => state.count,
      );
      expect(firstDelta).toBe(2n);
      const queueSizeAfterFirst = yield* queue.size;
      expect(queueSizeAfterFirst).toBe(1);

      const secondDelta = yield* metricDelta(
        readMempoolAcceptedCounter,
        txQueueProcessorAction(queue, 2, 4, true),
        (state) => state.count,
      );
      expect(secondDelta).toBe(1n);
      const queueSizeAfterSecond = yield* queue.size;
      expect(queueSizeAfterSecond).toBe(0);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});
