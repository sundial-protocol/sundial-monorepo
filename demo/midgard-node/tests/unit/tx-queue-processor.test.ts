import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Metric, Queue, Ref } from "effect";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

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

// Mirror metric declarations to read global metric state in assertions.
const queueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "Tx queue size sampled before each drain cycle",
  bigint: true,
}).register();

const queuePeakSizeGauge = Metric.gauge("tx_queue_peak_size", {
  description:
    "High-water mark of tx queue size since fiber startup; never decreases on drain so Prometheus scrapes capture burst spikes between polling cycles",
  bigint: true,
}).register();

const mempoolAcceptedCounter = Metric.counter(
  "tx_submissions_mempool_accepted",
  {
    description:
      "A counter for tracking L2 transactions durably inserted into the mempool after CBOR deserialization and semantic breakdown",
    bigint: true,
    incremental: true,
  },
).register();

const processingFailedCounter = Metric.counter(
  "tx_submissions_processing_failed",
  {
    description:
      "A counter for tracking L2 transaction processing batch failures (CBOR deserialization or mempool insertion errors)",
    bigint: true,
    incremental: true,
  },
).register();

// Import after mocks are set up.
import { txQueueProcessorAction } from "@/fibers/tx-queue-processor.js";

const sqlHarness = createMockSqlHarness();

const fakeProcessedTx = {
  txId: Buffer.alloc(32, 0xaa),
  txCbor: Buffer.alloc(10, 0xbb),
  spent: [],
  produced: [],
};

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  breakDownTxFn.mockReturnValue(Effect.succeed(fakeProcessedTx));
  mempoolInsertFn.mockReturnValue(Effect.succeed(undefined));
});

describe("txQueueProcessorAction — tx_submissions_mempool_accepted counter", () => {
  it.effect("increments by batch count after successful processing", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");
      yield* Queue.offer(queue, "cafebabe");

      const before = yield* Metric.value(mempoolAcceptedCounter);
      yield* txQueueProcessorAction(queue, true);
      const after = yield* Metric.value(mempoolAcceptedCounter);

      expect(after.count - before.count).toBe(2n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does NOT increment on empty queue", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);

      const before = yield* Metric.value(mempoolAcceptedCounter);
      yield* txQueueProcessorAction(queue, true);
      const after = yield* Metric.value(mempoolAcceptedCounter);

      expect(after.count - before.count).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does NOT increment when withMonitoring is false", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const before = yield* Metric.value(mempoolAcceptedCounter);
      yield* txQueueProcessorAction(queue, false);
      const after = yield* Metric.value(mempoolAcceptedCounter);

      expect(after.count - before.count).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — tx_submissions_processing_failed counter", () => {
  it.effect("increments when breakDownTx fails", () =>
    Effect.gen(function* () {
      breakDownTxFn.mockReturnValue(
        Effect.die(new Error("deserialization error")),
      );
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const before = yield* Metric.value(processingFailedCounter);
      yield* txQueueProcessorAction(queue, true).pipe(
        Effect.catchAllCause(() => Effect.void),
      );
      const after = yield* Metric.value(processingFailedCounter);

      expect(after.count - before.count).toBe(1n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("increments when MempoolDB.insertMultiple fails", () =>
    Effect.gen(function* () {
      mempoolInsertFn.mockReturnValue(
        Effect.die(new Error("db insertion error")),
      );
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const before = yield* Metric.value(processingFailedCounter);
      yield* txQueueProcessorAction(queue, true).pipe(
        Effect.catchAllCause(() => Effect.void),
      );
      const after = yield* Metric.value(processingFailedCounter);

      expect(after.count - before.count).toBe(1n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does NOT increment when withMonitoring is false", () =>
    Effect.gen(function* () {
      breakDownTxFn.mockReturnValue(
        Effect.die(new Error("deserialization error")),
      );
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const before = yield* Metric.value(processingFailedCounter);
      yield* txQueueProcessorAction(queue, false).pipe(
        Effect.catchAllCause(() => Effect.void),
      );
      const after = yield* Metric.value(processingFailedCounter);

      expect(after.count - before.count).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does NOT increment on successful processing", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const before = yield* Metric.value(processingFailedCounter);
      yield* txQueueProcessorAction(queue, true);
      const after = yield* Metric.value(processingFailedCounter);

      expect(after.count - before.count).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );
});

describe("txQueueProcessorAction — tx_queue_peak_size gauge", () => {
  it.effect("sets peak to current queue size on first call", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");
      yield* Queue.offer(queue, "cafebabe");

      const peakRef = yield* Ref.make(0n);
      yield* txQueueProcessorAction(queue, true, peakRef);

      const peak = yield* Metric.value(queuePeakSizeGauge);
      expect(peak.value).toBe(2n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect(
    "peak is monotonically non-decreasing across calls (does not drop on drain)",
    () =>
      Effect.gen(function* () {
        const queue = yield* Queue.bounded<string>(10);
        yield* Queue.offer(queue, "deadbeef");
        yield* Queue.offer(queue, "cafebabe");
        yield* Queue.offer(queue, "f00dface");

        const peakRef = yield* Ref.make(0n);

        // First cycle: queue has 3 items; peak should become 3.
        yield* txQueueProcessorAction(queue, true, peakRef);
        const peakAfterFirstDrain = yield* Metric.value(queuePeakSizeGauge);
        expect(peakAfterFirstDrain.value).toBe(3n);

        // Second cycle: queue is empty; peak must NOT decrease to 0.
        yield* txQueueProcessorAction(queue, true, peakRef);
        const peakAfterSecondDrain = yield* Metric.value(queuePeakSizeGauge);
        expect(peakAfterSecondDrain.value).toBe(3n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("peak grows when a later cycle has a larger backlog", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const peakRef = yield* Ref.make(0n);

      yield* txQueueProcessorAction(queue, true, peakRef);
      const peakAfterSmallBatch = yield* Metric.value(queuePeakSizeGauge);
      expect(peakAfterSmallBatch.value).toBe(1n);

      // Enqueue a larger burst before the next cycle.
      yield* Queue.offer(queue, "aabbccdd");
      yield* Queue.offer(queue, "11223344");
      yield* Queue.offer(queue, "55667788");

      yield* txQueueProcessorAction(queue, true, peakRef);
      const peakAfterLargeBatch = yield* Metric.value(queuePeakSizeGauge);
      expect(peakAfterLargeBatch.value).toBe(3n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect("does NOT update peak when withMonitoring is false", () =>
    Effect.gen(function* () {
      const queue = yield* Queue.bounded<string>(10);
      yield* Queue.offer(queue, "deadbeef");

      const peakRef = yield* Ref.make(0n);
      const sizeBefore = yield* Metric.value(queuePeakSizeGauge);
      yield* txQueueProcessorAction(queue, false, peakRef);
      const sizeAfter = yield* Metric.value(queuePeakSizeGauge);

      expect(sizeAfter.value - sizeBefore.value).toBe(0n);
    }).pipe(Effect.provide(sqlHarness.layer)),
  );

  it.effect(
    "peak gauge retains high-water mark when a subsequent cycle sees an empty queue",
    () =>
      Effect.gen(function* () {
        const queue = yield* Queue.bounded<string>(10);
        yield* Queue.offer(queue, "deadbeef");
        yield* Queue.offer(queue, "cafebabe");

        const peakRef = yield* Ref.make(0n);

        // First cycle drains 2 items; both gauges record 2.
        yield* txQueueProcessorAction(queue, true, peakRef);
        const sizeAfterFirst = yield* Metric.value(queueSizeGauge);
        const peakAfterFirst = yield* Metric.value(queuePeakSizeGauge);
        expect(sizeAfterFirst.value).toBe(2n);
        expect(peakAfterFirst.value).toBe(2n);

        // Second cycle: queue is empty; tx_queue_size drops to 0 but peak holds at 2.
        yield* txQueueProcessorAction(queue, true, peakRef);
        const sizeAfterSecond = yield* Metric.value(queueSizeGauge);
        const peakAfterSecond = yield* Metric.value(queuePeakSizeGauge);
        expect(sizeAfterSecond.value).toBe(0n);
        expect(peakAfterSecond.value).toBe(2n);
      }).pipe(Effect.provide(sqlHarness.layer)),
  );
});
