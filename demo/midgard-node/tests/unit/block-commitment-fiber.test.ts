import { describe, expect, vi, afterEach, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Fiber, Layer, Metric, MetricBoundaries } from "effect";
import { Globals } from "@/services/globals.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { NodeConfig } from "@/services/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { Lucid } from "@/services/lucid.js";
import {
  blockCommitmentMetrics,
  buildAndSubmitCommitmentBlockAction,
  unsafeResetCommitmentWorkerForTesting,
} from "@/fibers/block-commitment.js";
import { CommitmentWorkerMessageType } from "@/workers/utils/block-commitment.js";
import { metricDelta } from "./harness/metric-snapshot.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

// Hoisted so the mock factory can be swapped per test.
const makeWorkerInstance = vi.hoisted(() => vi.fn());
const ensureBlocksSeededFn = vi.hoisted(() => vi.fn());

vi.mock("worker_threads", () => ({
  parentPort: null,
  workerData: {},
  Worker: class MockWorker {
    constructor(_url: URL, _opts: unknown) {
      return makeWorkerInstance();
    }
  },
}));

vi.mock("@/fibers/seed-blocks-db-from-chain.js", () => ({
  get ensureBlocksDBSeededFromChain() {
    return ensureBlocksSeededFn();
  },
  blocksDbSeedingMetrics: {
    seedBlocksDbAttemptsCounter: Metric.counter(
      "test_blocks_db_seed_attempts_total",
      { bigint: true, incremental: true },
    ).register(),
    seedBlocksDbSuccessCounter: Metric.counter(
      "test_blocks_db_seed_success_total",
      { bigint: true, incremental: true },
    ).register(),
    seedBlocksDbFailuresCounter: Metric.counter(
      "test_blocks_db_seed_failures_total",
      { bigint: true, incremental: true },
    ).register(),
    seedBlocksDbTraversalHopsGauge: Metric.gauge(
      "test_blocks_db_seed_traversal_hops_last",
    ).register(),
    seedBlocksDbDurationHistogram: Metric.histogram(
      "test_blocks_db_seed_duration_seconds",
      MetricBoundaries.exponential({ start: 0.1, factor: 2, count: 6 }),
    ).register(),
  },
}));

const sqlHarness = createMockSqlHarness();

const fakeLucidLayer = Layer.succeed(
  Lucid,
  Lucid.of({
    _tag: "Lucid",
    api: {} as never,
    mainApi: {} as never,
    blockCommitmentApi: {} as never,
    mergeApi: {} as never,
    reinitializeMergeApi: Effect.void,
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsBlockCommitmentWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    checkReady: Effect.void,
  }),
);

const baseLayer = Layer.mergeAll(
  Globals.Default,
  makeTestNodeConfigLayer(),
  sqlHarness.layer,
  fakeLucidLayer,
  Layer.succeed(AlwaysSucceedsContract, null as never),
);
const TEST_WORKER_TIMEOUT_MS = 10;

const successfulCommitmentOutput = {
  type: "SuccessfulCommitmentOutput",
  stats: {
    deposits_count: 0,
    tx_requests_count: 0,
    tx_orders_count: 0,
    withdrawals_count: 0,
    total_events_size: 0,
  },
} as const;

const readFailureCounter = Metric.value(
  blockCommitmentMetrics.commitBlockCommitmentFailuresCounter,
);

const readDurationHistogram = Metric.value(
  blockCommitmentMetrics.commitBlockDurationHistogram,
);
const readBackpressureSkipCounter = Metric.value(
  blockCommitmentMetrics.commitBlockBackpressureSkipsCounter,
);
const readBatchWaitSkipCounter = Metric.value(
  blockCommitmentMetrics.commitmentBatchWaitSkipsCounter,
);
const readCommitmentWindowAgeGauge = Metric.value(
  blockCommitmentMetrics.commitmentWindowAgeSecondsGauge,
);
const readCommitmentWindowTotalGauge = Metric.value(
  blockCommitmentMetrics.commitmentWindowTxRequestsTotalGauge,
);

function runAction(layer = baseLayer) {
  return buildAndSubmitCommitmentBlockAction().pipe(
    Effect.catchTag("WorkerError", () => Effect.void),
    Effect.provide(layer),
  );
}

function makeShortTimeoutLayer() {
  return Effect.gen(function* () {
    const config = yield* NodeConfig.pipe(
      Effect.provide(makeTestNodeConfigLayer()),
    );
    return Layer.mergeAll(
      Globals.Default,
      sqlHarness.layer,
      fakeLucidLayer,
      Layer.succeed(AlwaysSucceedsContract, null as never),
      Layer.succeed(
        NodeConfig,
        NodeConfig.of({
          ...config,
          COMMITMENT_WORKER_TIMEOUT_MS: TEST_WORKER_TIMEOUT_MS,
        }),
      ),
    );
  });
}

function makeBackpressureThresholdLayer(maxBacklog: number) {
  return Effect.gen(function* () {
    const config = yield* NodeConfig.pipe(
      Effect.provide(makeTestNodeConfigLayer()),
    );
    return Layer.mergeAll(
      Globals.Default,
      sqlHarness.layer,
      fakeLucidLayer,
      Layer.succeed(AlwaysSucceedsContract, null as never),
      Layer.succeed(
        NodeConfig,
        NodeConfig.of({
          ...config,
          COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG: maxBacklog,
        }),
      ),
    );
  });
}

function runUntilCommitmentWorkerTimeout() {
  return Effect.gen(function* () {
    yield* Effect.sync(() => vi.useFakeTimers());
    const shortTimeoutLayer = yield* makeShortTimeoutLayer();
    const actionFiber = yield* Effect.fork(runAction(shortTimeoutLayer));
    yield* Effect.promise(() =>
      vi.advanceTimersByTimeAsync(TEST_WORKER_TIMEOUT_MS + 1),
    );
    yield* Fiber.join(actionFiber);
  });
}

// Helper: build a fake worker that emits one event asynchronously on postMessage.
function makeEventWorker(event: string, ...args: unknown[]) {
  const listeners = new Map<string, Set<(...a: unknown[]) => void>>();
  const terminate = vi.fn();
  const on = vi.fn((ev: string, cb: (...a: unknown[]) => void) => {
    const existing = listeners.get(ev) ?? new Set<(...a: unknown[]) => void>();
    existing.add(cb);
    listeners.set(ev, existing);
  });
  const off = vi.fn((ev: string, cb: (...a: unknown[]) => void) => {
    const existing = listeners.get(ev);
    if (existing) {
      existing.delete(cb);
    }
  });
  const postMessage = vi.fn(() => {
    queueMicrotask(() => {
      const matchingListeners = listeners.get(event);
      if (matchingListeners) {
        for (const listener of matchingListeners) {
          listener(...args);
        }
      }
    });
  });
  return { on, off, postMessage, terminate };
}

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  unsafeResetCommitmentWorkerForTesting();
  ensureBlocksSeededFn.mockReturnValue(Effect.succeed("already-seeded"));
});

afterEach(() => {
  vi.useRealTimers();
});

describe("buildAndSubmitCommitmentBlockAction — failure counter", () => {
  it.effect.each([
    {
      name: "increments on FailureOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "FailureOutput",
              error: "sdk error",
            },
          }),
        ),
      expectedDelta: 1n,
    },
    {
      name: "increments when worker emits an error",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("error", new Error("thread crash")),
        ),
      expectedDelta: 1n,
    },
    {
      name: "increments when worker exits with non-zero code",
      setup: () =>
        makeWorkerInstance.mockReturnValue(makeEventWorker("exit", 1)),
      expectedDelta: 1n,
    },
    {
      name: "does not increment on SuccessfulCommitmentOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: successfulCommitmentOutput,
          }),
        ),
      expectedDelta: 0n,
    },
    {
      name: "does not increment on SeededOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: { type: "SeededOutput" },
          }),
        ),
      expectedDelta: 0n,
    },
    {
      name: "does not increment on NoopCommitmentOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "NoopCommitmentOutput",
              reason: "no_events_in_window",
            },
          }),
        ),
      expectedDelta: 0n,
    },
  ])("$name", ({ setup, expectedDelta }) =>
    Effect.gen(function* () {
      setup();
      const delta = yield* metricDelta(
        readFailureCounter,
        runAction(),
        (state) => state.count,
      );
      expect(delta).toBe(expectedDelta);
    }),
  );

  it.effect("increments when the commitment worker times out", () =>
    Effect.gen(function* () {
      makeWorkerInstance.mockReturnValue({
        on: vi.fn(),
        off: vi.fn(),
        postMessage: vi.fn(),
        terminate: vi.fn(),
      });

      const beforeCount = (yield* readFailureCounter).count;
      yield* runUntilCommitmentWorkerTimeout();
      const afterCount = (yield* readFailureCounter).count;
      const delta = afterCount - beforeCount;
      expect(delta).toBe(1n);
    }),
  );

  it.effect("terminates the worker when the commitment worker times out", () =>
    Effect.gen(function* () {
      const neverRespondingWorker = {
        on: vi.fn(),
        off: vi.fn(),
        postMessage: vi.fn(),
        terminate: vi.fn(),
      };
      makeWorkerInstance.mockReturnValue(neverRespondingWorker);

      yield* runUntilCommitmentWorkerTimeout();

      expect(neverRespondingWorker.terminate).toHaveBeenCalled();
    }),
  );

  it.effect(
    "does not spawn worker while cold-start seeding is in progress",
    () =>
      Effect.gen(function* () {
        ensureBlocksSeededFn.mockReturnValue(Effect.succeed("retry-later"));
        yield* runAction();
        expect(makeWorkerInstance).not.toHaveBeenCalled();
      }),
  );

  it.effect("does not spawn worker immediately after successful seeding", () =>
    Effect.gen(function* () {
      ensureBlocksSeededFn.mockReturnValue(Effect.succeed("seeded"));
      yield* runAction();
      expect(makeWorkerInstance).not.toHaveBeenCalled();
    }),
  );

  it.effect(
    "skips commitment worker when pending backlog exceeds threshold",
    () =>
      Effect.gen(function* () {
        sqlHarness.setRows([{ count: "1" }]);
        const delta = yield* metricDelta(
          readBackpressureSkipCounter,
          runAction(),
          (state) => state.count,
        );
        expect(delta).toBe(1n);
        expect(makeWorkerInstance).not.toHaveBeenCalled();
      }),
  );

  it.effect(
    "runs commitment worker when pending backlog equals threshold",
    () =>
      Effect.gen(function* () {
        sqlHarness.setRows([{ count: "1" }]);
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: successfulCommitmentOutput,
          }),
        );
        const thresholdLayer = yield* makeBackpressureThresholdLayer(1);
        yield* runAction(thresholdLayer);
        expect(makeWorkerInstance).toHaveBeenCalledTimes(1);
      }),
  );

  it.effect(
    "increments batching skip counter when worker delays for a larger tx batch",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "NoopCommitmentOutput",
              reason: "waiting_for_min_tx_batch",
              commitmentWindow: {
                txRequestsTotalInWindow: 250,
                txRequestsSelected: 0,
                txRequestsDeferredInWindow: 0,
                windowAgeMs: 4_000,
              },
            },
          }),
        );
        const delta = yield* metricDelta(
          readBatchWaitSkipCounter,
          runAction(),
          (state) => state.count,
        );
        expect(delta).toBe(1n);
      }),
  );

  it.effect(
    "records waiting batch window gauges on batch-delay no-op output",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "NoopCommitmentOutput",
              reason: "waiting_for_min_tx_batch",
              commitmentWindow: {
                txRequestsTotalInWindow: 250,
                txRequestsSelected: 0,
                txRequestsDeferredInWindow: 0,
                windowAgeMs: 4_000,
              },
            },
          }),
        );
        yield* runAction();
        expect((yield* readCommitmentWindowTotalGauge).value).toBe(250n);
        expect((yield* readCommitmentWindowAgeGauge).value).toBe(4);
      }),
  );
});

describe("buildAndSubmitCommitmentBlockAction — duration histogram", () => {
  it.effect.each([
    {
      name: "records observation on SuccessfulCommitmentOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: successfulCommitmentOutput,
          }),
        ),
    },
    {
      name: "records observation on SeededOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: { type: "SeededOutput" },
          }),
        ),
    },
    {
      name: "records observation on FailureOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "FailureOutput",
              error: "sdk error",
            },
          }),
        ),
    },
    {
      name: "records observation when worker emits an error",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("error", new Error("thread crash")),
        ),
    },
    {
      name: "records observation on NoopCommitmentOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: CommitmentWorkerMessageType.RunCommitmentResult,
            output: {
              type: "NoopCommitmentOutput",
              reason: "no_events_in_window",
            },
          }),
        ),
    },
  ])("$name", ({ setup }) =>
    Effect.gen(function* () {
      setup();
      const delta = yield* metricDelta(
        readDurationHistogram,
        runAction(),
        (state) => state.count,
      );
      expect(delta).toBe(1);
    }),
  );
});
