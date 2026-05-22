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
} from "@/fibers/block-commitment.js";
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
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsBlockCommitmentWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
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

// Helper: build a fake worker that emits one event asynchronously after listener registration.
function makeEventWorker(event: string, ...args: unknown[]) {
  const terminate = vi.fn();
  const on = vi.fn((ev: string, cb: (...a: unknown[]) => void) => {
    if (ev === event) {
      queueMicrotask(() => cb(...args));
    }
  });
  return { on, terminate };
}

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
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
            type: "FailureOutput",
            error: "sdk error",
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
          makeEventWorker("message", successfulCommitmentOutput),
        ),
      expectedDelta: 0n,
    },
    {
      name: "does not increment on SeededOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", { type: "SeededOutput" }),
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
});

describe("buildAndSubmitCommitmentBlockAction — duration histogram", () => {
  it.effect.each([
    {
      name: "records observation on SuccessfulCommitmentOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", successfulCommitmentOutput),
        ),
    },
    {
      name: "records observation on SeededOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", { type: "SeededOutput" }),
        ),
    },
    {
      name: "records observation on FailureOutput",
      setup: () =>
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: "FailureOutput",
            error: "sdk error",
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
