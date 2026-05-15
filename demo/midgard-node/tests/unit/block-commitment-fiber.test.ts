import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Metric } from "effect";
import { Globals } from "@/services/globals.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { NodeConfig } from "@/services/config.js";
import { commitBlockDurationHistogramBoundaries } from "@/fibers/block-commitment.js";

// Hoisted so the mock factory can be swapped per test.
const makeWorkerInstance = vi.hoisted(() => vi.fn());

vi.mock("worker_threads", () => ({
  parentPort: null,
  workerData: {},
  Worker: class MockWorker {
    constructor(_url: URL, _opts: unknown) {
      return makeWorkerInstance();
    }
  },
}));

// Recreate the same counter metric so we can read its global state.
const failuresCounter = Metric.counter("commit_block_commitment_failures", {
  description:
    "A counter for tracking the number of block commitment worker failures (timeouts, crashes, and SDK/CML errors)",
  bigint: true,
  incremental: true,
}).register();

// Recreate the same histogram metric so we can read its global state.
const durationHistogram = Metric.histogram(
  "commit_block_duration_seconds",
  commitBlockDurationHistogramBoundaries,
  "Histogram of block commitment worker duration in seconds (success or failure)",
).register();

// Import after mocks are set up.
import { buildAndSubmitCommitmentBlockAction } from "@/fibers/block-commitment.js";

const baseLayer = Layer.mergeAll(Globals.Default, makeTestNodeConfigLayer());

function runAction(layer = baseLayer) {
  return buildAndSubmitCommitmentBlockAction().pipe(
    Effect.catchTag("WorkerError", () => Effect.void),
    Effect.provide(layer),
  );
}

async function readFailureCount(layer = baseLayer): Promise<bigint> {
  const state = await Effect.runPromise(
    Metric.value(failuresCounter).pipe(Effect.provide(layer)),
  );
  return state.count;
}

// Helper: build a fake worker that calls one event callback synchronously.
function makeEventWorker(event: string, ...args: unknown[]) {
  const terminate = vi.fn();
  const on = vi.fn((ev: string, cb: (...a: unknown[]) => void) => {
    if (ev === event) cb(...args);
  });
  return { on, terminate };
}

beforeEach(() => {
  vi.clearAllMocks();
});

describe("buildAndSubmitCommitmentBlockAction — failure counter", () => {
  it.effect("increments failure counter when worker sends FailureOutput", () =>
    Effect.gen(function* () {
      makeWorkerInstance.mockReturnValue(
        makeEventWorker("message", {
          type: "FailureOutput",
          error: "sdk error",
        }),
      );

      const before = yield* Metric.value(failuresCounter);
      yield* runAction();
      const after = yield* Metric.value(failuresCounter);

      expect(after.count - before.count).toBe(1n);
    }),
  );

  it.effect("increments failure counter when worker emits an error event", () =>
    Effect.gen(function* () {
      makeWorkerInstance.mockReturnValue(
        makeEventWorker("error", new Error("thread crash")),
      );

      const before = yield* Metric.value(failuresCounter);
      yield* runAction();
      const after = yield* Metric.value(failuresCounter);

      expect(after.count - before.count).toBe(1n);
    }),
  );

  it.effect(
    "increments failure counter when worker exits with non-zero code",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(makeEventWorker("exit", 1));

        const before = yield* Metric.value(failuresCounter);
        yield* runAction();
        const after = yield* Metric.value(failuresCounter);

        expect(after.count - before.count).toBe(1n);
      }),
  );

  it.effect(
    "increments failure counter when the commitment worker times out",
    () =>
      Effect.gen(function* () {
        // Worker that never fires any event, so the timeout triggers.
        makeWorkerInstance.mockReturnValue({
          on: vi.fn(),
          terminate: vi.fn(),
        });

        const shortTimeoutLayer = Layer.mergeAll(
          Globals.Default,
          Layer.succeed(
            NodeConfig,
            NodeConfig.of({
              ...(yield* NodeConfig.pipe(
                Effect.provide(makeTestNodeConfigLayer()),
              )),
              COMMITMENT_WORKER_TIMEOUT_MS: 10,
            }),
          ),
        );

        const before = yield* Metric.value(failuresCounter);
        yield* runAction(shortTimeoutLayer);
        const after = yield* Metric.value(failuresCounter);

        expect(after.count - before.count).toBe(1n);
      }),
  );

  it.effect("terminates the worker when the commitment worker times out", () =>
    Effect.gen(function* () {
      const neverRespondingWorker = {
        on: vi.fn(),
        terminate: vi.fn(),
      };
      makeWorkerInstance.mockReturnValue(neverRespondingWorker);

      const shortTimeoutLayer = Layer.mergeAll(
        Globals.Default,
        Layer.succeed(
          NodeConfig,
          NodeConfig.of({
            ...(yield* NodeConfig.pipe(
              Effect.provide(makeTestNodeConfigLayer()),
            )),
            COMMITMENT_WORKER_TIMEOUT_MS: 10,
          }),
        ),
      );

      yield* runAction(shortTimeoutLayer);

      expect(neverRespondingWorker.terminate).toHaveBeenCalled();
    }),
  );

  it.effect(
    "does NOT increment failure counter on a successful SuccessfulCommitmentOutput",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: "SuccessfulCommitmentOutput",
            stats: {
              deposits_count: 0,
              tx_requests_count: 0,
              tx_orders_count: 0,
              withdrawals_count: 0,
              total_events_size: 0,
            },
          }),
        );

        const before = yield* Metric.value(failuresCounter);
        yield* runAction();
        const after = yield* Metric.value(failuresCounter);

        expect(after.count - before.count).toBe(0n);
      }),
  );

  it.effect("does NOT increment failure counter on a SeededOutput", () =>
    Effect.gen(function* () {
      makeWorkerInstance.mockReturnValue(
        makeEventWorker("message", { type: "SeededOutput" }),
      );

      const before = yield* Metric.value(failuresCounter);
      yield* runAction();
      const after = yield* Metric.value(failuresCounter);

      expect(after.count - before.count).toBe(0n);
    }),
  );
});

describe("buildAndSubmitCommitmentBlockAction — duration histogram", () => {
  it.effect(
    "records a histogram observation on successful SuccessfulCommitmentOutput",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: "SuccessfulCommitmentOutput",
            stats: {
              deposits_count: 0,
              tx_requests_count: 0,
              tx_orders_count: 0,
              withdrawals_count: 0,
              total_events_size: 0,
            },
          }),
        );

        const before = yield* Metric.value(durationHistogram);
        yield* runAction();
        const after = yield* Metric.value(durationHistogram);

        expect(after.count - before.count).toBe(1);
      }),
  );

  it.effect("records a histogram observation on SeededOutput", () =>
    Effect.gen(function* () {
      makeWorkerInstance.mockReturnValue(
        makeEventWorker("message", { type: "SeededOutput" }),
      );

      const before = yield* Metric.value(durationHistogram);
      yield* runAction();
      const after = yield* Metric.value(durationHistogram);

      expect(after.count - before.count).toBe(1);
    }),
  );

  it.effect(
    "records a histogram observation when the worker emits a FailureOutput",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("message", {
            type: "FailureOutput",
            error: "sdk error",
          }),
        );

        const before = yield* Metric.value(durationHistogram);
        yield* runAction();
        const after = yield* Metric.value(durationHistogram);

        expect(after.count - before.count).toBe(1);
      }),
  );

  it.effect(
    "records a histogram observation when the worker emits an error event",
    () =>
      Effect.gen(function* () {
        makeWorkerInstance.mockReturnValue(
          makeEventWorker("error", new Error("thread crash")),
        );

        const before = yield* Metric.value(durationHistogram);
        yield* runAction();
        const after = yield* Metric.value(durationHistogram);

        expect(after.count - before.count).toBe(1);
      }),
  );
});
