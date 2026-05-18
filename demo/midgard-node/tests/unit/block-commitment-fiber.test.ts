import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Metric } from "effect";
import { Globals } from "@/services/globals.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { NodeConfig } from "@/services/config.js";
import {
  blockCommitmentMetrics,
  buildAndSubmitCommitmentBlockAction,
} from "@/fibers/block-commitment.js";
import { metricDelta } from "./harness/metric-snapshot.js";

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

const baseLayer = Layer.mergeAll(Globals.Default, makeTestNodeConfigLayer());

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
      Layer.succeed(
        NodeConfig,
        NodeConfig.of({ ...config, COMMITMENT_WORKER_TIMEOUT_MS: 10 }),
      ),
    );
  });
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

      const shortTimeoutLayer = yield* makeShortTimeoutLayer();
      const delta = yield* metricDelta(
        readFailureCounter,
        runAction(shortTimeoutLayer),
        (state) => state.count,
      );
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

      const shortTimeoutLayer = yield* makeShortTimeoutLayer();
      yield* runAction(shortTimeoutLayer);

      expect(neverRespondingWorker.terminate).toHaveBeenCalled();
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
