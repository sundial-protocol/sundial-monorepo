import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Metric, MetricBoundaries, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";
import {
  CommitmentWorkerMessageType,
  WorkerInput,
  WorkerMessage,
  WorkerOutput,
} from "@/workers/utils/block-commitment.js";
import { Worker } from "worker_threads";
import { BlocksDB } from "@/database/index.js";
import { performance } from "node:perf_hooks";
import {
  blocksDbSeedingMetrics,
  ensureBlocksDBSeededFromChain,
} from "@/fibers/seed-blocks-db-from-chain.js";
import { DatabaseError } from "@/database/utils/common.js";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const COMMITMENT_WORKER_NAME = "commit-block-header";
const CURRENT_MODULE_DIR =
  typeof __filename !== "undefined"
    ? dirname(__filename)
    : dirname(fileURLToPath(import.meta.url));
const COMMITMENT_WORKER_URL = pathToFileURL(
  resolve(CURRENT_MODULE_DIR, "./block-commitment.js"),
);

type PendingCommitmentWorkerRequest = {
  readonly complete: (effect: Effect.Effect<WorkerOutput, WorkerError>) => void;
  readonly clearTimeout: () => void;
  readonly onMessage: (message: WorkerMessage) => void;
};

type CommitmentWorkerState = {
  readonly worker: Worker;
  pendingRequest: PendingCommitmentWorkerRequest | null;
};

let commitmentWorkerState: CommitmentWorkerState | null = null;
const commitmentWorkerSemaphore = Effect.unsafeMakeSemaphore(1);

const resetCommitmentWorkerState = (state?: CommitmentWorkerState | null) => {
  const activeState = state ?? commitmentWorkerState;
  if (activeState === null) {
    return;
  }
  commitmentWorkerState = null;
  if (activeState.pendingRequest !== null) {
    activeState.worker.off("message", activeState.pendingRequest.onMessage);
  }
  activeState.pendingRequest?.clearTimeout();
  activeState.pendingRequest = null;
};

const spawnCommitmentWorker = (): CommitmentWorkerState => {
  Effect.runSync(
    Effect.logInfo("👷 Starting persistent block commitment worker..."),
  );
  const worker = new Worker(COMMITMENT_WORKER_URL);
  const state: CommitmentWorkerState = {
    worker,
    pendingRequest: null,
  };
  worker.on("error", (error: Error) => {
    const pendingRequest = state.pendingRequest;
    resetCommitmentWorkerState(state);
    pendingRequest?.complete(
      Effect.fail(
        new WorkerError({
          worker: COMMITMENT_WORKER_NAME,
          message: `Error in commitment worker: ${error}`,
          cause: error,
        }),
      ),
    );
  });
  worker.on("exit", (code: number) => {
    const pendingRequest = state.pendingRequest;
    resetCommitmentWorkerState(state);
    pendingRequest?.complete(
      Effect.fail(
        new WorkerError({
          worker: COMMITMENT_WORKER_NAME,
          message:
            code === 0
              ? "Commitment worker exited while request was in-flight"
              : `Commitment worker exited with code: ${code}`,
          cause: `exit code ${code}`,
        }),
      ),
    );
  });
  commitmentWorkerState = state;
  return state;
};

const getOrSpawnCommitmentWorker = (): CommitmentWorkerState =>
  commitmentWorkerState ?? spawnCommitmentWorker();

const terminateCommitmentWorker = () => {
  const state = commitmentWorkerState;
  if (state === null) {
    return;
  }
  resetCommitmentWorkerState(state);
  void state.worker.terminate();
};

const runCommitmentWorkerCycle = (
  timeoutMs: number,
): Effect.Effect<WorkerOutput, WorkerError> =>
  commitmentWorkerSemaphore.withPermits(1)(
    Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      const state = getOrSpawnCommitmentWorker();

      let isDone = false;
      let onMessage: (message: WorkerMessage) => void = () => {};
      const complete = (effect: Effect.Effect<WorkerOutput, WorkerError>) => {
        if (isDone) {
          return;
        }
        isDone = true;
        state.worker.off("message", onMessage);
        state.pendingRequest = null;
        resume(effect);
      };

      const workerInputData: WorkerInput = {
        type: CommitmentWorkerMessageType.RunCommitment,
      };
      const timeoutId = setTimeout(() => {
        complete(
          Effect.fail(
            new WorkerError({
              worker: COMMITMENT_WORKER_NAME,
              message: `Commitment worker timed out after ${timeoutMs}ms`,
              cause: "Timed out waiting for worker output",
            }),
          ),
        );
        terminateCommitmentWorker();
      }, timeoutMs);
      const clearTimeoutFn = () => clearTimeout(timeoutId);

      onMessage = (message: WorkerMessage) => {
        if (message.type !== CommitmentWorkerMessageType.RunCommitmentResult) {
          return;
        }
        clearTimeoutFn();
        if (message.output.type === "FailureOutput") {
          complete(
            Effect.fail(
              new WorkerError({
                worker: COMMITMENT_WORKER_NAME,
                message: "Commitment worker failed",
                cause: message.output.error,
              }),
            ),
          );
          return;
        }
        complete(Effect.succeed(message.output));
      };

      state.pendingRequest = {
        complete,
        clearTimeout: clearTimeoutFn,
        onMessage,
      };
      state.worker.on("message", onMessage);
      state.worker.postMessage(workerInputData);

      return Effect.sync(() => {
        const pendingRequest = state.pendingRequest;
        if (pendingRequest?.onMessage === onMessage) {
          pendingRequest.clearTimeout();
          state.pendingRequest = null;
          terminateCommitmentWorker();
        }
        state.worker.off("message", onMessage);
      });
    }),
  );

export const unsafeResetCommitmentWorkerForTesting = () => {
  terminateCommitmentWorker();
};

const commitBlockNumTxGauge = Metric.gauge("commit_block_txs_per_block", {
  description:
    "A gauge for tracking the number of transactions in the most recently committed block",
  bigint: true,
}).register();

const commitBlockEventsSizeGauge = Metric.gauge(
  "commit_block_events_size_bytes",
  {
    description:
      "A gauge for tracking the total byte size of all events (deposits, withdrawals, tx orders, tx requests) in the most recently committed block",
  },
).register();

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
}).register();

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in committed blocks",
  bigint: true,
  incremental: true,
}).register();

const commitBlockL1UserEventsGauge = Metric.gauge(
  "commit_block_l1_user_events",
  {
    description:
      "A gauge for tracking the number of L1 user events (deposits, withdrawals, and tx orders) in the most recently committed block",
  },
).register();

const commitBlockCommitmentFailuresCounter = Metric.counter(
  "commit_block_commitment_failures",
  {
    description:
      "A counter for tracking the number of block commitment worker failures (timeouts, crashes, and SDK/CML errors)",
    bigint: true,
    incremental: true,
  },
).register();

const commitBlockBackpressureSkipsCounter = Metric.counter(
  "commit_block_backpressure_skips",
  {
    description:
      "A counter for commitment cycles skipped due to unsubmitted block backlog pressure",
    bigint: true,
    incremental: true,
  },
).register();

const commitmentWindowTxRequestsTotalGauge = Metric.gauge(
  "commitment_window_tx_requests_total",
  {
    description:
      "Total tx requests observed in the commitment window before per-block capping",
    bigint: true,
  },
).register();

const commitmentWindowTxRequestsSelectedGauge = Metric.gauge(
  "commitment_window_tx_requests_selected",
  {
    description:
      "Tx requests selected for processing in the latest commitment cycle",
    bigint: true,
  },
).register();

const commitmentWindowTxRequestsDeferredGauge = Metric.gauge(
  "commitment_window_tx_requests_deferred",
  {
    description:
      "Tx requests deferred from the latest commitment cycle due to commitment window capping",
    bigint: true,
  },
).register();

const commitmentWindowTxRequestsDeferredCounter = Metric.counter(
  "commitment_window_tx_requests_deferred_total",
  {
    description:
      "Total tx requests deferred across commitment cycles due to commitment window capping",
    bigint: true,
    incremental: true,
  },
).register();

const commitmentWindowAgeSecondsGauge = Metric.gauge(
  "commitment_window_age_seconds",
  {
    description:
      "Age in seconds of the current commitment window observed by the latest worker cycle",
  },
).register();

const commitmentBatchWaitSkipsCounter = Metric.counter(
  "commitment_batch_wait_skips_total",
  {
    description:
      "Total commitment cycles intentionally delayed while waiting for a larger tx-only batch",
    bigint: true,
    incremental: true,
  },
).register();

export const commitBlockDurationHistogramBoundaries =
  MetricBoundaries.exponential({ start: 0.1, factor: 2, count: 13 });

const commitBlockDurationHistogram = Metric.histogram(
  "commit_block_duration_seconds",
  commitBlockDurationHistogramBoundaries,
  "Histogram of block commitment worker duration in seconds (success or failure)",
).register();

// Inclusion latency: how long the commitment window spanned before this block
// was committed. Equivalently, the age of the oldest event included in the
// block (a conservative upper bound on per-tx mempool-accepted -> committed
// latency). Complements the last-value commitment_window_age_seconds gauge with
// a distribution for p50/p95/p99 reporting.
const txIngressToCommitDurationHistogram = Metric.histogram(
  "tx_ingress_to_commit_duration_seconds",
  MetricBoundaries.exponential({ start: 0.5, factor: 2, count: 13 }),
  "Histogram of commitment window span in seconds per committed block (inclusion latency upper bound)",
).register();

export const blockCommitmentMetrics = {
  commitBlockNumTxGauge,
  commitBlockEventsSizeGauge,
  commitBlockCounter,
  commitBlockTxCounter,
  commitBlockL1UserEventsGauge,
  commitBlockCommitmentFailuresCounter,
  commitBlockBackpressureSkipsCounter,
  commitmentWindowTxRequestsTotalGauge,
  commitmentWindowTxRequestsSelectedGauge,
  commitmentWindowTxRequestsDeferredGauge,
  commitmentWindowTxRequestsDeferredCounter,
  commitmentWindowAgeSecondsGauge,
  commitmentBatchWaitSkipsCounter,
  commitBlockDurationHistogram,
  txIngressToCommitDurationHistogram,
  ...blocksDbSeedingMetrics,
} as const;

export const buildAndSubmitCommitmentBlockAction = () =>
  Effect.gen(function* () {
    const seedResult = yield* ensureBlocksDBSeededFromChain;
    if (seedResult === "retry-later") {
      return;
    }
    if (seedResult === "seeded") {
      yield* Effect.logInfo(
        "🔹 ✅ BlocksDB seeded from chain. Will commit on next cycle.",
      );
      return;
    }

    const globals = yield* Globals;
    const {
      COMMITMENT_WORKER_TIMEOUT_MS,
      COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG,
      COMMITMENT_WINDOW_WARN_TX_REQUESTS,
      COMMITMENT_WINDOW_WARN_TOTAL_EVENTS,
      COMMITMENT_WINDOW_WARN_TOTAL_BYTES,
    } = yield* NodeConfig;
    const pendingUnsubmittedBacklog = yield* BlocksDB.countPendingBlocks;
    const maxUnsubmittedBacklog = BigInt(
      COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG,
    );
    if (pendingUnsubmittedBacklog > maxUnsubmittedBacklog) {
      yield* Metric.increment(commitBlockBackpressureSkipsCounter);
      yield* Effect.logWarning(
        `Commitment backpressure active: skipping cycle because pending_unsubmitted_blocks=${pendingUnsubmittedBacklog.toString()} exceeded threshold=${COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG}`,
      );
      return;
    }

    const workerStartMs = performance.now();
    const workerOutput: WorkerOutput = yield* runCommitmentWorkerCycle(
      COMMITMENT_WORKER_TIMEOUT_MS,
    ).pipe(
      Effect.tapBoth({
        onFailure: (_) =>
          Effect.all([
            Metric.update(
              commitBlockDurationHistogram,
              (performance.now() - workerStartMs) / 1000,
            ),
            Metric.increment(commitBlockCommitmentFailuresCounter),
          ]),
        onSuccess: (_) =>
          Metric.update(
            commitBlockDurationHistogram,
            (performance.now() - workerStartMs) / 1000,
          ),
      }),
    );

    switch (workerOutput.type) {
      case "SuccessfulCommitmentOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        const stats = workerOutput.stats;
        const commitmentWindow = workerOutput.commitmentWindow;
        const txRequestsTotalInWindow =
          commitmentWindow?.txRequestsTotalInWindow ??
          stats[BlocksDB.Columns.TX_REQUESTS_COUNT];
        const txRequestsSelected =
          commitmentWindow?.txRequestsSelected ??
          stats[BlocksDB.Columns.TX_REQUESTS_COUNT];
        const txRequestsDeferredInWindow =
          commitmentWindow?.txRequestsDeferredInWindow ?? 0;
        const totalEventsCount = BlocksDB.getTotalEventsCount(stats);
        const thresholdBreaches =
          BlocksDB.getCommitmentWindowWarningThresholdBreaches(stats, {
            txRequestsCount: COMMITMENT_WINDOW_WARN_TX_REQUESTS,
            totalEventsCount: COMMITMENT_WINDOW_WARN_TOTAL_EVENTS,
            totalEventsSizeBytes: COMMITMENT_WINDOW_WARN_TOTAL_BYTES,
          });

        yield* Metric.set(
          commitBlockL1UserEventsGauge,
          stats[BlocksDB.Columns.DEPOSITS_COUNT] +
            stats[BlocksDB.Columns.WITHDRAWALS_COUNT] +
            stats[BlocksDB.Columns.TX_ORDERS_COUNT],
        );
        yield* Metric.set(
          commitBlockNumTxGauge,
          BigInt(stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
        );
        yield* Metric.set(
          commitmentWindowTxRequestsTotalGauge,
          BigInt(txRequestsTotalInWindow),
        );
        yield* Metric.set(
          commitmentWindowTxRequestsSelectedGauge,
          BigInt(txRequestsSelected),
        );
        yield* Metric.set(
          commitmentWindowTxRequestsDeferredGauge,
          BigInt(txRequestsDeferredInWindow),
        );
        yield* Metric.set(
          commitmentWindowAgeSecondsGauge,
          (commitmentWindow?.windowAgeMs ?? 0) / 1000,
        );
        if (commitmentWindow?.windowAgeMs !== undefined) {
          yield* Metric.update(
            txIngressToCommitDurationHistogram,
            commitmentWindow.windowAgeMs / 1000,
          );
        }
        yield* Metric.incrementBy(
          commitmentWindowTxRequestsDeferredCounter,
          BigInt(txRequestsDeferredInWindow),
        );
        yield* Metric.set(
          commitBlockEventsSizeGauge,
          stats[BlocksDB.Columns.TOTAL_EVENTS_SIZE],
        );
        yield* Metric.increment(commitBlockCounter);
        yield* Metric.incrementBy(
          commitBlockTxCounter,
          BigInt(stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
        );
        if (thresholdBreaches.length > 0) {
          yield* Effect.logWarning(
            `Committed block exceeded commitment window warning thresholds: breaches=${thresholdBreaches.join(",")} tx_requests=${stats[BlocksDB.Columns.TX_REQUESTS_COUNT]}/${COMMITMENT_WINDOW_WARN_TX_REQUESTS} total_events=${totalEventsCount}/${COMMITMENT_WINDOW_WARN_TOTAL_EVENTS} total_events_size_bytes=${stats[BlocksDB.Columns.TOTAL_EVENTS_SIZE]}/${COMMITMENT_WINDOW_WARN_TOTAL_BYTES}`,
          );
        }
        yield* Effect.logInfo("🔹 ☑️  Block commitment completed.");
        break;
      }
      case "NoopCommitmentOutput": {
        const commitmentWindow = workerOutput.commitmentWindow;
        if (workerOutput.reason === "waiting_for_min_tx_batch") {
          yield* Metric.increment(commitmentBatchWaitSkipsCounter);
          yield* Metric.set(commitBlockL1UserEventsGauge, 0);
          yield* Metric.set(commitBlockNumTxGauge, 0n);
          yield* Metric.set(
            commitmentWindowTxRequestsTotalGauge,
            BigInt(commitmentWindow?.txRequestsTotalInWindow ?? 0),
          );
          yield* Metric.set(
            commitmentWindowTxRequestsSelectedGauge,
            BigInt(commitmentWindow?.txRequestsSelected ?? 0),
          );
          yield* Metric.set(commitmentWindowTxRequestsDeferredGauge, 0n);
          yield* Metric.set(
            commitmentWindowAgeSecondsGauge,
            (commitmentWindow?.windowAgeMs ?? 0) / 1000,
          );
          yield* Metric.set(commitBlockEventsSizeGauge, 0);
          yield* Effect.logInfo(
            "🔹 No-op commitment cycle: waiting for a larger tx-only batch before committing.",
          );
          break;
        }
        yield* Metric.set(commitBlockL1UserEventsGauge, 0);
        yield* Metric.set(commitBlockNumTxGauge, 0n);
        yield* Metric.set(commitmentWindowTxRequestsTotalGauge, 0n);
        yield* Metric.set(commitmentWindowTxRequestsSelectedGauge, 0n);
        yield* Metric.set(commitmentWindowTxRequestsDeferredGauge, 0n);
        yield* Metric.set(commitmentWindowAgeSecondsGauge, 0);
        yield* Metric.set(commitBlockEventsSizeGauge, 0);
        yield* Effect.logInfo(
          "🔹 No-op commitment cycle: skipped empty window (no events).",
        );
        break;
      }
      case "SeededOutput": {
        yield* Effect.logInfo(
          "🔹 ✅ BlocksDB seeded from chain. Will commit on next cycle.",
        );
        break;
      }
    }
  });

export const blockCommitmentAction: Effect.Effect<
  void,
  WorkerError | DatabaseError,
  AlwaysSucceedsContract | Database | Globals | Lucid | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (!RESET_IN_PROGRESS) {
    yield* Effect.logInfo("🔹 New block commitment process started.");
    yield* buildAndSubmitCommitmentBlockAction().pipe(
      Effect.withSpan("buildAndSubmitCommitmentBlockAction"),
    );
  }
});

export const blockCommitmentFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  AlwaysSucceedsContract | Database | Globals | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    // Initialize metrics so panels have a visible baseline before first commit.
    yield* Metric.set(commitBlockNumTxGauge, 0n);
    yield* Metric.set(commitBlockEventsSizeGauge, 0);
    yield* Metric.set(commitBlockL1UserEventsGauge, 0);
    yield* Metric.set(commitmentWindowTxRequestsTotalGauge, 0n);
    yield* Metric.set(commitmentWindowTxRequestsSelectedGauge, 0n);
    yield* Metric.set(commitmentWindowTxRequestsDeferredGauge, 0n);
    yield* Metric.set(commitmentWindowAgeSecondsGauge, 0);
    yield* Metric.update(commitBlockDurationHistogram, 0);
    yield* Metric.incrementBy(commitBlockCounter, 0n);
    yield* Metric.incrementBy(commitBlockTxCounter, 0n);
    yield* Metric.incrementBy(commitBlockCommitmentFailuresCounter, 0n);
    yield* Metric.incrementBy(commitBlockBackpressureSkipsCounter, 0n);
    yield* Metric.incrementBy(commitmentWindowTxRequestsDeferredCounter, 0n);
    yield* Metric.incrementBy(commitmentBatchWaitSkipsCounter, 0n);
    yield* Metric.incrementBy(
      blocksDbSeedingMetrics.seedBlocksDbAttemptsCounter,
      0n,
    );
    yield* Metric.incrementBy(
      blocksDbSeedingMetrics.seedBlocksDbSuccessCounter,
      0n,
    );
    yield* Metric.incrementBy(
      blocksDbSeedingMetrics.seedBlocksDbFailuresCounter,
      0n,
    );
    yield* Metric.set(blocksDbSeedingMetrics.seedBlocksDbTraversalHopsGauge, 0);
    yield* Metric.update(
      blocksDbSeedingMetrics.seedBlocksDbDurationHistogram,
      0,
    );
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber", { root: true }),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
