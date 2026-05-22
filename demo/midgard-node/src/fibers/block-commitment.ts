import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Metric, MetricBoundaries, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";
import { WorkerInput, WorkerOutput } from "@/workers/utils/block-commitment.js";
import { Worker } from "worker_threads";
import { BlocksDB } from "@/database/index.js";
import { performance } from "node:perf_hooks";
import {
  blocksDbSeedingMetrics,
  ensureBlocksDBSeededFromChain,
} from "@/fibers/seed-blocks-db-from-chain.js";

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

export const commitBlockDurationHistogramBoundaries =
  MetricBoundaries.exponential({ start: 0.1, factor: 2, count: 13 });

const commitBlockDurationHistogram = Metric.histogram(
  "commit_block_duration_seconds",
  commitBlockDurationHistogramBoundaries,
  "Histogram of block commitment worker duration in seconds (success or failure)",
).register();

export const blockCommitmentMetrics = {
  commitBlockNumTxGauge,
  commitBlockEventsSizeGauge,
  commitBlockCounter,
  commitBlockTxCounter,
  commitBlockL1UserEventsGauge,
  commitBlockCommitmentFailuresCounter,
  commitBlockDurationHistogram,
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
      COMMITMENT_WINDOW_WARN_TX_REQUESTS,
      COMMITMENT_WINDOW_WARN_TOTAL_EVENTS,
      COMMITMENT_WINDOW_WARN_TOTAL_BYTES,
    } = yield* NodeConfig;

    const worker = Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      let isDone = false;
      const complete = (effect: Effect.Effect<WorkerOutput, WorkerError>) => {
        if (!isDone) {
          isDone = true;
          clearTimeout(timeoutId);
          resume(effect);
        }
      };

      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const workerInputData: WorkerInput = { data: {} };
      const worker = new Worker(
        new URL("./block-commitment.js", import.meta.url),
        { workerData: workerInputData },
      );
      const timeoutId = setTimeout(() => {
        complete(
          Effect.fail(
            new WorkerError({
              worker: "commit-block-header",
              message: `Commitment worker timed out after ${COMMITMENT_WORKER_TIMEOUT_MS}ms`,
              cause: "Timed out waiting for worker output",
            }),
          ),
        );
        worker.terminate();
      }, COMMITMENT_WORKER_TIMEOUT_MS);
      worker.on("message", (output: WorkerOutput) => {
        if (output.type === "FailureOutput") {
          complete(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker failed`,
                cause: output.error,
              }),
            ),
          );
        } else {
          complete(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        complete(
          Effect.fail(
            new WorkerError({
              worker: "commit-block-header",
              message: `Error in commitment worker: ${e}`,
              cause: e,
            }),
          ),
        );
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          complete(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker exited with code: ${code}`,
                cause: `exit code ${code}`,
              }),
            ),
          );
        }
      });
      return Effect.sync(() => {
        clearTimeout(timeoutId);
        worker.terminate();
      });
    });

    const workerStartMs = performance.now();
    const workerOutput: WorkerOutput = yield* worker.pipe(
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
        yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
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
  WorkerError,
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
    yield* Metric.update(commitBlockDurationHistogram, 0);
    yield* Metric.incrementBy(commitBlockCounter, 0n);
    yield* Metric.incrementBy(commitBlockTxCounter, 0n);
    yield* Metric.incrementBy(commitBlockCommitmentFailuresCounter, 0n);
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
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
