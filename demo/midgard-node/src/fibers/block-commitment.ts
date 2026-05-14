import { Globals, NodeConfig } from "@/services/index.js";
import { Effect, Ref, Schedule } from "effect";
import { WorkerError } from "@/workers/utils/common.js";
import { WorkerInput, WorkerOutput } from "@/workers/utils/block-commitment.js";
import { Metric } from "effect";
import { Worker } from "worker_threads";
import { BlocksDB } from "@/database/index.js";
import { performance } from "node:perf_hooks";

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

const commitBlockDurationGauge = Metric.gauge("commit_block_duration_seconds", {
  description:
    "Duration in seconds of the last block commitment worker run (success or failure)",
}).register();

export const buildAndSubmitCommitmentBlockAction = () =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const { COMMITMENT_WORKER_TIMEOUT_MS } = yield* NodeConfig;

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
          Metric.set(
            commitBlockDurationGauge,
            (performance.now() - workerStartMs) / 1000,
          ),
        onSuccess: (_) =>
          Metric.set(
            commitBlockDurationGauge,
            (performance.now() - workerStartMs) / 1000,
          ),
      }),
    );

    switch (workerOutput.type) {
      case "SuccessfulCommitmentOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);

        yield* Metric.set(
          commitBlockL1UserEventsGauge,
          workerOutput.stats[BlocksDB.Columns.DEPOSITS_COUNT] +
            workerOutput.stats[BlocksDB.Columns.WITHDRAWALS_COUNT] +
            workerOutput.stats[BlocksDB.Columns.TX_ORDERS_COUNT],
        );
        yield* Metric.set(
          commitBlockNumTxGauge,
          BigInt(workerOutput.stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
        );
        yield* Metric.set(
          commitBlockEventsSizeGauge,
          workerOutput.stats[BlocksDB.Columns.TOTAL_EVENTS_SIZE],
        );
        yield* Metric.increment(commitBlockCounter);
        yield* Metric.incrementBy(
          commitBlockTxCounter,
          BigInt(workerOutput.stats[BlocksDB.Columns.TX_REQUESTS_COUNT]),
        );
        yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
        break;
      }
      case "SeededOutput": {
        yield* Effect.logInfo(
          "🔹 ✅ BlocksDB seeded from chain. Will commit on next cycle.",
        );
        break;
      }
      case "FailureOutput": {
        yield* Metric.increment(commitBlockCommitmentFailuresCounter);
        break;
      }
    }
  });

export const blockCommitmentAction: Effect.Effect<
  void,
  WorkerError,
  Globals | NodeConfig
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
): Effect.Effect<void, never, Globals | NodeConfig> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    // Initialize metrics so panels have a visible baseline before first commit.
    yield* Metric.set(commitBlockNumTxGauge, 0n);
    yield* Metric.set(commitBlockEventsSizeGauge, 0);
    yield* Metric.set(commitBlockL1UserEventsGauge, 0);
    yield* Metric.set(commitBlockDurationGauge, 0);
    yield* Metric.incrementBy(commitBlockCounter, 0n);
    yield* Metric.incrementBy(commitBlockTxCounter, 0n);
    yield* Metric.incrementBy(commitBlockCommitmentFailuresCounter, 0n);
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
