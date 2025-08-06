import { parentPort, workerData } from "worker_threads";
import { Effect, Schedule, pipe } from "effect";
import { NodeConfig, User } from "@/config.js";
import { Database } from "@/services/database.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";

const inputData = workerData as WorkerInput;

const wrapper = (
  inputData: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User | Database> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;
    const retryPolicy = Schedule.exponential("100 millis").pipe(
      Schedule.compose(Schedule.recurs(4)),
    );
    yield* Effect.logInfo("🔹 Fetching latest commited block...");
    return {
      type: "FailedConfirmationOutput",
    };
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(Database.layer),
  Effect.provide(User.layer),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAll((e) =>
      Effect.succeed({
        type: "FailedConfirmationOutput",
        error:
          e instanceof Error
            ? e.message
            : "Unknown error from tx confirmation worker",
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(`👷 Work completed (${JSON.stringify(output)}).`),
  );
  parentPort?.postMessage(output);
});
