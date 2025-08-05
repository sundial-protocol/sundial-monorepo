import { Effect, Schedule, pipe } from "effect";
import { NodeConfig, User } from "@/config.js";
import { Database } from "@/services/database.js";
import {
  BlockConfirmationWorkerInput as WorkerInput,
  BlockConfirmationWorkerOutput as WorkerOutput,
} from "@/utils.js";

const emptyOutput: WorkerOutput = {
  txSize: 0,
  mempoolTxsCount: 0,
  sizeOfBlocksTxs: 0,
};

const wrapper = (
  input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User | Database> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;
    return emptyOutput;
  });
