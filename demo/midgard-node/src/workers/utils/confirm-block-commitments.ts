import {
  WorkerOutput as BlockCommitmentWorkerOutput,
  SerializedStateQueueUTxO,
} from "./commit-block-header.js";

export type WorkerInput = {
  data: BlockCommitmentWorkerOutput;
};

export type SuccessfulConfirmationOutput = {
  type: "SuccessfulConfirmationOutput";
  blocksUTxO: SerializedStateQueueUTxO;
};

export type FailedConfirmationOutput = {
  type: "FailedConfirmationOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulConfirmationOutput
  | FailedConfirmationOutput;
