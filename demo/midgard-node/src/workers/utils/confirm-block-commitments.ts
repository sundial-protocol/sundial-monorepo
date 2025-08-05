import { WorkerOutput as BlockCommitmentWorkerOutput } from "./commit-block-header.js";

export type WorkerInput = {
  data: BlockCommitmentWorkerOutput;
};

export type BlockConfirmationWorkerOutput = {
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
};
