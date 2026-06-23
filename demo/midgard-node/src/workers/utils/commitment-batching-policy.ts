export type CommitmentBatchingDecisionInput = {
  txRequestsTotalInWindow: number;
  l1UserEventsCount: number;
  commitmentWindowAgeMs: number;
  minTxRequestsPerBlock: number;
  maxWaitMs: number;
};

export const shouldDelayCommitmentForBatch = ({
  txRequestsTotalInWindow,
  l1UserEventsCount,
  commitmentWindowAgeMs,
  minTxRequestsPerBlock,
  maxWaitMs,
}: CommitmentBatchingDecisionInput): boolean =>
  txRequestsTotalInWindow > 0 &&
  l1UserEventsCount === 0 &&
  txRequestsTotalInWindow < minTxRequestsPerBlock &&
  maxWaitMs > 0 &&
  commitmentWindowAgeMs < maxWaitMs;
