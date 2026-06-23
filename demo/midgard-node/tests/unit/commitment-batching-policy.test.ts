import { describe, expect, it } from "vitest";
import { shouldDelayCommitmentForBatch } from "@/workers/utils/commitment-batching-policy.js";

describe("shouldDelayCommitmentForBatch", () => {
  it("delays a tx-only window that is below the batch threshold and max wait", () => {
    expect(
      shouldDelayCommitmentForBatch({
        txRequestsTotalInWindow: 250,
        l1UserEventsCount: 0,
        commitmentWindowAgeMs: 4_000,
        minTxRequestsPerBlock: 1_000,
        maxWaitMs: 10_000,
      }),
    ).toBe(true);
  });

  it("does not delay once the tx threshold is reached", () => {
    expect(
      shouldDelayCommitmentForBatch({
        txRequestsTotalInWindow: 1_000,
        l1UserEventsCount: 0,
        commitmentWindowAgeMs: 4_000,
        minTxRequestsPerBlock: 1_000,
        maxWaitMs: 10_000,
      }),
    ).toBe(false);
  });

  it("does not delay once the max wait is exceeded", () => {
    expect(
      shouldDelayCommitmentForBatch({
        txRequestsTotalInWindow: 250,
        l1UserEventsCount: 0,
        commitmentWindowAgeMs: 10_000,
        minTxRequestsPerBlock: 1_000,
        maxWaitMs: 10_000,
      }),
    ).toBe(false);
  });

  it("does not delay when authenticated L1 user events are present", () => {
    expect(
      shouldDelayCommitmentForBatch({
        txRequestsTotalInWindow: 250,
        l1UserEventsCount: 2,
        commitmentWindowAgeMs: 4_000,
        minTxRequestsPerBlock: 1_000,
        maxWaitMs: 10_000,
      }),
    ).toBe(false);
  });

  it("does not delay when batching is disabled", () => {
    expect(
      shouldDelayCommitmentForBatch({
        txRequestsTotalInWindow: 250,
        l1UserEventsCount: 0,
        commitmentWindowAgeMs: 4_000,
        minTxRequestsPerBlock: 1,
        maxWaitMs: 0,
      }),
    ).toBe(false);
  });
});
