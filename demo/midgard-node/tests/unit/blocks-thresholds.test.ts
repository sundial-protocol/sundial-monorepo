import { describe, expect, it } from "vitest";
import * as BlocksDB from "@/database/blocks.js";

const baseStats: BlocksDB.Stats = {
  [BlocksDB.Columns.DEPOSITS_COUNT]: 2,
  [BlocksDB.Columns.TX_REQUESTS_COUNT]: 3,
  [BlocksDB.Columns.TX_ORDERS_COUNT]: 4,
  [BlocksDB.Columns.WITHDRAWALS_COUNT]: 5,
  [BlocksDB.Columns.TOTAL_EVENTS_SIZE]: 256,
};

describe("BlocksDB commitment-window warning thresholds", () => {
  it("computes total events count across all event categories", () => {
    expect(BlocksDB.getTotalEventsCount(baseStats)).toBe(14);
  });

  it("reports threshold breaches when counts are equal-to or above thresholds", () => {
    const breaches = BlocksDB.getCommitmentWindowWarningThresholdBreaches(
      baseStats,
      {
        txRequestsCount: 3,
        totalEventsCount: 14,
        totalEventsSizeBytes: 256,
      },
    );

    expect(breaches).toEqual([
      "tx_requests_count",
      "total_events_count",
      "total_events_size_bytes",
    ]);
  });

  it("reports no breaches when all values are strictly below thresholds", () => {
    const breaches = BlocksDB.getCommitmentWindowWarningThresholdBreaches(
      baseStats,
      {
        txRequestsCount: 4,
        totalEventsCount: 15,
        totalEventsSizeBytes: 257,
      },
    );

    expect(breaches).toEqual([]);
  });
});
