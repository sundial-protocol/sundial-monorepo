import { describe, expect, it } from "vitest";
import { resolveSubmissionBatchSize } from "@/fibers/block-submission.js";

describe("resolveSubmissionBatchSize", () => {
  it("uses minimum batch size for small workloads", () => {
    expect(resolveSubmissionBatchSize(0)).toBe(250);
    expect(resolveSubmissionBatchSize(999)).toBe(250);
  });

  it("uses default batch size for medium workloads", () => {
    expect(resolveSubmissionBatchSize(1000)).toBe(1000);
    expect(resolveSubmissionBatchSize(4999)).toBe(1000);
  });

  it("uses larger batch size for high workloads", () => {
    expect(resolveSubmissionBatchSize(5000)).toBe(2500);
    expect(resolveSubmissionBatchSize(19999)).toBe(2500);
  });

  it("uses maximum batch size for very large workloads", () => {
    expect(resolveSubmissionBatchSize(20000)).toBe(5000);
    expect(resolveSubmissionBatchSize(100000)).toBe(5000);
  });
});
