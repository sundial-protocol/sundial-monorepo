import { describe, expect, it } from "vitest";
import { isHexString } from "@/utils.js";

describe("MN-UNIT-001 isHexString", () => {
  it("accepts lowercase hex string", () => {
    expect(isHexString("deadbeef0123456789")).toBe(true);
  });
});
