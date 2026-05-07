import { describe, expect, it } from "vitest";
import {
  logSuccess,
  logWarning,
  logAbort,
  logInfo,
  FileSystemError,
} from "@/utils.js";

describe("logSuccess runs without error", () => {
  it("logSuccess runs without error", () => {
    expect(() => logSuccess("all good")).not.toThrow();
  });
});

describe("logWarning runs without error", () => {
  it("logWarning runs without error", () => {
    expect(() => logWarning("watch out")).not.toThrow();
  });
});

describe("logAbort runs without error", () => {
  it("logAbort runs without error", () => {
    expect(() => logAbort("aborted")).not.toThrow();
  });
});

describe("logInfo runs without error", () => {
  it("logInfo runs without error", () => {
    expect(() => logInfo("info message")).not.toThrow();
  });
});

describe("FileSystemError has the expected tag", () => {
  it("FileSystemError has the expected tag", () => {
    const err = new FileSystemError({
      message: "disk full",
      cause: new Error("ENOSPC"),
    });
    expect(err._tag).toBe("FileSystemError");
    expect(err.message).toBe("disk full");
  });
});

describe("FileSystemError without cause", () => {
  it("FileSystemError without cause", () => {
    const err = new FileSystemError({ message: "no cause", cause: undefined });
    expect(err._tag).toBe("FileSystemError");
  });
});
