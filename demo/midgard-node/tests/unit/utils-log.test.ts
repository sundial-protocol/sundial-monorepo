import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  logSuccess,
  logWarning,
  logAbort,
  logInfo,
  FileSystemError,
} from "@/utils.js";

describe("log helpers", () => {
  beforeEach(() => {
    vi.spyOn(console, "log").mockImplementation(() => {});
    vi.spyOn(console, "warn").mockImplementation(() => {});
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("logSuccess writes to console", () => {
    logSuccess("all good");
    expect(vi.mocked(console.log)).toHaveBeenCalled();
  });

  it("logWarning writes to console", () => {
    logWarning("watch out");
    const warned =
      vi.mocked(console.warn).mock.calls.length > 0 ||
      vi.mocked(console.log).mock.calls.length > 0;
    expect(warned).toBe(true);
  });

  it("logAbort writes to console", () => {
    logAbort("aborted");
    const logged =
      vi.mocked(console.error).mock.calls.length > 0 ||
      vi.mocked(console.log).mock.calls.length > 0;
    expect(logged).toBe(true);
  });

  it("logInfo writes to console", () => {
    logInfo("info message");
    expect(vi.mocked(console.log)).toHaveBeenCalled();
  });
});

describe("FileSystemError", () => {
  it("has the expected tag and message", () => {
    const err = new FileSystemError({
      message: "disk full",
      cause: new Error("ENOSPC"),
    });
    expect(err._tag).toBe("FileSystemError");
    expect(err.message).toBe("disk full");
  });

  it("works without cause", () => {
    const err = new FileSystemError({ message: "no cause", cause: undefined });
    expect(err._tag).toBe("FileSystemError");
  });
});
