import { afterEach, describe, expect, it } from "vitest";
import { Effect, Exit } from "effect";
import {
  parseTxCborInWorkerPool,
  unsafeResetTxParseWorkerPoolForTesting,
} from "@/fibers/tx-parse-worker-pool.js";

// In the unit environment the compiled worker file (src/fibers/tx-parse.js) is
// absent, so the pool transparently falls back to in-process `breakDownTx`.
// This exercises the fallback branch and the reset hook without spawning a real
// worker thread (which is integration-test territory).
describe("parseTxCborInWorkerPool fallback", () => {
  afterEach(() => {
    unsafeResetTxParseWorkerPoolForTesting();
  });

  it("parses via the in-process fallback when no worker file exists", async () => {
    // The stubbed CML rejects this payload, so the fallback path surfaces a
    // deserialization failure rather than hanging on a worker round-trip.
    const exit = await Effect.runPromiseExit(parseTxCborInWorkerPool("00", 1));
    expect(Exit.isFailure(exit)).toBe(true);
  });

  it("surfaces malformed hex as an Effect failure instead of throwing", async () => {
    // Regression guard: fromHex() throws a plain synchronous Error on
    // odd-length hex. It must be caught inside Effect.try rather than
    // escaping this call as an uncaught exception.
    const exit = await Effect.runPromiseExit(parseTxCborInWorkerPool("abc", 1));
    expect(Exit.isFailure(exit)).toBe(true);
  });

  it("reset is safe to call when no pool has been created", () => {
    expect(() => unsafeResetTxParseWorkerPoolForTesting()).not.toThrow();
  });
});
