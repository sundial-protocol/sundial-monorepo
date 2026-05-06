import { describe, expect, it } from "vitest";
import { it as itEffect } from "@effect/vitest";
import { Effect } from "effect";
import { isHexString, batchProgram } from "@/utils.js";
import { utxoToOutRef, outRefsAreEqual } from "@/transactions/utils.js";

describe("Accept uppercase hex string", () => {
  it("Accept uppercase hex string", () => {
    expect(isHexString("DEADBEEF0123456789")).toBe(true);
  });
});

describe("Batch program uses expected ranges", () => {
  itEffect("Batch program uses expected ranges", () =>
    Effect.gen(function* () {
      const calls: [number, number][] = [];
      yield* batchProgram(2, 5, "test", (s, e) => {
        calls.push([s, e]);
        return Effect.succeed(undefined);
      });
      expect(calls).toEqual([
        [0, 2],
        [2, 4],
        [4, 6],
      ]);
    }),
  );
});

describe("Batch program returns continuation results", () => {
  itEffect("Batch program returns continuation results", () =>
    Effect.gen(function* () {
      const results = yield* batchProgram(3, 7, "test", (s, _e) =>
        Effect.succeed(s),
      );
      expect(results).toEqual([0, 3, 6]);
    }),
  );
});

describe("UTxO converts to out ref", () => {
  it("UTxO converts to out ref", () => {
    const utxo = {
      txHash: "aa".repeat(32),
      outputIndex: 2,
      address: "addr_test1...",
      assets: {},
    };
    const result = utxoToOutRef(utxo as any);
    expect(result).toEqual({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
    });
  });
});

describe("Equal out refs compare true", () => {
  it("Equal out refs compare true", () => {
    const ref = { txHash: "aa".repeat(32), outputIndex: 0 };
    expect(outRefsAreEqual(ref, { ...ref })).toBe(true);
  });
});
