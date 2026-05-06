import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  uint32ToFraudProofID,
  createFraudProofCatalogueMpt,
} from "@/transactions/initialization.js";

const EMPTY_ROOT =
  "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421";

describe("uint32ToFraudProofID", () => {
  it("uint32ToFraudProofID", () => {
    const result = uint32ToFraudProofID(1);
    expect(result).toBeInstanceOf(Buffer);
    expect(result.length).toBe(4);
    expect(result.readUInt32BE(0)).toBe(1);
  });
});

describe("fraud proof catalogue MPT", () => {
  it.effect("fraud proof catalogue MPT", () =>
    Effect.gen(function* () {
      const indexedProofs: [Buffer, { spendingScriptHash: string }][] = [
        [uint32ToFraudProofID(0), { spendingScriptHash: "aa".repeat(28) }],
        [uint32ToFraudProofID(1), { spendingScriptHash: "bb".repeat(28) }],
      ];
      const mpt = yield* createFraudProofCatalogueMpt(indexedProofs as any);
      const root = yield* mpt.getRootHex();
      const isEmpty = yield* mpt.rootIsEmpty();
      expect(isEmpty).toBe(false);
      expect(root).not.toBe(EMPTY_ROOT);
    }),
  );
});
