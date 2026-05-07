import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  uint32ToFraudProofID,
  fraudProofsToIndexedValidators,
  createFraudProofCatalogueMpt,
} from "@/transactions/initialization.js";

describe("uint32ToFraudProofID encodes index as big-endian uint32", () => {
  it("uint32ToFraudProofID encodes index as big-endian uint32", () => {
    const buf = uint32ToFraudProofID(0);
    expect(buf).toBeInstanceOf(Buffer);
    expect(buf.length).toBe(4);
    expect(buf.readUInt32BE(0)).toBe(0);
  });
});

describe("uint32ToFraudProofID encodes non-zero index correctly", () => {
  it("uint32ToFraudProofID encodes non-zero index correctly", () => {
    const buf = uint32ToFraudProofID(1);
    expect(buf.readUInt32BE(0)).toBe(1);

    const buf256 = uint32ToFraudProofID(256);
    expect(buf256.readUInt32BE(0)).toBe(256);
  });
});

describe("fraudProofsToIndexedValidators returns indexed pairs", () => {
  it("fraudProofsToIndexedValidators returns indexed pairs", () => {
    const fraudProofs = {
      fpA: { spendingScriptHash: "aabb" },
      fpB: { spendingScriptHash: "ccdd" },
    } as any;
    const pairs = fraudProofsToIndexedValidators(fraudProofs);
    expect(pairs.length).toBe(2);
    expect(pairs[0][0]).toBeInstanceOf(Buffer);
    expect(pairs[0][0].readUInt32BE(0)).toBe(0);
    expect(pairs[0][1]).toEqual({ spendingScriptHash: "aabb" });
    expect(pairs[1][0].readUInt32BE(0)).toBe(1);
    expect(pairs[1][1]).toEqual({ spendingScriptHash: "ccdd" });
  });
});

describe("fraudProofsToIndexedValidators with empty object returns empty array", () => {
  it("fraudProofsToIndexedValidators with empty object returns empty array", () => {
    const pairs = fraudProofsToIndexedValidators({} as any);
    expect(pairs.length).toBe(0);
  });
});

describe("createFraudProofCatalogueMpt builds a trie with entries", () => {
  it.effect("createFraudProofCatalogueMpt builds a trie with entries", () =>
    Effect.gen(function* () {
      const pairs: [Buffer, any][] = [
        [uint32ToFraudProofID(0), { spendingScriptHash: "deadbeef" }],
        [uint32ToFraudProofID(1), { spendingScriptHash: "cafebabe" }],
      ];
      const trie = yield* createFraudProofCatalogueMpt(pairs);
      const root = yield* trie.getRootHex();
      expect(root).not.toBe(
        "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
      );
    }),
  );
});

describe("createFraudProofCatalogueMpt with no entries returns empty trie", () => {
  it.effect(
    "createFraudProofCatalogueMpt with no entries returns empty trie",
    () =>
      Effect.gen(function* () {
        const trie = yield* createFraudProofCatalogueMpt([]);
        const isEmpty = yield* trie.rootIsEmpty();
        expect(isEmpty).toBe(true);
      }),
  );
});
