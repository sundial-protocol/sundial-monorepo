import { expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  uint32ToFraudProofID,
  createFraudProofCatalogueMpt,
} from "@/transactions/initialization.js";
import { EMPTY_ROOT } from "../constants.js";

it("uint32ToFraudProofID encodes a 4-byte big-endian ID", () => {
  const result = uint32ToFraudProofID(1);
  expect(result).toBeInstanceOf(Buffer);
  expect(result.length).toBe(4);
  expect(result.readUInt32BE(0)).toBe(1);
});

it.effect("createFraudProofCatalogueMpt produces non-empty root", () =>
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
