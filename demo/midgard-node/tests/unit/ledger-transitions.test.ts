import { describe, expect, beforeEach, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

const cmlMocks = vi.hoisted(() => ({
  transactionInputFromCborBytes: vi.fn((bytes: Buffer) => ({
    transaction_id: () => ({
      to_hex: () => bytes.subarray(0, 1).toString("hex"),
    }),
    index: () => BigInt(bytes[1] ?? 0),
  })),
}));

const txBreakdownMocks = vi.hoisted(() => ({
  breakDownTx: vi.fn(),
}));

vi.mock("@lucid-evolution/lucid", () => ({
  CML: {
    TransactionInput: {
      from_cbor_bytes: cmlMocks.transactionInputFromCborBytes,
    },
  },
}));

vi.mock("@al-ft/midgard-sdk", () => {
  class CmlDeserializationError extends Error {
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
      this.name = "CmlDeserializationError";
    }
  }

  return {
    CmlDeserializationError,
  };
});

vi.mock("@/utils.js", () => ({
  breakDownTx: txBreakdownMocks.breakDownTx,
}));

import * as Ledger from "../../src/database/utils/ledger.js";
import { makeLedgerEntry } from "./harness/fixtures.js";

beforeEach(() => {
  vi.clearAllMocks();
});

const outrefA = Buffer.from([0xaa, 0x00]);
const outrefB = Buffer.from([0xbb, 0x01]);
const outrefC = Buffer.from([0xcc, 0x02]);

describe("Ledger transition helpers", () => {
  it.effect("removeSpentOutRef removes the matching ledger outref", () => {
    const entryA = makeLedgerEntry(0x10, { outref: outrefA });
    const entryB = makeLedgerEntry(0x20, { outref: outrefB });

    return Ledger.removeSpentOutRef([entryA, entryB], outrefA).pipe(
      Effect.map((remaining) => {
        expect(remaining).toEqual([entryB]);
        expect(cmlMocks.transactionInputFromCborBytes).toHaveBeenCalledTimes(4);
      }),
    );
  });

  it.effect("applyTx adds produced outputs and removes spent outputs", () => {
    const entryA = makeLedgerEntry(0x10, { outref: outrefA });
    const entryB = makeLedgerEntry(0x20, { outref: outrefB });
    const produced = makeLedgerEntry(0x30, { outref: outrefC });
    const txCbor = Buffer.from([0x99]);

    txBreakdownMocks.breakDownTx.mockReturnValue(
      Effect.succeed({
        spent: [outrefA],
        produced: [produced],
      }),
    );

    return Ledger.applyTx([entryA, entryB], txCbor).pipe(
      Effect.map((nextLedger) => {
        expect(txBreakdownMocks.breakDownTx).toHaveBeenCalledWith(txCbor);
        expect(nextLedger).toEqual([entryB, produced]);
      }),
    );
  });
});
