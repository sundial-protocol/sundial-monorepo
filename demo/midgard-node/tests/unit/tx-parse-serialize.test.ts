import { describe, expect, it } from "vitest";
import * as Ledger from "@/database/utils/ledger.js";
import type { ProcessedTx } from "@/utils.js";
import {
  deserializeProcessedTx,
  serializeProcessedTx,
} from "@/workers/utils/tx-parse.js";

const makeProcessedTx = (): ProcessedTx => ({
  txId: Buffer.alloc(32, 0x11),
  txCbor: Buffer.alloc(12, 0x22),
  spent: [Buffer.alloc(8, 0x33), Buffer.alloc(8, 0x44)],
  produced: [
    {
      [Ledger.Columns.TX_ID]: Buffer.alloc(32, 0x11),
      [Ledger.Columns.OUTREF]: Buffer.alloc(6, 0x55),
      [Ledger.Columns.OUTPUT]: Buffer.alloc(10, 0x66),
      [Ledger.Columns.ADDRESS]: "addr_test1_produced",
    },
  ],
});

describe("tx-parse worker serialization", () => {
  it("serializes a processed tx to hex-encoded fields", () => {
    const serialized = serializeProcessedTx(makeProcessedTx());

    expect(serialized.txIdHex).toBe("11".repeat(32));
    expect(serialized.txCborHex).toBe("22".repeat(12));
    expect(serialized.spentHex).toEqual(["33".repeat(8), "44".repeat(8)]);
    expect(serialized.produced).toEqual([
      {
        txIdHex: "11".repeat(32),
        outRefHex: "55".repeat(6),
        outputHex: "66".repeat(10),
        address: "addr_test1_produced",
      },
    ]);
  });

  it("round-trips through serialize/deserialize without data loss", () => {
    const original = makeProcessedTx();
    const restored = deserializeProcessedTx(serializeProcessedTx(original));

    expect(restored.txId.equals(original.txId)).toBe(true);
    expect(restored.txCbor.equals(original.txCbor)).toBe(true);
    expect(restored.spent).toHaveLength(original.spent.length);
    restored.spent.forEach((buf, i) => {
      expect(buf.equals(original.spent[i])).toBe(true);
    });
    expect(restored.produced).toHaveLength(1);
    const restoredEntry = restored.produced[0];
    const originalEntry = original.produced[0];
    expect(
      (restoredEntry[Ledger.Columns.OUTREF] as Buffer).equals(
        originalEntry[Ledger.Columns.OUTREF] as Buffer,
      ),
    ).toBe(true);
    expect(
      (restoredEntry[Ledger.Columns.OUTPUT] as Buffer).equals(
        originalEntry[Ledger.Columns.OUTPUT] as Buffer,
      ),
    ).toBe(true);
    expect(restoredEntry[Ledger.Columns.ADDRESS]).toBe(
      originalEntry[Ledger.Columns.ADDRESS],
    );
  });
});
