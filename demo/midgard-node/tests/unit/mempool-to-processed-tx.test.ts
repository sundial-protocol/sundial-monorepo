import { beforeEach, describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import * as MempoolDB from "@/database/mempool.js";
import * as Tx from "@/database/utils/tx.js";

const breakDownTxFn = vi.hoisted(() => vi.fn());

vi.mock("@/utils.js", () => ({
  breakDownTx: (...args: unknown[]) => breakDownTxFn(...args),
}));

describe("MempoolDB.toProcessedTx", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it.effect("uses persisted normalized effects when present", () =>
    Effect.gen(function* () {
      const txId = Buffer.alloc(32, 0xaa);
      const txCbor = Buffer.from([0x01, 0x02]);
      const spentOutRef = Buffer.alloc(32, 0xbb);
      const producedOutRef = Buffer.alloc(32, 0xcc);
      const producedOutput = Buffer.from([0x03, 0x04, 0x05]);
      const producedAddress = "addr_test1qpersisted";
      const timestamp = new Date("2026-01-01T00:00:00.000Z");

      const entry: MempoolDB.EntryWithEffects = {
        [Tx.Columns.TX_ID]: txId,
        [Tx.Columns.TX]: txCbor,
        [Tx.Columns.TIMESTAMPTZ]: timestamp,
        [MempoolDB.Columns.TX_SIZE_BYTES]: txCbor.length,
        [MempoolDB.Columns.SPENT_OUTREFS]: [spentOutRef],
        [MempoolDB.Columns.PRODUCED_OUTREFS]: [producedOutRef],
        [MempoolDB.Columns.PRODUCED_OUTPUTS]: [producedOutput],
        [MempoolDB.Columns.PRODUCED_ADDRESSES]: [producedAddress],
      };

      const processedTx = yield* MempoolDB.toProcessedTx(entry);

      expect(breakDownTxFn).not.toHaveBeenCalled();
      expect(processedTx.txId.equals(txId)).toBe(true);
      expect(processedTx.txCbor.equals(txCbor)).toBe(true);
      expect(processedTx.spent).toEqual([spentOutRef]);
      expect(processedTx.produced).toEqual([
        {
          tx_id: txId,
          outref: producedOutRef,
          output: producedOutput,
          address: producedAddress,
        },
      ]);
    }),
  );

  it.effect(
    "falls back to breakDownTx when normalized effects are missing",
    () =>
      Effect.gen(function* () {
        const txId = Buffer.alloc(32, 0xdd);
        const txCbor = Buffer.from([0x08, 0x09]);
        const timestamp = new Date("2026-01-01T00:00:00.000Z");
        const expectedProcessedTx = {
          txId,
          txCbor,
          spent: [Buffer.alloc(32, 0x11)],
          produced: [],
        };

        breakDownTxFn.mockReturnValue(Effect.succeed(expectedProcessedTx));

        const entry: MempoolDB.EntryWithEffects = {
          [Tx.Columns.TX_ID]: txId,
          [Tx.Columns.TX]: txCbor,
          [Tx.Columns.TIMESTAMPTZ]: timestamp,
          [MempoolDB.Columns.TX_SIZE_BYTES]: null,
          [MempoolDB.Columns.SPENT_OUTREFS]: null,
          [MempoolDB.Columns.PRODUCED_OUTREFS]: null,
          [MempoolDB.Columns.PRODUCED_OUTPUTS]: null,
          [MempoolDB.Columns.PRODUCED_ADDRESSES]: null,
        };

        const processedTx = yield* MempoolDB.toProcessedTx(entry);

        expect(breakDownTxFn).toHaveBeenCalledOnce();
        expect(breakDownTxFn).toHaveBeenCalledWith(txCbor);
        expect(processedTx).toEqual(expectedProcessedTx);
      }),
  );
});
