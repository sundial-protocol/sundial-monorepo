// NIT-006 … NIT-015  — Transaction and Mempool Flow

import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";

// Deterministic fixtures shared across the file.
const txCborA = Buffer.alloc(64, 0xbb);
const txCborB = Buffer.alloc(64, 0xcc);
// Spent outref produced by lucid stub (mockInputList.get returns {to_cbor_bytes: () => inputCborBytes}).
const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
// Produced outref produced by lucid stub CML.TransactionInput.new(txHash, 0).
const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]);
const outputCborBytes = Buffer.alloc(16, 0xcc);
// Output address from lucid stub.
const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
// Different address for seed entry (spent input) to avoid (event_id, address) collisions.
const spentAddress =
  "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl";

const makeSeedEntry = (): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: Buffer.alloc(32, 0xaa),
  [Ledger.Columns.OUTREF]: inputCborBytes,
  [Ledger.Columns.OUTPUT]: outputCborBytes,
  [Ledger.Columns.ADDRESS]: spentAddress,
});

const makeBaseLayers = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

// ─── NIT-006 ─────────────────────────────────────────────────────────────────

describe("Queued transaction reaches MempoolDB", () => {
  it.effect("Queued transaction reaches MempoolDB", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const found = yield* MempoolDB.retrieveTxCborByHash(processedTx.txId);

      expect(found).not.toBeUndefined();
      expect(Buffer.from(found!).equals(txCborA)).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-007 ─────────────────────────────────────────────────────────────────

describe("Queued transaction updates MempoolLedgerDB", () => {
  it.effect("Queued transaction updates MempoolLedgerDB", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const afterLedger = yield* MempoolLedgerDB.retrieve;
      const outrefs = afterLedger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );

      // Spent outref removed.
      expect(outrefs.some((o) => o === inputCborBytes.toString("hex"))).toBe(
        false,
      );
      // Produced outref present.
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-008 ─────────────────────────────────────────────────────────────────

describe("Queued transaction creates slated address history", () => {
  it.effect("Queued transaction creates slated address history", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      // Produced output goes to testAddress, spent input was at spentAddress.
      const producedHistory = yield* AddressHistoryDB.retrieve(testAddress);
      const spentHistory = yield* AddressHistoryDB.retrieve(spentAddress);

      expect(producedHistory.length + spentHistory.length).toBeGreaterThan(0);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-009 ─────────────────────────────────────────────────────────────────

describe("Multiple queued transactions are drained together", () => {
  it.effect("Multiple queued transactions are drained together", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const procA = yield* breakDownTx(txCborA);
      // Build a second processed tx with a distinct txId to avoid PK collision.
      const txIdB = Buffer.alloc(32, 0xbb);
      const procB = {
        txId: txIdB,
        txCbor: txCborB,
        spent: [] as Buffer[],
        produced: [
          {
            [Ledger.Columns.TX_ID]: txIdB,
            [Ledger.Columns.OUTREF]: Buffer.alloc(32, 0xbc),
            [Ledger.Columns.OUTPUT]: Buffer.alloc(16, 0xcc),
            [Ledger.Columns.ADDRESS]: testAddress,
          },
        ],
      };

      yield* MempoolDB.insertMultiple([procA, procB]);

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(2n);

      const afterLedger = yield* MempoolLedgerDB.retrieve;
      const outrefs = afterLedger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );
      expect(
        outrefs.some((o) => o === Buffer.alloc(32, 0xbc).toString("hex")),
      ).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-010 ─────────────────────────────────────────────────────────────────

describe("Mempool retrieval by hash sees newly processed transaction", () => {
  it.effect("Mempool retrieval by hash sees newly processed transaction", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const processed = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processed]);

      const cbor = yield* MempoolDB.retrieveTxCborByHash(processed.txId);

      expect(cbor).not.toBeUndefined();
      expect(Buffer.from(cbor!).equals(txCborA)).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-011 ─────────────────────────────────────────────────────────────────

describe("Mempool retrieval by hashes returns the processed set", () => {
  it.effect("Mempool retrieval by hashes returns the processed set", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const procA = yield* breakDownTx(txCborA);
      const txIdB = Buffer.alloc(32, 0xbb);
      const procB = {
        txId: txIdB,
        txCbor: txCborB,
        spent: [] as Buffer[],
        produced: [
          {
            [Ledger.Columns.TX_ID]: txIdB,
            [Ledger.Columns.OUTREF]: Buffer.alloc(32, 0xbc),
            [Ledger.Columns.OUTPUT]: Buffer.alloc(16, 0xcc),
            [Ledger.Columns.ADDRESS]: testAddress,
          },
        ],
      };

      yield* MempoolDB.insertMultiple([procA, procB]);

      const results = yield* MempoolDB.retrieveTxCborsByHashes([
        procA.txId,
        txIdB,
      ]);

      expect(results.length).toBe(2);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-012 ─────────────────────────────────────────────────────────────────

describe("Mempool count reflects processed transactions", () => {
  it.effect("Mempool count reflects processed transactions", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const count0 = yield* MempoolDB.retrieveTxCount;

      const procA = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([procA]);
      const count1 = yield* MempoolDB.retrieveTxCount;

      const txIdB = Buffer.alloc(32, 0xbb);
      const procB = {
        txId: txIdB,
        txCbor: txCborB,
        spent: [] as Buffer[],
        produced: [] as Ledger.Entry[],
      };
      yield* MempoolDB.insertMultiple([procB]);
      const count2 = yield* MempoolDB.retrieveTxCount;

      expect(count0).toBe(0n);
      expect(count1).toBe(1n);
      expect(count2).toBe(2n);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-013 ─────────────────────────────────────────────────────────────────

describe("Mempool time-bound query includes eligible requests", () => {
  it.effect("Mempool time-bound query includes eligible requests", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const procA = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([procA]);

      const midpoint = new Date();

      const txIdB = Buffer.alloc(32, 0xbb);
      const procB = {
        txId: txIdB,
        txCbor: txCborB,
        spent: [] as Buffer[],
        produced: [] as Ledger.Entry[],
      };
      yield* MempoolDB.insertMultiple([procB]);

      const startDate = new Date(0);
      const entries = yield* MempoolDB.retrieveTimeBoundEntries(
        startDate,
        midpoint,
      );

      expect(entries.length).toBe(1);
      expect(Buffer.from(entries[0].tx_id).equals(procA.txId)).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-014 ─────────────────────────────────────────────────────────────────

describe("Address transaction lookup sees mempool transaction", () => {
  it.effect("Address transaction lookup sees mempool transaction", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const processed = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processed]);

      const txCbors = yield* AddressHistoryDB.retrieve(testAddress);

      expect(txCbors.length).toBeGreaterThan(0);
      // At least one result should be txCborA.
      const found = txCbors.some((c) => Buffer.from(c).equals(txCborA));
      expect(found).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-015 ─────────────────────────────────────────────────────────────────

describe("Empty transaction queue leaves storage unchanged", () => {
  it.effect("Empty transaction queue leaves storage unchanged", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);

      const procA = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([procA]);

      // Empty insert is a no-op.
      yield* MempoolDB.insertMultiple([]);

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(1n);

      const ledger = yield* MempoolLedgerDB.retrieve;
      const outrefs = ledger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );
    }).pipe(Effect.provide(layers));
  });
});
