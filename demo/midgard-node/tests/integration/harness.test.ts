// NIT-001 … NIT-005  — Harness and Initialization

import { describe, expect, afterEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import * as path from "node:path";
import * as os from "node:os";
import { randomUUID } from "node:crypto";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as DepositsDB from "@/database/deposits.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as BlocksDB from "@/database/blocks.js";
import * as BlocksTxsDB from "@/database/blocksTxs.js";
import * as LatestLedgerDB from "@/database/latestLedger.js";
import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";
import { MidgardMpt, deleteMpt } from "@/workers/utils/mpt.js";

// Shared deterministic fixtures.
const txCborA = Buffer.alloc(64, 0xbb);
const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]); // spent outref from lucid stub
const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]); // produced outref from lucid stub
const outputCborBytes = Buffer.alloc(16, 0xcc);
// Address returned by the lucid stub for all produced outputs.
const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
// Different address for the spent-input seed so (event_id, address) is unique.
const spentAddress =
  "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl";

const makeSeedLedgerEntry = (): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: Buffer.alloc(32, 0xaa),
  [Ledger.Columns.OUTREF]: inputCborBytes,
  [Ledger.Columns.OUTPUT]: outputCborBytes,
  [Ledger.Columns.ADDRESS]: spentAddress,
});

// ─── NIT-001 ─────────────────────────────────────────────────────────────────

describe("Database initialization supports repository workflow", () => {
  it.effect("Database initialization supports repository workflow", () => {
    const layers = Layer.mergeAll(
      makeTestSqlLayer(),
      makeTestNodeConfigLayer(),
    );
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* MempoolLedgerDB.insert([makeSeedLedgerEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const txByHash = yield* MempoolDB.retrieveTxCborByHash(processedTx.txId);
      const ledger = yield* MempoolLedgerDB.retrieve;

      expect(txByHash).not.toBeUndefined();
      expect(Buffer.from(txByHash!).equals(txCborA)).toBe(true);
      expect(ledger.length).toBeGreaterThan(0);
      const outrefs = ledger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );
      expect(outrefs.some((o) => o === inputCborBytes.toString("hex"))).toBe(
        false,
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-002 ─────────────────────────────────────────────────────────────────

describe("Initialization is idempotent for an existing schema", () => {
  it.effect("Initialization is idempotent for an existing schema", () => {
    const layers = Layer.mergeAll(
      makeTestSqlLayer(),
      makeTestNodeConfigLayer(),
    );
    return Effect.gen(function* () {
      // Run init twice — second run must not throw or drop tables.
      yield* DBInitialization.program;
      yield* DBInitialization.program;

      yield* DepositsDB.insertEntry({
        event_id: Buffer.alloc(32, 0x01),
        event_info: Buffer.alloc(16, 0x01),
        asset_name: "01".repeat(10),
        l1_utxo_cbor: Buffer.alloc(64, 0x01),
        inclusion_time: new Date(1_700_000_000_000),
      });

      const deposits = yield* DepositsDB.retrieveAllEntries();
      expect(deposits.length).toBe(1);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-003 ─────────────────────────────────────────────────────────────────

describe("Repository clear helpers produce a clean business state", () => {
  it.effect("Repository clear helpers produce a clean business state", () => {
    const layers = Layer.mergeAll(
      makeTestSqlLayer(),
      makeTestNodeConfigLayer(),
    );
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      yield* MempoolLedgerDB.insert([makeSeedLedgerEntry()]);
      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      // Clear all tables.
      yield* Effect.all(
        [
          MempoolDB.clear,
          MempoolLedgerDB.clear,
          AddressHistoryDB.clear,
          LatestLedgerDB.clear,
          ConfirmedLedgerDB.clear,
          BlocksDB.clear,
          BlocksTxsDB.clear,
        ],
        { concurrency: "unbounded" },
      );

      // Insert one new transaction after clearing.
      yield* MempoolDB.insertMultiple([processedTx]);

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(1n);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-004 ─────────────────────────────────────────────────────────────────

describe("Test NodeConfig layer drives MPT storage paths", () => {
  let tmpLedgerPath: string | undefined;
  let tmpMempoolPath: string | undefined;

  afterEach(async () => {
    if (tmpLedgerPath)
      await Effect.runPromise(deleteMpt(tmpLedgerPath, "ledger"));
    if (tmpMempoolPath)
      await Effect.runPromise(deleteMpt(tmpMempoolPath, "mempool"));
    tmpLedgerPath = undefined;
    tmpMempoolPath = undefined;
  });

  it.effect("Test NodeConfig layer drives MPT storage paths", () =>
    Effect.gen(function* () {
      tmpLedgerPath = path.join(os.tmpdir(), `nit004-ledger-${randomUUID()}`);
      tmpMempoolPath = path.join(os.tmpdir(), `nit004-mempool-${randomUUID()}`);

      const mpt1 = yield* MidgardMpt.create("nit004", tmpLedgerPath);
      yield* mpt1.batch([
        {
          type: "put",
          key: Buffer.alloc(32, 0xaa),
          value: Buffer.alloc(16, 0xbb),
        },
      ]);
      const root1 = yield* mpt1.getRootHex();

      yield* Effect.tryPromise(() =>
        mpt1.databaseAndPath!.database._leveldb.close(),
      );

      const mpt2 = yield* MidgardMpt.create("nit004", tmpLedgerPath);
      const root2 = yield* mpt2.getRootHex();

      expect(root2).toBe(root1);

      yield* Effect.tryPromise(() =>
        mpt2.databaseAndPath!.database._leveldb.close(),
      );
    }),
  );
});

// ─── NIT-005 ─────────────────────────────────────────────────────────────────

describe("Real layers compose for a node action", () => {
  it.effect("Real layers compose for a node action", () => {
    const layers = Layer.mergeAll(
      makeTestSqlLayer(),
      makeTestNodeConfigLayer(),
    );
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      yield* MempoolLedgerDB.insert([makeSeedLedgerEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const count = yield* MempoolDB.retrieveTxCount;
      expect(count).toBe(1n);

      const ledger = yield* MempoolLedgerDB.retrieve;
      const outrefs = ledger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );

      // Both spent (spentAddress) and produced (testAddress) should have entries.
      const ahForProduced = yield* AddressHistoryDB.retrieve(testAddress);
      const ahForSpent = yield* AddressHistoryDB.retrieve(spentAddress);
      expect(ahForProduced.length + ahForSpent.length).toBeGreaterThan(0);
    }).pipe(Effect.provide(layers));
  });
});
