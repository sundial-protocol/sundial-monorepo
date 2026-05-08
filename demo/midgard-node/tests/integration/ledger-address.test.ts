// NIT-026 … NIT-035  — Ledger and Address Projection

import { expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import {
  txCborA,
  txIdA,
  inputCborBytes,
  outrefCborBytes,
  outputCborBytes,
  testAddress,
  spentAddress,
  makeSeedLedgerEntry as makeSeedEntry,
} from "./harness/fixtures.js";
import * as DBInitialization from "@/database/init.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as LatestLedgerDB from "@/database/latestLedger.js";
import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import * as ImmutableDB from "@/database/immutable.js";
import * as MempoolDB from "@/database/mempool.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";

const makeBaseLayers = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

// ─── NIT-026 ─────────────────────────────────────────────────────────────────

it.effect("Deposit event converts into ledger entry", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;
    yield* MempoolLedgerDB.insert([makeSeedEntry()]);

    const retrieved = yield* MempoolLedgerDB.retrieveByOutRef(inputCborBytes);

    expect(retrieved[Ledger.Columns.ADDRESS]).toBe(spentAddress);
    expect(Buffer.from(retrieved[Ledger.Columns.TX_ID]).equals(txIdA)).toBe(
      true,
    );
    expect(
      Buffer.from(retrieved[Ledger.Columns.OUTREF]).equals(inputCborBytes),
    ).toBe(true);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-027 ─────────────────────────────────────────────────────────────────

it.effect("Deposit event creates deposit address history", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    // Insert a tx so the JOIN in AddressHistoryDB.retrieve can find it.
    yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA });

    const ahEntry: AddressHistoryDB.Entry = {
      [AddressHistoryDB.Columns.EVENT_ID]: txIdA,
      [AddressHistoryDB.Columns.ADDRESS]: testAddress,
      [AddressHistoryDB.Columns.EVENT_TYPE]: AddressHistoryDB.EventType.DEPOSIT,
      [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SUBMITTED,
    };
    yield* AddressHistoryDB.upsertEntries([ahEntry]);

    const txCbors = yield* AddressHistoryDB.retrieve(testAddress);

    expect(txCbors.length).toBeGreaterThan(0);
    const found = txCbors.some((c) => Buffer.from(c).equals(txCborA));
    expect(found).toBe(true);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-028 ─────────────────────────────────────────────────────────────────

it.effect("Withdrawal event resolves against latest ledger", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    const ledgerEntry: Ledger.Entry = {
      [Ledger.Columns.TX_ID]: txIdA,
      [Ledger.Columns.OUTREF]: inputCborBytes,
      [Ledger.Columns.OUTPUT]: outputCborBytes,
      [Ledger.Columns.ADDRESS]: testAddress,
    };
    yield* LatestLedgerDB.insertMultiple([ledgerEntry]);

    const resolved = yield* LatestLedgerDB.retrieveByOutRef(inputCborBytes);

    expect(resolved[Ledger.Columns.ADDRESS]).toBe(testAddress);
    expect(
      Buffer.from(resolved[Ledger.Columns.OUTREF]).equals(inputCborBytes),
    ).toBe(true);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-029 ─────────────────────────────────────────────────────────────────

it.effect("Withdrawal event creates submitted address history", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    // Insert a tx so the JOIN in AddressHistoryDB.retrieve can find it.
    yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA });

    const ahEntry: AddressHistoryDB.Entry = {
      [AddressHistoryDB.Columns.EVENT_ID]: txIdA,
      [AddressHistoryDB.Columns.ADDRESS]: testAddress,
      [AddressHistoryDB.Columns.EVENT_TYPE]:
        AddressHistoryDB.EventType.WITHDRAWAL,
      [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SUBMITTED,
    };
    yield* AddressHistoryDB.upsertEntries([ahEntry]);

    const txCbors = yield* AddressHistoryDB.retrieve(testAddress);

    expect(txCbors.length).toBeGreaterThan(0);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-030 ─────────────────────────────────────────────────────────────────

it.effect("Processed tx aggregation uses real ledger lookup", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;
    yield* MempoolLedgerDB.insert([makeSeedEntry()]);

    const processedTx = yield* breakDownTx(txCborA);
    const agg = yield* AddressHistoryDB.aggregateProcessedTxs(
      MempoolLedgerDB.tableName,
      [processedTx],
      AddressHistoryDB.Status.SLATED,
    );

    expect(agg.allTxEntries.length).toBe(1);
    expect(agg.collectiveSpent.length).toBe(1);
    expect(agg.collectiveProduced.length).toBeGreaterThan(0);
    expect(agg.addressHistoryEntries.length).toBeGreaterThan(0);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-031 ─────────────────────────────────────────────────────────────────

it.effect("Mempool transaction lookup by address joins real tables", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;
    yield* MempoolLedgerDB.insert([makeSeedEntry()]);

    const processedTx = yield* breakDownTx(txCborA);
    yield* MempoolDB.insertMultiple([processedTx]);

    const txCbors = yield* AddressHistoryDB.retrieve(testAddress);

    expect(txCbors.length).toBeGreaterThan(0);
    const found = txCbors.some((c) => Buffer.from(c).equals(txCborA));
    expect(found).toBe(true);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-032 ─────────────────────────────────────────────────────────────────

it.effect("Immutable transaction lookup by address joins real tables", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA });

    const ahEntry: AddressHistoryDB.Entry = {
      [AddressHistoryDB.Columns.EVENT_ID]: txIdA,
      [AddressHistoryDB.Columns.ADDRESS]: testAddress,
      [AddressHistoryDB.Columns.EVENT_TYPE]: AddressHistoryDB.EventType.TX,
      [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SUBMITTED,
    };
    yield* AddressHistoryDB.upsertEntries([ahEntry]);

    const txCbors = yield* AddressHistoryDB.retrieve(testAddress);

    expect(txCbors.length).toBeGreaterThan(0);
    const found = txCbors.some((c) => Buffer.from(c).equals(txCborA));
    expect(found).toBe(true);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-033 ─────────────────────────────────────────────────────────────────

it.effect(
  "Latest ledger applies produced entries and removes spent entries",
  () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;
      yield* LatestLedgerDB.insertMultiple([makeSeedEntry()]);

      const processedTx = yield* breakDownTx(txCborA);
      yield* LatestLedgerDB.insertMultiple(processedTx.produced);
      yield* LatestLedgerDB.clearUTxOs(processedTx.spent);

      const afterLedger = yield* LatestLedgerDB.retrieve;
      const outrefs = afterLedger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );

      expect(outrefs.some((o) => o === inputCborBytes.toString("hex"))).toBe(
        false,
      );
      expect(outrefs.some((o) => o === outrefCborBytes.toString("hex"))).toBe(
        true,
      );
    }).pipe(Effect.provide(layers));
  },
);

// ─── NIT-034 ─────────────────────────────────────────────────────────────────

it.effect("Confirmed ledger can receive submitted-state entries", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    const entryA: Ledger.Entry = {
      [Ledger.Columns.TX_ID]: txIdA,
      [Ledger.Columns.OUTREF]: inputCborBytes,
      [Ledger.Columns.OUTPUT]: outputCborBytes,
      [Ledger.Columns.ADDRESS]: testAddress,
    };
    const entryB: Ledger.Entry = {
      [Ledger.Columns.TX_ID]: txIdA,
      [Ledger.Columns.OUTREF]: outrefCborBytes,
      [Ledger.Columns.OUTPUT]: outputCborBytes,
      [Ledger.Columns.ADDRESS]: testAddress,
    };
    yield* ConfirmedLedgerDB.insertMultiple([entryA, entryB]);

    const allEntries = yield* ConfirmedLedgerDB.retrieve;

    expect(allEntries.length).toBe(2);
  }).pipe(Effect.provide(layers));
});

// ─── NIT-035 ─────────────────────────────────────────────────────────────────

it.effect("Address history status upsert advances an event", () => {
  const layers = makeBaseLayers();
  return Effect.gen(function* () {
    yield* DBInitialization.program;

    // Insert a tx so the JOIN in retrieve() can return data.
    yield* ImmutableDB.insertTx({ tx_id: txIdA, tx: txCborA });

    const slated: AddressHistoryDB.Entry = {
      [AddressHistoryDB.Columns.EVENT_ID]: txIdA,
      [AddressHistoryDB.Columns.ADDRESS]: testAddress,
      [AddressHistoryDB.Columns.EVENT_TYPE]: AddressHistoryDB.EventType.TX,
      [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SLATED,
    };
    yield* AddressHistoryDB.upsertEntries([slated]);

    const submitted: AddressHistoryDB.Entry = {
      ...slated,
      [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SUBMITTED,
    };
    yield* AddressHistoryDB.upsertEntries([submitted]);

    // ON CONFLICT DO UPDATE SET produces exactly 1 row, not 2.
    const txCbors = yield* AddressHistoryDB.retrieve(testAddress);
    expect(txCbors.length).toBe(1);
  }).pipe(Effect.provide(layers));
});
