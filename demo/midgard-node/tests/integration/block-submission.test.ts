// NIT-045 … NIT-050  — Block and Submission Flow

import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Option } from "effect";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as BlocksDB from "@/database/blocks.js";
import * as BlocksTxsDB from "@/database/blocksTxs.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as ImmutableDB from "@/database/immutable.js";
import * as LatestLedgerDB from "@/database/latestLedger.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as DepositsDB from "@/database/deposits.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import * as TxOrdersDB from "@/database/txOrders.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import * as UserEvents from "@/database/utils/user-events.js";
import { breakDownTx } from "@/utils.js";

// Deterministic fixtures from the lucid stub.
const txCborA = Buffer.alloc(64, 0xbb);
const txIdA = Buffer.alloc(32, 0xaa);
const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]);
const outputCborBytes = Buffer.alloc(16, 0xcc);
const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const spentAddress =
  "addr_test1vz0p8k0ekk5xvms5jlqmajgddmqm4xp58yd8c92lvd63hwcv6znrl";

// Placeholder UTxOs buffer — equivalent to serializeUTxOsForStorage([]).
const emptyUtxosBuf = Buffer.from("[]");

const makeSeedEntry = (): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: txIdA,
  [Ledger.Columns.OUTREF]: inputCborBytes,
  [Ledger.Columns.OUTPUT]: outputCborBytes,
  [Ledger.Columns.ADDRESS]: spentAddress,
});

const makeBlockEntry = (
  headerHash: Buffer,
  startTime: Date,
  endTime: Date,
  status = BlocksDB.Status.UNSUBMITTED,
): BlocksDB.EntryNoMeta => ({
  [BlocksDB.Columns.HEADER_HASH]: headerHash,
  [BlocksDB.Columns.EVENT_START_TIME]: startTime,
  [BlocksDB.Columns.EVENT_END_TIME]: endTime,
  [BlocksDB.Columns.NEW_WALLET_UTXOS]: emptyUtxosBuf,
  [BlocksDB.Columns.PRODUCED_UTXOS]: emptyUtxosBuf,
  [BlocksDB.Columns.L1_CBOR]: Buffer.alloc(64, 0xee),
  [BlocksDB.Columns.DEPOSITS_COUNT]: 0,
  [BlocksDB.Columns.TX_REQUESTS_COUNT]: 0,
  [BlocksDB.Columns.TX_ORDERS_COUNT]: 0,
  [BlocksDB.Columns.WITHDRAWALS_COUNT]: 0,
  [BlocksDB.Columns.TOTAL_EVENTS_SIZE]: 0,
  [BlocksDB.Columns.STATUS]: status,
});

const makeUserEventEntry = (seed: number): UserEvents.Entry => ({
  [UserEvents.Columns.ID]: Buffer.alloc(32, seed),
  [UserEvents.Columns.INFO]: Buffer.alloc(16, seed),
  [UserEvents.Columns.ASSET_NAME]: seed
    .toString(16)
    .padStart(2, "0")
    .repeat(10),
  [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, seed),
  [UserEvents.Columns.INCLUSION_TIME]: new Date(),
});

const makeBaseLayers = () =>
  Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());

// ─── NIT-045 ─────────────────────────────────────────────────────────────────

describe("BlocksDB retrieves combined event interval", () => {
  it.effect("BlocksDB retrieves combined event interval", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const start = new Date(0);
      const end = new Date(Date.now() + 60_000);

      const depositEntry = makeUserEventEntry(0x01);
      const withdrawalEntry = makeUserEventEntry(0x02);
      const txOrderEntry = makeUserEventEntry(0x03);

      yield* DepositsDB.insertEntry(depositEntry);
      yield* WithdrawalsDB.insertEntries([withdrawalEntry]);
      yield* TxOrdersDB.insertEntries([txOrderEntry]);

      yield* MempoolLedgerDB.insert([makeSeedEntry()]);
      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const events = yield* BlocksDB.retrieveEvents(start, end);

      expect(events.deposits.length).toBe(1);
      expect(events.withdrawals.length).toBe(1);
      expect(events.txOrders.length).toBe(1);
      expect(events.txRequests.length).toBe(1);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-046 ─────────────────────────────────────────────────────────────────

describe("BlocksDB earliest unsubmitted block follows height order", () => {
  it.effect("BlocksDB earliest unsubmitted block follows height order", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = new Date(1_700_000_000_000);
      const T1 = new Date(1_700_000_001_000);
      const T2 = new Date(1_700_000_002_000);

      const hashA = Buffer.alloc(32, 0x01);
      const hashB = Buffer.alloc(32, 0x02);

      yield* BlocksDB.upsert(makeBlockEntry(hashA, T0, T1));
      yield* BlocksDB.upsert(makeBlockEntry(hashB, T1, T2));

      const earliest = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;

      expect(Option.isSome(earliest)).toBe(true);
      expect(
        Buffer.from(
          Option.getOrThrow(earliest)[BlocksDB.Columns.HEADER_HASH],
        ).equals(hashA),
      ).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-047 ─────────────────────────────────────────────────────────────────

describe("BlocksDB latest entry follows newest height", () => {
  it.effect("BlocksDB latest entry follows newest height", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = new Date(1_700_000_000_000);
      const T1 = new Date(1_700_000_001_000);
      const T2 = new Date(1_700_000_002_000);

      const hashA = Buffer.alloc(32, 0x01);
      const hashB = Buffer.alloc(32, 0x02);

      yield* BlocksDB.upsert(makeBlockEntry(hashA, T0, T1));
      yield* BlocksDB.upsert(makeBlockEntry(hashB, T1, T2));

      const latest = yield* BlocksDB.retrieveLatestEntry;

      expect(Option.isSome(latest)).toBe(true);
      expect(
        Buffer.from(
          Option.getOrThrow(latest)[BlocksDB.Columns.HEADER_HASH],
        ).equals(hashB),
      ).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-048 ─────────────────────────────────────────────────────────────────

describe("Submitted block status is persisted", () => {
  it.effect("Submitted block status is persisted", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = new Date(1_700_000_000_000);
      const T1 = new Date(1_700_000_001_000);
      const hashA = Buffer.alloc(32, 0x01);

      yield* BlocksDB.upsert(makeBlockEntry(hashA, T0, T1));

      const unsubmitted = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;
      expect(Option.isSome(unsubmitted)).toBe(true);

      const blockEntry = Option.getOrThrow(unsubmitted);
      yield* BlocksDB.setStatusOfEntry(blockEntry, BlocksDB.Status.SUBMITTED);

      const afterUnsubmitted = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;
      expect(Option.isNone(afterUnsubmitted)).toBe(true);

      const allBlocks = yield* BlocksDB.retrieve;
      expect(allBlocks[0][BlocksDB.Columns.STATUS]).toBe(
        BlocksDB.Status.SUBMITTED,
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-049 ─────────────────────────────────────────────────────────────────

describe("Mempool transactions transfer to immutable block history", () => {
  it.effect("Mempool transactions transfer to immutable block history", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      yield* MempoolLedgerDB.insert([makeSeedEntry()]);
      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const T0 = new Date(0);
      const T1 = new Date(Date.now() + 60_000);
      const hashA = Buffer.alloc(32, 0x01);
      yield* BlocksDB.upsert(makeBlockEntry(hashA, T0, T1));

      // Transfer: read mempool entries in interval → immutable + block mapping.
      const txEntries = yield* MempoolDB.retrieveTimeBoundEntries(T0, T1);
      expect(txEntries.length).toBe(1);

      const txPairs: Tx.EntryNoTimeStamp[] = txEntries.map((e) => ({
        [Tx.Columns.TX_ID]: e[Tx.Columns.TX_ID],
        [Tx.Columns.TX]: e[Tx.Columns.TX],
      }));
      yield* ImmutableDB.insertTxs(txPairs);
      yield* BlocksTxsDB.insert(
        hashA,
        txPairs.map((e) => e[Tx.Columns.TX_ID]),
      );
      yield* MempoolDB.clearTxs([processedTx.txId]);

      const mempoolCount = yield* MempoolDB.retrieveTxCount;
      const immutableCbor = yield* ImmutableDB.retrieveTxCborByHash(
        processedTx.txId,
      );
      const blockTxHashes =
        yield* BlocksTxsDB.retrieveTxHashesByHeaderHash(hashA);

      expect(mempoolCount).toBe(0n);
      expect(immutableCbor).not.toBeUndefined();
      expect(Buffer.from(immutableCbor!).equals(txCborA)).toBe(true);
      expect(blockTxHashes.length).toBe(1);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-050 ─────────────────────────────────────────────────────────────────

describe("Submitted block updates latest ledger and address history", () => {
  it.effect("Submitted block updates latest ledger and address history", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      // Seed: latest ledger has the entry that txCborA will spend.
      yield* LatestLedgerDB.insertMultiple([makeSeedEntry()]);

      // Seed: one mempool tx.
      yield* MempoolLedgerDB.insert([makeSeedEntry()]);
      const processedTx = yield* breakDownTx(txCborA);
      yield* MempoolDB.insertMultiple([processedTx]);

      const T0 = new Date(0);
      const T1 = new Date(Date.now() + 60_000);
      const hashA = Buffer.alloc(32, 0x01);
      yield* BlocksDB.upsert(makeBlockEntry(hashA, T0, T1));

      // Step 1: Transfer mempool → immutable.
      const txEntries = yield* MempoolDB.retrieveTimeBoundEntries(T0, T1);
      const txPairs: Tx.EntryNoTimeStamp[] = txEntries.map((e) => ({
        [Tx.Columns.TX_ID]: e[Tx.Columns.TX_ID],
        [Tx.Columns.TX]: e[Tx.Columns.TX],
      }));
      yield* ImmutableDB.insertTxs(txPairs);
      yield* MempoolDB.clearTxs([processedTx.txId]);

      // Step 2: Apply latest ledger updates.
      yield* LatestLedgerDB.insertMultiple(processedTx.produced);
      yield* LatestLedgerDB.clearUTxOs(processedTx.spent);

      // Step 3: Upsert submitted address history for the tx.
      const ahEntry: AddressHistoryDB.Entry = {
        [AddressHistoryDB.Columns.EVENT_ID]: processedTx.txId,
        [AddressHistoryDB.Columns.ADDRESS]: testAddress,
        [AddressHistoryDB.Columns.EVENT_TYPE]: AddressHistoryDB.EventType.TX,
        [AddressHistoryDB.Columns.STATUS]: AddressHistoryDB.Status.SUBMITTED,
      };
      yield* AddressHistoryDB.upsertEntries([ahEntry]);

      // Step 4: Set block status to SUBMITTED.
      const unsubmitted = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;
      yield* BlocksDB.setStatusOfEntry(
        Option.getOrThrow(unsubmitted),
        BlocksDB.Status.SUBMITTED,
      );

      // Assert final state.
      const allBlocks = yield* BlocksDB.retrieve;
      expect(allBlocks[0][BlocksDB.Columns.STATUS]).toBe(
        BlocksDB.Status.SUBMITTED,
      );

      const latestLedger = yield* LatestLedgerDB.retrieve;
      const latestOutrefs = latestLedger.map((e) =>
        Buffer.from(e[Ledger.Columns.OUTREF]).toString("hex"),
      );
      expect(
        latestOutrefs.some((o) => o === inputCborBytes.toString("hex")),
      ).toBe(false);
      expect(
        latestOutrefs.some((o) => o === outrefCborBytes.toString("hex")),
      ).toBe(true);

      const ahForAddress = yield* AddressHistoryDB.retrieve(testAddress);
      expect(ahForAddress.length).toBeGreaterThan(0);
    }).pipe(Effect.provide(layers));
  });
});
