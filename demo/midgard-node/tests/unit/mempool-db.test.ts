import { expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as Tx from "@/database/utils/tx.js";
import {
  COMMON_ADDRESSES,
  makeLedgerEntry,
  makeTxEntryNoTimeStamp,
} from "./harness/fixtures.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

vi.mock("@/database/addressHistory.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "address_history",
    Columns: {
      EVENT_ID: "event_id",
      ADDRESS: "address",
      EVENT_TYPE: "event_type",
      STATUS: "status",
    },
    Status: { SLATED: 0, SUBMITTED: 1, MERGED: 2 },
    EventType: { TX: 0, WITHDRAWAL: 1, DEPOSIT: 2 },
    createTable: vi.fn(() => E.succeed(undefined)),
    upsertEntries: vi.fn(() => E.succeed(undefined)),
    aggregateProcessedTxs: vi.fn(() =>
      E.succeed({
        allTxEntries: [],
        addressHistoryEntries: [],
        collectiveSpent: [],
        collectiveProduced: [],
      }),
    ),
    retrieve: vi.fn(() => E.succeed([])),
    clear: E.succeed(undefined),
  };
});

vi.mock("@/database/mempoolLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "mempool_ledger",
    insert: vi.fn(() => E.succeed(undefined)),
    retrieve: E.succeed([]),
    retrieveByAddress: vi.fn(() => E.succeed([])),
    retrieveByOutRefs: vi.fn(() => E.succeed([])),
    retrieveByOutRef: vi.fn(() => E.succeed(undefined)),
    clearUTxOs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});

vi.mock("@/database/utils/tx.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: { TX_ID: "tx_id", TX: "tx", TIMESTAMPTZ: "time_stamp_tz" },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    retrieveValue: vi.fn(() => E.succeed(undefined)),
    retrieveValues: vi.fn(() => E.succeed([])),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    delMultiple: vi.fn(() => E.succeed(undefined)),
  };
});

const txEntry = makeTxEntryNoTimeStamp(0xaa);
const txIdA = txEntry.tx_id;
const txCborA = txEntry.tx;
const outrefA = Buffer.alloc(32, 0xcc);
const testAddress = COMMON_ADDRESSES.produced;

const processedTx = {
  txId: txIdA,
  txCbor: txCborA,
  spent: [outrefA],
  produced: [makeLedgerEntry(0xaa, { tx_id: txIdA, outref: outrefA })],
};

const makeProcessedTx = (seed: number) => {
  const txEntrySeed = makeTxEntryNoTimeStamp(seed);
  const txId = txEntrySeed.tx_id;
  const outref = Buffer.alloc(32, seed + 0x40);
  return {
    txId,
    txCbor: txEntrySeed.tx,
    spent: [outref],
    produced: [makeLedgerEntry(seed, { tx_id: txId, outref })],
  };
};

const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
  vi.clearAllMocks();
  sqlHarness.setRows([{ tx_id: txIdA }]);
  vi.mocked(AddressHistoryDB.aggregateProcessedTxs).mockReturnValue(
    Effect.succeed({
      allTxEntries: [{ tx_id: txIdA, tx: txCborA }],
      addressHistoryEntries: [
        {
          event_id: txIdA,
          address: testAddress,
          event_type: 0,
          status: 0,
        },
      ],
      collectiveSpent: [outrefA],
      collectiveProduced: [processedTx.produced[0]],
    }),
  );
});

it.effect("insertMultiple stores tx and updates ledger", () =>
  MempoolDB.insertMultiple([processedTx]).pipe(
    Effect.map((insertedCount) => {
      expect(insertedCount).toBe(1);
      expect(
        vi.mocked(AddressHistoryDB.aggregateProcessedTxs),
      ).toHaveBeenCalled();
      expect(vi.mocked(Tx.insertEntries)).not.toHaveBeenCalled();
      expect(vi.mocked(AddressHistoryDB.upsertEntries)).toHaveBeenCalled();
      expect(vi.mocked(MempoolLedgerDB.insert)).toHaveBeenCalled();
      expect(vi.mocked(MempoolLedgerDB.clearUTxOs)).toHaveBeenCalled();
      expect(sqlHarness.getTransactionCallCount()).toBe(1);
    }),
    Effect.provide(sqlHarness.layer),
  ),
);

it.effect("insertMultiple aggregates address history as slated", () =>
  MempoolDB.insertMultiple([processedTx]).pipe(
    Effect.map(() => {
      expect(
        vi.mocked(AddressHistoryDB.aggregateProcessedTxs),
      ).toHaveBeenCalledWith("mempool_ledger", [processedTx], 0);
    }),
    Effect.provide(sqlHarness.layer),
  ),
);

it.effect("retrieveTimeBoundEntries queries mempool rows with effects", () => {
  const start = new Date("2024-01-01T00:00:00Z");
  const end = new Date("2024-01-02T00:00:00Z");
  sqlHarness.setRows([
    {
      tx_id: txIdA,
      tx: txCborA,
      time_stamp_tz: start,
      tx_size_bytes: txCborA.length,
      spent_outrefs: [outrefA],
      produced_outrefs: [outrefA],
      produced_outputs: [txCborA],
      produced_addresses: [testAddress],
    },
  ]);
  return MempoolDB.retrieveTimeBoundEntries(start, end).pipe(
    Effect.map((entries) => {
      expect(entries.length).toBe(1);
      expect(vi.mocked(Tx.retrieveTimeBoundEntries)).not.toHaveBeenCalled();
      expect(sqlHarness.getCallCount()).toBeGreaterThan(0);
    }),
    Effect.provide(sqlHarness.layer),
  );
});

it.effect("insertMultiple no-ops on duplicate mempool insert", () => {
  sqlHarness.setRows([]);
  return MempoolDB.insertMultiple([processedTx]).pipe(
    Effect.map((insertedCount) => {
      expect(insertedCount).toBe(0);
      expect(
        vi.mocked(AddressHistoryDB.aggregateProcessedTxs),
      ).not.toHaveBeenCalled();
      expect(vi.mocked(AddressHistoryDB.upsertEntries)).not.toHaveBeenCalled();
      expect(vi.mocked(MempoolLedgerDB.insert)).not.toHaveBeenCalled();
      expect(vi.mocked(MempoolLedgerDB.clearUTxOs)).not.toHaveBeenCalled();
    }),
    Effect.provide(sqlHarness.layer),
  );
});

it.effect("retrieveTxCount returns parsed bigint from SQL count row", () => {
  sqlHarness.setRows([{ count: "7" }]);
  return MempoolDB.retrieveTxCount.pipe(
    Effect.map((count) => {
      expect(count).toBe(7n);
    }),
    Effect.provide(sqlHarness.layer),
  );
});

it.effect("insertMultiple chunks projection writes into transactions", () => {
  const manyProcessedTxs = Array.from({ length: 101 }, (_, index) =>
    makeProcessedTx(index + 1),
  );
  sqlHarness.setRows(manyProcessedTxs.map((tx) => ({ tx_id: tx.txId })));

  vi.mocked(AddressHistoryDB.aggregateProcessedTxs).mockImplementation(
    (_referenceLedgerTableName, chunkProcessedTxs) =>
      Effect.succeed({
        allTxEntries: chunkProcessedTxs.map((tx) => ({
          tx_id: tx.txId,
          tx: tx.txCbor,
        })),
        addressHistoryEntries: [],
        collectiveSpent: [],
        collectiveProduced: [],
      }),
  );

  return MempoolDB.insertMultiple(manyProcessedTxs).pipe(
    Effect.map(() => {
      expect(sqlHarness.getTransactionCallCount()).toBe(2);
      expect(
        vi.mocked(AddressHistoryDB.aggregateProcessedTxs),
      ).toHaveBeenCalledTimes(101);
      const txBatchSizes = vi
        .mocked(AddressHistoryDB.aggregateProcessedTxs)
        .mock.calls.map((call) => call[1].length);
      expect(txBatchSizes.every((size) => size === 1)).toBe(true);
    }),
    Effect.provide(sqlHarness.layer),
  );
});
