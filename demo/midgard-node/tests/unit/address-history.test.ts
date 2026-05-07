import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

vi.mock("@/database/utils/ledger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: {
      TX_ID: "tx_id",
      OUTREF: "outref",
      OUTPUT: "output",
      ADDRESS: "address",
      TIMESTAMPTZ: "time_stamp_tz",
    },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    retrieveAllEntriesNoTimeStamps: vi.fn(() => E.succeed([])),
    retrieveByOutRef: vi.fn(() => E.succeed({})),
    retrieveByOutRefs: vi.fn(() => E.succeed([])),
    retrieveEntriesWithAddress: vi.fn(() => E.succeed([])),
    delEntries: vi.fn(() => E.succeed(undefined)),
    removeSpentOutRef: vi.fn(() => E.succeed([])),
    applyTx: vi.fn(() => E.succeed([])),
  };
});

import * as Ledger from "@/database/utils/ledger.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import { COMMON_ADDRESSES, makeLedgerEntry } from "./harness/fixtures.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const testAddress = COMMON_ADDRESSES.produced;
const txIdA = Buffer.alloc(32, 0xaa);
const txCborA = Buffer.alloc(64, 0xcc);
const outrefA = Buffer.alloc(32, 0xbb);
const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
  vi.clearAllMocks();
});

describe("upsertEntries", () => {
  it.effect("upsertEntries", () => {
    const entries: AddressHistoryDB.Entry[] = [
      {
        event_id: txIdA,
        address: testAddress,
        event_type: AddressHistoryDB.EventType.TX,
        status: AddressHistoryDB.Status.SLATED,
      },
    ];
    return AddressHistoryDB.upsertEntries(entries).pipe(
      Effect.map(() => {
        expect(sqlHarness.getCallCount()).toBeGreaterThan(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("upsert updates status", () => {
  it.effect("upsert updates status", () => {
    const entry: AddressHistoryDB.Entry = {
      event_id: txIdA,
      address: testAddress,
      event_type: AddressHistoryDB.EventType.TX,
      status: AddressHistoryDB.Status.SUBMITTED,
    };
    return AddressHistoryDB.upsertEntries([entry]).pipe(
      Effect.andThen(() =>
        AddressHistoryDB.upsertEntries([
          { ...entry, status: AddressHistoryDB.Status.MERGED },
        ]),
      ),
      Effect.map(() => {
        expect(sqlHarness.getCallCount()).toBeGreaterThan(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("aggregateProcessedTxs", () => {
  it.effect("aggregateProcessedTxs", () => {
    const ledgerEntry = makeLedgerEntry(0xbb, {
      tx_id: txIdA,
      outref: outrefA,
      address: testAddress,
    });
    sqlHarness.setRows([ledgerEntry]);

    const processedTx = {
      txId: txIdA,
      txCbor: txCborA,
      spent: [outrefA],
      produced: [ledgerEntry],
    };

    return AddressHistoryDB.aggregateProcessedTxs(
      "mempool_ledger",
      [processedTx],
      AddressHistoryDB.Status.SLATED,
    ).pipe(
      Effect.map((result) => {
        expect(result.allTxEntries.length).toBe(1);
        expect(result.collectiveSpent.length).toBe(1);
        expect(result.collectiveProduced.length).toBe(1);
        expect(result.addressHistoryEntries.length).toBeGreaterThan(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("retrieve from mempool", () => {
  it.effect("retrieve from mempool", () => {
    sqlHarness.setRows([{ tx: txCborA }]);
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(txCbors.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("retrieve from immutable", () => {
  it.effect("retrieve from immutable", () => {
    sqlHarness.setRows([{ tx: txCborA }]);
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(txCbors).toHaveLength(1);
        expect(txCbors[0]).toEqual(txCborA);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});
