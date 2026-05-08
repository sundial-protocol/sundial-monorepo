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
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as LatestLedgerDB from "@/database/latestLedger.js";
import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import { makeLedgerEntry } from "./harness/fixtures.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const testEntry = makeLedgerEntry(0xaa);
const testEntryWithTimeStamp = {
  ...testEntry,
  [Ledger.Columns.TIMESTAMPTZ]: new Date(),
};

const outrefA = Buffer.alloc(32, 0xbb);

const noopSqlLayer = createMockSqlHarness().layer;

beforeEach(() => {
  vi.clearAllMocks();
});

describe("MempoolLedgerDB", () => {
  it.effect("insert stores entries", () =>
    MempoolLedgerDB.insert([testEntry]).pipe(
      Effect.map(() => {
        expect(vi.mocked(Ledger.insertEntries)).toHaveBeenCalledWith(
          "mempool_ledger",
          [testEntry],
        );
      }),
      Effect.provide(noopSqlLayer),
    ),
  );

  it.effect("retrieve returns all entries", () =>
    MempoolLedgerDB.retrieve.pipe(
      Effect.map((entries) => {
        expect(entries).toHaveLength(0);
      }),
      Effect.provide(noopSqlLayer),
    ),
  );

  it.effect("retrieveByAddress filters by address", () => {
    vi.mocked(Ledger.retrieveEntriesWithAddress).mockReturnValue(
      Effect.succeed([testEntryWithTimeStamp]),
    );
    return MempoolLedgerDB.retrieveByAddress(testEntry.address).pipe(
      Effect.map((entries) => {
        expect(entries.length).toBe(1);
        expect(
          vi.mocked(Ledger.retrieveEntriesWithAddress),
        ).toHaveBeenCalledWith("mempool_ledger", testEntry.address);
      }),
      Effect.provide(noopSqlLayer),
    );
  });

  it.effect("retrieveByOutRefs returns matching entries", () => {
    vi.mocked(Ledger.retrieveByOutRefs).mockReturnValue(
      Effect.succeed([testEntry]),
    );
    return MempoolLedgerDB.retrieveByOutRefs([outrefA]).pipe(
      Effect.map((entries) => {
        expect(entries.length).toBe(1);
        expect(vi.mocked(Ledger.retrieveByOutRefs)).toHaveBeenCalledWith(
          "mempool_ledger",
          [outrefA],
        );
      }),
      Effect.provide(noopSqlLayer),
    );
  });

  it.effect("retrieveByOutRef returns single entry", () => {
    vi.mocked(Ledger.retrieveByOutRef).mockReturnValue(
      Effect.succeed(testEntry),
    );
    return MempoolLedgerDB.retrieveByOutRef(outrefA).pipe(
      Effect.map((entry) => {
        expect(entry).toEqual(testEntry);
        expect(vi.mocked(Ledger.retrieveByOutRef)).toHaveBeenCalledWith(
          "mempool_ledger",
          outrefA,
        );
      }),
      Effect.provide(noopSqlLayer),
    );
  });

  it.effect("clearUTxOs removes entries", () =>
    MempoolLedgerDB.clearUTxOs([outrefA]).pipe(
      Effect.map(() => {
        expect(vi.mocked(Ledger.delEntries)).toHaveBeenCalledWith(
          "mempool_ledger",
          [outrefA],
        );
      }),
      Effect.provide(noopSqlLayer),
    ),
  );
});

describe("LatestLedgerDB", () => {
  it.effect("insertMultiple stores entries", () =>
    LatestLedgerDB.insertMultiple([testEntry]).pipe(
      Effect.map(() => {
        expect(vi.mocked(Ledger.insertEntries)).toHaveBeenCalledWith(
          "latest_ledger",
          [testEntry],
        );
      }),
      Effect.provide(noopSqlLayer),
    ),
  );

  it.effect("retrieveByOutRef returns entry", () => {
    vi.mocked(Ledger.retrieveByOutRef).mockReturnValue(
      Effect.succeed(testEntry),
    );
    return LatestLedgerDB.retrieveByOutRef(outrefA).pipe(
      Effect.map((entry) => {
        expect(entry).toEqual(testEntry);
        expect(vi.mocked(Ledger.retrieveByOutRef)).toHaveBeenCalledWith(
          "latest_ledger",
          outrefA,
        );
      }),
      Effect.provide(noopSqlLayer),
    );
  });
});

describe("ConfirmedLedgerDB", () => {
  it.effect("insertMultiple stores entries", () =>
    ConfirmedLedgerDB.insertMultiple([testEntry]).pipe(
      Effect.map(() => {
        expect(vi.mocked(Ledger.insertEntries)).toHaveBeenCalledWith(
          "confirmed_ledger",
          [testEntry],
        );
      }),
      Effect.provide(noopSqlLayer),
    ),
  );
});
