import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";

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

const testEntry: any = {
  tx_id: Buffer.alloc(32, 0xaa),
  outref: Buffer.alloc(32, 0xbb),
  output: Buffer.alloc(16, 0xcc),
  address: "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
};

const outrefA = Buffer.alloc(32, 0xbb);

const noopSqlLayer = Layer.succeed(
  SqlClient.SqlClient,
  {} as SqlClient.SqlClient,
);

beforeEach(() => {
  vi.clearAllMocks();
});

describe("MempoolLedgerDB insert stores entries", () => {
  it.effect("MempoolLedgerDB insert stores entries", () =>
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
});

describe("MempoolLedgerDB retrieve returns all entries", () => {
  it.effect("MempoolLedgerDB retrieve returns all entries", () =>
    MempoolLedgerDB.retrieve.pipe(
      Effect.map((entries) => {
        expect(Array.isArray(entries)).toBe(true);
      }),
      Effect.provide(noopSqlLayer),
    ),
  );
});

describe("MempoolLedgerDB retrieveByAddress filters by address", () => {
  it.effect("MempoolLedgerDB retrieveByAddress filters by address", () => {
    vi.mocked(Ledger.retrieveEntriesWithAddress).mockReturnValue(
      Effect.succeed([testEntry]),
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
});

describe("MempoolLedgerDB retrieveByOutRefs returns matching entries", () => {
  it.effect(
    "MempoolLedgerDB retrieveByOutRefs returns matching entries",
    () => {
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
    },
  );
});

describe("MempoolLedgerDB retrieveByOutRef returns single entry", () => {
  it.effect("MempoolLedgerDB retrieveByOutRef returns single entry", () => {
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
});

describe("MempoolLedgerDB clearUTxOs removes entries", () => {
  it.effect("MempoolLedgerDB clearUTxOs removes entries", () =>
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

describe("LatestLedgerDB insertMultiple stores entries", () => {
  it.effect("LatestLedgerDB insertMultiple stores entries", () =>
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
});

describe("LatestLedgerDB retrieveByOutRef returns entry", () => {
  it.effect("LatestLedgerDB retrieveByOutRef returns entry", () => {
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

describe("ConfirmedLedgerDB insertMultiple stores entries", () => {
  it.effect("ConfirmedLedgerDB insertMultiple stores entries", () =>
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
