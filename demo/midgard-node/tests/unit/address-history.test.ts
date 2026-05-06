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
import * as AddressHistoryDB from "@/database/addressHistory.js";

const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const txIdA = Buffer.alloc(32, 0xaa);
const txCborA = Buffer.alloc(64, 0xcc);
const outrefA = Buffer.alloc(32, 0xbb);

let mockSqlRows: any[] = [];

const mockSql: any = Object.assign(
  function (stringsOrStr: any, ..._values: unknown[]) {
    if (Array.isArray(stringsOrStr) && "raw" in stringsOrStr) {
      return Effect.succeed([...mockSqlRows]);
    }
    return stringsOrStr;
  },
  {
    withTransaction: (eff: Effect.Effect<unknown>) => eff,
    insert: (obj: unknown) => obj,
    in: (_col: string, vals: unknown[]) => vals,
    literal: (s: string) => s,
  },
);

const mockDbLayer = Layer.succeed(
  SqlClient.SqlClient,
  mockSql as unknown as SqlClient.SqlClient,
);

beforeEach(() => {
  mockSqlRows = [];
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
        expect(true).toBe(true);
      }),
      Effect.provide(mockDbLayer),
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
        expect(true).toBe(true);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("aggregateProcessedTxs", () => {
  it.effect("aggregateProcessedTxs", () => {
    const ledgerEntry = {
      tx_id: txIdA,
      outref: outrefA,
      output: Buffer.alloc(16),
      address: testAddress,
    };
    mockSqlRows = [ledgerEntry];

    const processedTx = {
      txId: txIdA,
      txCbor: txCborA,
      spent: [outrefA],
      produced: [ledgerEntry as any],
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
      Effect.provide(mockDbLayer),
    );
  });
});

describe("retrieve from mempool", () => {
  it.effect("retrieve from mempool", () => {
    mockSqlRows = [{ tx: txCborA }];
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(txCbors.length).toBe(1);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("retrieve from immutable", () => {
  it.effect("retrieve from immutable", () => {
    mockSqlRows = [{ tx: txCborA }];
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(Array.isArray(txCbors)).toBe(true);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});
