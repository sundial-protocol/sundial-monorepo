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

import * as DatabaseIndex from "@/database/index.js";
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

describe("AddressHistoryDB", () => {
  it.effect("upsertEntries inserts a new entry", () => {
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

  it.effect("upsertEntries updates status on second call", () => {
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

  it.effect("aggregateProcessedTxs returns grouped entries", () => {
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

  it.effect("aggregateProcessedTxs batches spent outref lookup", () => {
    const spentOutRefCount = 2505;
    const spentOutRefs = Array.from(
      { length: spentOutRefCount },
      (_, index) => {
        const outRef = Buffer.alloc(32);
        outRef.writeUInt32BE(index, 0);
        return outRef;
      },
    );
    const processedTxs = spentOutRefs.map((spentOutRef, index) => {
      const txId = Buffer.alloc(32);
      txId.writeUInt32BE(index + 1, 0);
      const producedEntry = makeLedgerEntry((index % 250) + 1, {
        tx_id: txId,
        outref: Buffer.from(txId),
        address: testAddress,
      });
      return {
        txId,
        txCbor: Buffer.alloc(64, (index % 251) + 1),
        spent: [spentOutRef],
        produced: [producedEntry],
      };
    });

    const retrieveByOutRefsSpy = vi
      .spyOn(DatabaseIndex.Ledger, "retrieveByOutRefs")
      .mockImplementation((_tableName, outRefs) =>
        Effect.succeed(
          outRefs.map((outRef, index) =>
            makeLedgerEntry((index % 250) + 1, {
              outref: outRef,
              tx_id: Buffer.alloc(32, 0xdd),
              address: COMMON_ADDRESSES.spent,
            }),
          ),
        ),
      );

    return AddressHistoryDB.aggregateProcessedTxs(
      "mempool_ledger",
      processedTxs,
      AddressHistoryDB.Status.SLATED,
    ).pipe(
      Effect.map((result) => {
        expect(result.collectiveSpent).toHaveLength(spentOutRefCount);
        expect(result.allTxEntries).toHaveLength(spentOutRefCount);
        expect(result.addressHistoryEntries.length).toBeGreaterThanOrEqual(
          spentOutRefCount,
        );

        const retrieveByOutRefsCalls = retrieveByOutRefsSpy.mock.calls;
        expect(retrieveByOutRefsCalls).toHaveLength(3);
        expect(
          retrieveByOutRefsCalls.every(
            ([tableName, outRefs]) =>
              tableName === "mempool_ledger" && outRefs.length <= 1000,
          ),
        ).toBe(true);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect(
    "aggregateProcessedTxs resolves spent addresses by outref, not SQL row order",
    () => {
      const txIdFirst = Buffer.alloc(32, 0x11);
      const txIdSecond = Buffer.alloc(32, 0x22);
      const spentOutRefFirst = Buffer.alloc(32, 0xa1);
      const spentOutRefSecond = Buffer.alloc(32, 0xb2);
      const firstSpentAddress = COMMON_ADDRESSES.spent;
      const secondSpentAddress = testAddress;

      const firstLedgerEntry = makeLedgerEntry(0x30, {
        outref: spentOutRefFirst,
        address: firstSpentAddress,
      });
      const secondLedgerEntry = makeLedgerEntry(0x40, {
        outref: spentOutRefSecond,
        address: secondSpentAddress,
      });

      // Intentionally reverse row order to validate deterministic by-outref
      // attribution.
      const retrieveByOutRefsSpy = vi
        .spyOn(DatabaseIndex.Ledger, "retrieveByOutRefs")
        .mockReturnValue(Effect.succeed([secondLedgerEntry, firstLedgerEntry]));

      const processedTxs = [
        {
          txId: txIdFirst,
          txCbor: Buffer.alloc(64, 0x11),
          spent: [spentOutRefFirst],
          produced: [],
        },
        {
          txId: txIdSecond,
          txCbor: Buffer.alloc(64, 0x22),
          spent: [spentOutRefSecond],
          produced: [],
        },
      ];

      return AddressHistoryDB.aggregateProcessedTxs(
        "mempool_ledger",
        processedTxs,
        AddressHistoryDB.Status.SLATED,
      ).pipe(
        Effect.map((result) => {
          expect(retrieveByOutRefsSpy).toHaveBeenCalledTimes(1);
          expect(result.addressHistoryEntries).toEqual([
            {
              event_id: txIdFirst,
              address: firstSpentAddress,
              event_type: AddressHistoryDB.EventType.TX,
              status: AddressHistoryDB.Status.SLATED,
            },
            {
              event_id: txIdSecond,
              address: secondSpentAddress,
              event_type: AddressHistoryDB.EventType.TX,
              status: AddressHistoryDB.Status.SLATED,
            },
          ]);
        }),
        Effect.provide(sqlHarness.layer),
      );
    },
  );

  it.effect(
    "aggregateProcessedTxs skips address history entry when a spent outref is missing",
    () => {
      const missingOutRef = Buffer.alloc(32, 0xee);
      const txId = Buffer.alloc(32, 0x77);

      vi.spyOn(DatabaseIndex.Ledger, "retrieveByOutRefs").mockReturnValue(
        Effect.succeed([]),
      );

      return AddressHistoryDB.aggregateProcessedTxs(
        "mempool_ledger",
        [
          {
            txId,
            txCbor: Buffer.alloc(64, 0x33),
            spent: [missingOutRef],
            produced: [],
          },
        ],
        AddressHistoryDB.Status.SLATED,
      ).pipe(
        Effect.map((result) => {
          expect(result.addressHistoryEntries).toHaveLength(0);
          expect(result.collectiveSpent).toHaveLength(1);
        }),
        Effect.provide(sqlHarness.layer),
      );
    },
  );

  it.effect("upsertEntries skips SQL for an empty entry list", () => {
    return AddressHistoryDB.upsertEntries([]).pipe(
      Effect.map(() => {
        expect(sqlHarness.getCallCount()).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieve returns one tx when one row present", () => {
    sqlHarness.setRows([{ tx: txCborA }]);
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(txCbors.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieve returns correct cbor buffer", () => {
    sqlHarness.setRows([{ tx: txCborA }]);
    return AddressHistoryDB.retrieve(testAddress).pipe(
      Effect.map((txCbors) => {
        expect(txCbors).toHaveLength(1);
        expect(txCbors[0]).toEqual(txCborA);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  // The mock SQL harness replaces every interpolated value (including
  // `sql.literal(...)` values) with a `?` placeholder in `template`, and
  // records the actual substituted values, in order, in `values`. So the
  // query shape is asserted via `template`, and the effective LIMIT/OFFSET
  // via the trailing two entries of `values`.
  const getLimitOffsetCall = () =>
    sqlHarness.getCalls().find((c) => c.template.includes("LIMIT ? OFFSET"));

  it.effect(
    "retrieve defaults to a bounded LIMIT/OFFSET when none given",
    () => {
      sqlHarness.setRows([{ tx: txCborA }]);
      return AddressHistoryDB.retrieve(testAddress).pipe(
        Effect.map(() => {
          const call = getLimitOffsetCall();
          expect(call).toBeDefined();
          expect(call?.values.slice(-2)).toEqual([
            String(AddressHistoryDB.DEFAULT_ADDRESS_HISTORY_LIMIT),
            "0",
          ]);
        }),
        Effect.provide(sqlHarness.layer),
      );
    },
  );

  it.effect("retrieve forwards a caller-supplied limit and offset", () => {
    sqlHarness.setRows([{ tx: txCborA }]);
    return AddressHistoryDB.retrieve(testAddress, {
      limit: 25,
      offset: 50,
    }).pipe(
      Effect.map(() => {
        const call = getLimitOffsetCall();
        expect(call?.values.slice(-2)).toEqual(["25", "50"]);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect(
    "retrieve clamps an out-of-range limit/offset instead of running an unbounded query",
    () => {
      sqlHarness.setRows([{ tx: txCborA }]);
      return AddressHistoryDB.retrieve(testAddress, {
        limit: 10_000_000,
        offset: -5,
      }).pipe(
        Effect.map(() => {
          const call = getLimitOffsetCall();
          expect(call?.values.slice(-2)).toEqual([
            String(AddressHistoryDB.MAX_ADDRESS_HISTORY_LIMIT),
            "0",
          ]);
        }),
        Effect.provide(sqlHarness.layer),
      );
    },
  );
});
