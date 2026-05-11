/**
 * Unit tests for database/utils/user-events.ts and database/utils/tx.ts.
 *
 * These modules are NOT mocked here — tests exercise the real function bodies
 * using a lightweight in-process SQL mock that satisfies the SqlClient.SqlClient
 * interface (type Database = SqlClient.SqlClient).
 */
import { describe, expect, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Exit } from "effect";

import * as UserEvents from "@/database/utils/user-events.js";
import * as Tx from "@/database/utils/tx.js";
import {
  makeTxEntryNoTimeStamp,
  makeTxEntryWithTimeStamp,
  makeUserEventEntry,
} from "./harness/fixtures.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

// ---------------------------------------------------------------------------
// Shared SQL mock
// ---------------------------------------------------------------------------

const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
});

// ---------------------------------------------------------------------------
// UserEvents fixtures
// ---------------------------------------------------------------------------

const testEntry = makeUserEventEntry(0xaa);
const eventId = testEntry.event_id;

describe("UserEvents", () => {
  it.effect("createTable succeeds", () =>
    UserEvents.createTable("test_user_events").pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntry succeeds", () =>
    UserEvents.insertEntry("test_user_events", testEntry).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with entries succeeds", () =>
    UserEvents.insertEntries("test_user_events", [testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with empty array is a no-op", () =>
    UserEvents.insertEntries("test_user_events", []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("retrieveTimeBoundEntries returns rows from SQL", () => {
    sqlHarness.setRows([testEntry]);
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return UserEvents.retrieveTimeBoundEntries(
      "test_user_events",
      start,
      end,
    ).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveTimeBoundEntries returns empty when no rows", () => {
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return UserEvents.retrieveTimeBoundEntries(
      "test_user_events",
      start,
      end,
    ).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveAllEntries returns mock rows", () => {
    sqlHarness.setRows([testEntry, testEntry]);
    return UserEvents.retrieveAllEntries("test_user_events").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(2);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveAllEntries returns empty array when no rows", () =>
    UserEvents.retrieveAllEntries("test_user_events").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("delEntries succeeds", () =>
    UserEvents.delEntries("test_user_events", [eventId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("delEntries with empty ids list succeeds", () =>
    UserEvents.delEntries("test_user_events", []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx fixtures
// ---------------------------------------------------------------------------

const testTxEntry = makeTxEntryNoTimeStamp(0x11);
const txId = testTxEntry.tx_id;
const txCbor = testTxEntry.tx;

describe("Tx", () => {
  it.effect("createTable succeeds", () =>
    Tx.createTable("test_tx").pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntry succeeds", () =>
    Tx.insertEntry("test_tx", testTxEntry).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with entries succeeds", () =>
    Tx.insertEntries("test_tx", [testTxEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with empty array is a no-op", () =>
    Tx.insertEntries("test_tx", []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("delMultiple succeeds", () => {
    sqlHarness.setRows([{ tx_id: txId }]);
    return Tx.delMultiple("test_tx", [txId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveValue returns buffer when row found", () => {
    sqlHarness.setRows([{ tx: txCbor }]);
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.map((result) => {
        expect(result).toEqual(txCbor);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveValue fails when no row", () => {
    sqlHarness.setRows([]);
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(Exit.isFailure(exit)).toBe(true);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveValues returns all matching buffers", () => {
    sqlHarness.setRows([{ tx: txCbor }, { tx: Buffer.alloc(32, 0x33) }]);
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(2);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveValues returns empty array when no matches", () => {
    sqlHarness.setRows([]);
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveAllEntries returns rows", () => {
    const withTimestamp = makeTxEntryWithTimeStamp(0x22, {
      tx_id: txId,
      tx: txCbor,
      time_stamp_tz: new Date("2024-01-01T00:00:00Z"),
    });
    sqlHarness.setRows([withTimestamp]);
    return Tx.retrieveAllEntries("test_tx").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveTimeBoundEntries returns time-filtered rows", () => {
    sqlHarness.setRows([testTxEntry]);
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Tx.retrieveTimeBoundEntries("test_tx", start, end).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveTimeBoundEntries returns empty when no rows", () => {
    sqlHarness.setRows([]);
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Tx.retrieveTimeBoundEntries("test_tx", start, end).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});
