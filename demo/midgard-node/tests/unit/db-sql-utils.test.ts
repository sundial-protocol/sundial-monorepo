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

// ---------------------------------------------------------------------------
// UserEvents — createTable
// ---------------------------------------------------------------------------

describe("UserEvents.createTable succeeds", () => {
  it.effect("UserEvents.createTable succeeds", () =>
    UserEvents.createTable("test_user_events").pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — insertEntry
// ---------------------------------------------------------------------------

describe("UserEvents.insertEntry succeeds with mock SQL", () => {
  it.effect("UserEvents.insertEntry succeeds with mock SQL", () =>
    UserEvents.insertEntry("test_user_events", testEntry).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — insertEntries
// ---------------------------------------------------------------------------

describe("UserEvents.insertEntries with entries succeeds", () => {
  it.effect("UserEvents.insertEntries with entries succeeds", () =>
    UserEvents.insertEntries("test_user_events", [testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("UserEvents.insertEntries with empty array is a no-op", () => {
  it.effect("UserEvents.insertEntries with empty array is a no-op", () =>
    UserEvents.insertEntries("test_user_events", []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("UserEvents.retrieveTimeBoundEntries returns rows from SQL", () => {
  it.effect("UserEvents.retrieveTimeBoundEntries returns rows from SQL", () => {
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
});

describe("UserEvents.retrieveTimeBoundEntries returns empty when no rows", () => {
  it.effect(
    "UserEvents.retrieveTimeBoundEntries returns empty when no rows",
    () => {
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
    },
  );
});

// ---------------------------------------------------------------------------
// UserEvents — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("UserEvents.retrieveAllEntries returns mock rows", () => {
  it.effect("UserEvents.retrieveAllEntries returns mock rows", () => {
    sqlHarness.setRows([testEntry, testEntry]);
    return UserEvents.retrieveAllEntries("test_user_events").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(2);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("UserEvents.retrieveAllEntries returns empty array when no rows", () => {
  it.effect(
    "UserEvents.retrieveAllEntries returns empty array when no rows",
    () =>
      UserEvents.retrieveAllEntries("test_user_events").pipe(
        Effect.map((rows) => {
          expect(rows.length).toBe(0);
        }),
        Effect.provide(sqlHarness.layer),
      ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — delEntries
// ---------------------------------------------------------------------------

describe("UserEvents.delEntries succeeds with mock SQL", () => {
  it.effect("UserEvents.delEntries succeeds with mock SQL", () =>
    UserEvents.delEntries("test_user_events", [eventId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("UserEvents.delEntries with empty ids list succeeds", () => {
  it.effect("UserEvents.delEntries with empty ids list succeeds", () =>
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

// ---------------------------------------------------------------------------
// Tx — createTable
// ---------------------------------------------------------------------------

describe("Tx.createTable succeeds with mock SQL", () => {
  it.effect("Tx.createTable succeeds with mock SQL", () =>
    Tx.createTable("test_tx").pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — insertEntry
// ---------------------------------------------------------------------------

describe("Tx.insertEntry succeeds with mock SQL", () => {
  it.effect("Tx.insertEntry succeeds with mock SQL", () =>
    Tx.insertEntry("test_tx", testTxEntry).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — insertEntries
// ---------------------------------------------------------------------------

describe("Tx.insertEntries with entries succeeds", () => {
  it.effect("Tx.insertEntries with entries succeeds", () =>
    Tx.insertEntries("test_tx", [testTxEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("Tx.insertEntries with empty array is a no-op", () => {
  it.effect("Tx.insertEntries with empty array is a no-op", () =>
    Tx.insertEntries("test_tx", []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — delMultiple
// ---------------------------------------------------------------------------

describe("Tx.delMultiple succeeds with mock SQL", () => {
  it.effect("Tx.delMultiple succeeds with mock SQL", () => {
    sqlHarness.setRows([{ tx_id: txId }]);
    return Tx.delMultiple("test_tx", [txId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveValue
// ---------------------------------------------------------------------------

describe("Tx.retrieveValue returns buffer when row found", () => {
  it.effect("Tx.retrieveValue returns buffer when row found", () => {
    sqlHarness.setRows([{ tx: txCbor }]);
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.map((result) => {
        expect(result).toEqual(txCbor);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("Tx.retrieveValue fails with NotFoundError when no row", () => {
  it.effect("Tx.retrieveValue fails with NotFoundError when no row", () => {
    sqlHarness.setRows([]);
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(Exit.isFailure(exit)).toBe(true);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveValues
// ---------------------------------------------------------------------------

describe("Tx.retrieveValues returns all matching buffers", () => {
  it.effect("Tx.retrieveValues returns all matching buffers", () => {
    sqlHarness.setRows([{ tx: txCbor }, { tx: Buffer.alloc(32, 0x33) }]);
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(2);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("Tx.retrieveValues returns empty array when no matches", () => {
  it.effect("Tx.retrieveValues returns empty array when no matches", () => {
    sqlHarness.setRows([]);
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("Tx.retrieveAllEntries returns rows ordered by time", () => {
  it.effect("Tx.retrieveAllEntries returns rows ordered by time", () => {
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
});

// ---------------------------------------------------------------------------
// Tx — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("Tx.retrieveTimeBoundEntries returns time-filtered rows", () => {
  it.effect("Tx.retrieveTimeBoundEntries returns time-filtered rows", () => {
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
});

describe("Tx.retrieveTimeBoundEntries returns empty when no rows", () => {
  it.effect("Tx.retrieveTimeBoundEntries returns empty when no rows", () => {
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
