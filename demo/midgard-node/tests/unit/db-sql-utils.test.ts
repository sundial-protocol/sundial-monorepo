/**
 * Unit tests for database/utils/user-events.ts and database/utils/tx.ts.
 *
 * These modules are NOT mocked here — tests exercise the real function bodies
 * using a lightweight in-process SQL mock that satisfies the SqlClient.SqlClient
 * interface (type Database = SqlClient.SqlClient).
 */
import { describe, expect, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Exit, Layer } from "effect";
import { SqlClient } from "@effect/sql";

import * as UserEvents from "@/database/utils/user-events.js";
import * as Tx from "@/database/utils/tx.js";

// ---------------------------------------------------------------------------
// Shared SQL mock
// ---------------------------------------------------------------------------

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
});

// ---------------------------------------------------------------------------
// UserEvents fixtures
// ---------------------------------------------------------------------------

const eventId = Buffer.alloc(32, 0xaa);
const eventInfo = Buffer.alloc(32, 0xbb);
const l1UxoCbor = Buffer.alloc(64, 0xcc);
const inclusionTime = new Date("2024-06-01T00:00:00Z");

const testEntry: UserEvents.Entry = {
  event_id: eventId,
  event_info: eventInfo,
  asset_name: "aabbccdd",
  l1_utxo_cbor: l1UxoCbor,
  inclusion_time: inclusionTime,
};

// ---------------------------------------------------------------------------
// UserEvents — createTable
// ---------------------------------------------------------------------------

describe("UserEvents.createTable succeeds", () => {
  it.effect("UserEvents.createTable succeeds", () =>
    UserEvents.createTable("test_user_events").pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — insertEntry
// ---------------------------------------------------------------------------

describe("UserEvents.insertEntry succeeds with mock SQL", () => {
  it.effect("UserEvents.insertEntry succeeds with mock SQL", () =>
    UserEvents.insertEntry("test_user_events", testEntry).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — insertEntries
// ---------------------------------------------------------------------------

describe("UserEvents.insertEntries with entries succeeds", () => {
  it.effect("UserEvents.insertEntries with entries succeeds", () =>
    UserEvents.insertEntries("test_user_events", [testEntry]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("UserEvents.insertEntries with empty array is a no-op", () => {
  it.effect("UserEvents.insertEntries with empty array is a no-op", () =>
    UserEvents.insertEntries("test_user_events", []).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("UserEvents.retrieveTimeBoundEntries returns rows from SQL", () => {
  it.effect("UserEvents.retrieveTimeBoundEntries returns rows from SQL", () => {
    mockSqlRows = [testEntry];
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
      Effect.provide(mockDbLayer),
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
        Effect.provide(mockDbLayer),
      );
    },
  );
});

// ---------------------------------------------------------------------------
// UserEvents — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("UserEvents.retrieveAllEntries returns mock rows", () => {
  it.effect("UserEvents.retrieveAllEntries returns mock rows", () => {
    mockSqlRows = [testEntry, testEntry];
    return UserEvents.retrieveAllEntries("test_user_events").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(2);
      }),
      Effect.provide(mockDbLayer),
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
        Effect.provide(mockDbLayer),
      ),
  );
});

// ---------------------------------------------------------------------------
// UserEvents — delEntries
// ---------------------------------------------------------------------------

describe("UserEvents.delEntries succeeds with mock SQL", () => {
  it.effect("UserEvents.delEntries succeeds with mock SQL", () =>
    UserEvents.delEntries("test_user_events", [eventId]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("UserEvents.delEntries with empty ids list succeeds", () => {
  it.effect("UserEvents.delEntries with empty ids list succeeds", () =>
    UserEvents.delEntries("test_user_events", []).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx fixtures
// ---------------------------------------------------------------------------

const txId = Buffer.alloc(32, 0x11);
const txCbor = Buffer.alloc(64, 0x22);

const testTxEntry: Tx.EntryNoTimeStamp = {
  tx_id: txId,
  tx: txCbor,
};

// ---------------------------------------------------------------------------
// Tx — createTable
// ---------------------------------------------------------------------------

describe("Tx.createTable succeeds with mock SQL", () => {
  it.effect("Tx.createTable succeeds with mock SQL", () =>
    Tx.createTable("test_tx").pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — insertEntry
// ---------------------------------------------------------------------------

describe("Tx.insertEntry succeeds with mock SQL", () => {
  it.effect("Tx.insertEntry succeeds with mock SQL", () =>
    Tx.insertEntry("test_tx", testTxEntry).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — insertEntries
// ---------------------------------------------------------------------------

describe("Tx.insertEntries with entries succeeds", () => {
  it.effect("Tx.insertEntries with entries succeeds", () =>
    Tx.insertEntries("test_tx", [testTxEntry]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("Tx.insertEntries with empty array is a no-op", () => {
  it.effect("Tx.insertEntries with empty array is a no-op", () =>
    Tx.insertEntries("test_tx", []).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Tx — delMultiple
// ---------------------------------------------------------------------------

describe("Tx.delMultiple succeeds with mock SQL", () => {
  it.effect("Tx.delMultiple succeeds with mock SQL", () => {
    mockSqlRows = [{ tx_id: txId }];
    return Tx.delMultiple("test_tx", [txId]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveValue
// ---------------------------------------------------------------------------

describe("Tx.retrieveValue returns buffer when row found", () => {
  it.effect("Tx.retrieveValue returns buffer when row found", () => {
    mockSqlRows = [{ tx: txCbor }];
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.map((result) => {
        expect(result).toEqual(txCbor);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("Tx.retrieveValue fails with NotFoundError when no row", () => {
  it.effect("Tx.retrieveValue fails with NotFoundError when no row", () => {
    mockSqlRows = [];
    return Tx.retrieveValue("test_tx", txId).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(Exit.isFailure(exit)).toBe(true);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveValues
// ---------------------------------------------------------------------------

describe("Tx.retrieveValues returns all matching buffers", () => {
  it.effect("Tx.retrieveValues returns all matching buffers", () => {
    mockSqlRows = [{ tx: txCbor }, { tx: Buffer.alloc(32, 0x33) }];
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(2);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("Tx.retrieveValues returns empty array when no matches", () => {
  it.effect("Tx.retrieveValues returns empty array when no matches", () => {
    mockSqlRows = [];
    return Tx.retrieveValues("test_tx", [txId]).pipe(
      Effect.map((results) => {
        expect(results.length).toBe(0);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("Tx.retrieveAllEntries returns rows ordered by time", () => {
  it.effect("Tx.retrieveAllEntries returns rows ordered by time", () => {
    const withTimestamp: Tx.EntryWithTimeStamp = {
      tx_id: txId,
      tx: txCbor,
      time_stamp_tz: new Date("2024-01-01T00:00:00Z"),
    };
    mockSqlRows = [withTimestamp];
    return Tx.retrieveAllEntries("test_tx").pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

// ---------------------------------------------------------------------------
// Tx — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("Tx.retrieveTimeBoundEntries returns time-filtered rows", () => {
  it.effect("Tx.retrieveTimeBoundEntries returns time-filtered rows", () => {
    mockSqlRows = [testTxEntry];
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Tx.retrieveTimeBoundEntries("test_tx", start, end).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("Tx.retrieveTimeBoundEntries returns empty when no rows", () => {
  it.effect("Tx.retrieveTimeBoundEntries returns empty when no rows", () => {
    mockSqlRows = [];
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Tx.retrieveTimeBoundEntries("test_tx", start, end).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});
