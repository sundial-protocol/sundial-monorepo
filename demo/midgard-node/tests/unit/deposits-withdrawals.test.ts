import { describe, expect, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";

import * as Deposits from "@/database/deposits.js";
import * as Withdrawals from "@/database/withdrawals.js";
import * as UserEvents from "@/database/utils/user-events.js";

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
// Deposits — insertEntries
// ---------------------------------------------------------------------------

describe("Deposits.insertEntries with entries succeeds", () => {
  it.effect("Deposits.insertEntries with entries succeeds", () =>
    Deposits.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("Deposits.insertEntries with empty array is a no-op", () => {
  it.effect("Deposits.insertEntries with empty array is a no-op", () =>
    Deposits.insertEntries([]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Deposits — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("Deposits.retrieveAllEntries returns rows from SQL", () => {
  it.effect("Deposits.retrieveAllEntries returns rows from SQL", () => {
    mockSqlRows = [testEntry];
    return Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("Deposits.retrieveAllEntries returns empty when no rows", () => {
  it.effect("Deposits.retrieveAllEntries returns empty when no rows", () =>
    Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Deposits — delEntries
// ---------------------------------------------------------------------------

describe("Deposits.delEntries succeeds", () => {
  it.effect("Deposits.delEntries succeeds", () =>
    Deposits.delEntries([eventId]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("Deposits.delEntries with empty ids is a no-op", () => {
  it.effect("Deposits.delEntries with empty ids is a no-op", () =>
    Deposits.delEntries([]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Withdrawals — insertEntries
// ---------------------------------------------------------------------------

describe("Withdrawals.insertEntries with entries succeeds", () => {
  it.effect("Withdrawals.insertEntries with entries succeeds", () =>
    Withdrawals.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

describe("Withdrawals.insertEntries with empty array is a no-op", () => {
  it.effect("Withdrawals.insertEntries with empty array is a no-op", () =>
    Withdrawals.insertEntries([]).pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(mockDbLayer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Withdrawals — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("Withdrawals.retrieveTimeBoundEntries returns rows", () => {
  it.effect("Withdrawals.retrieveTimeBoundEntries returns rows", () => {
    mockSqlRows = [testEntry];
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Withdrawals.retrieveTimeBoundEntries(start, end).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});

describe("Withdrawals.retrieveTimeBoundEntries returns empty when no rows", () => {
  it.effect(
    "Withdrawals.retrieveTimeBoundEntries returns empty when no rows",
    () => {
      const start = new Date("2024-01-01T00:00:00Z");
      const end = new Date("2024-12-31T00:00:00Z");
      return Withdrawals.retrieveTimeBoundEntries(start, end).pipe(
        Effect.map((rows) => {
          expect(rows.length).toBe(0);
        }),
        Effect.provide(mockDbLayer),
      );
    },
  );
});
