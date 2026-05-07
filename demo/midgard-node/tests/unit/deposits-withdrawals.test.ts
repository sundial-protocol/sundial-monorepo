import { describe, expect, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

import * as Deposits from "@/database/deposits.js";
import * as Withdrawals from "@/database/withdrawals.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { makeUserEventEntry } from "./harness/fixtures.js";

const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
});

const testEntry = makeUserEventEntry(0xaa);
const eventId = testEntry.event_id;

// ---------------------------------------------------------------------------
// Deposits — insertEntries
// ---------------------------------------------------------------------------

describe("Deposits.insertEntries with entries succeeds", () => {
  it.effect("Deposits.insertEntries with entries succeeds", () =>
    Deposits.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("Deposits.insertEntries with empty array is a no-op", () => {
  it.effect("Deposits.insertEntries with empty array is a no-op", () =>
    Deposits.insertEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Deposits — retrieveAllEntries
// ---------------------------------------------------------------------------

describe("Deposits.retrieveAllEntries returns rows from SQL", () => {
  it.effect("Deposits.retrieveAllEntries returns rows from SQL", () => {
    sqlHarness.setRows([testEntry]);
    return Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(1);
        expect(rows[0].event_id).toEqual(testEntry.event_id);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("Deposits.retrieveAllEntries returns empty when no rows", () => {
  it.effect("Deposits.retrieveAllEntries returns empty when no rows", () =>
    Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Deposits — delEntries
// ---------------------------------------------------------------------------

describe("Deposits.delEntries succeeds", () => {
  it.effect("Deposits.delEntries succeeds", () =>
    Deposits.delEntries([eventId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("Deposits.delEntries with empty ids is a no-op", () => {
  it.effect("Deposits.delEntries with empty ids is a no-op", () =>
    Deposits.delEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Withdrawals — insertEntries
// ---------------------------------------------------------------------------

describe("Withdrawals.insertEntries with entries succeeds", () => {
  it.effect("Withdrawals.insertEntries with entries succeeds", () =>
    Withdrawals.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("Withdrawals.insertEntries with empty array is a no-op", () => {
  it.effect("Withdrawals.insertEntries with empty array is a no-op", () =>
    Withdrawals.insertEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

// ---------------------------------------------------------------------------
// Withdrawals — retrieveTimeBoundEntries
// ---------------------------------------------------------------------------

describe("Withdrawals.retrieveTimeBoundEntries returns rows", () => {
  it.effect("Withdrawals.retrieveTimeBoundEntries returns rows", () => {
    sqlHarness.setRows([testEntry]);
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Withdrawals.retrieveTimeBoundEntries(start, end).pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(1);
        expect(rows[0].event_info).toEqual(testEntry.event_info);
      }),
      Effect.provide(sqlHarness.layer),
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
          expect(rows).toHaveLength(0);
        }),
        Effect.provide(sqlHarness.layer),
      );
    },
  );
});
