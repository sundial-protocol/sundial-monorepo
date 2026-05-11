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

describe("Deposits", () => {
  it.effect("insertEntries with entries succeeds", () =>
    Deposits.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with empty array is a no-op", () =>
    Deposits.insertEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("retrieveAllEntries returns rows from SQL", () => {
    sqlHarness.setRows([testEntry]);
    return Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(1);
        expect(rows[0].event_id).toEqual(testEntry.event_id);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("retrieveAllEntries returns empty when no rows", () =>
    Deposits.retrieveAllEntries().pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("delEntries succeeds", () =>
    Deposits.delEntries([eventId]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("delEntries with empty ids still succeeds", () =>
    Deposits.delEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("Withdrawals", () => {
  it.effect("insertEntries with entries succeeds", () =>
    Withdrawals.insertEntries([testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("insertEntries with empty array is a no-op", () =>
    Withdrawals.insertEntries([]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("retrieveTimeBoundEntries returns rows", () => {
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

  it.effect("retrieveTimeBoundEntries returns empty when no rows", () => {
    const start = new Date("2024-01-01T00:00:00Z");
    const end = new Date("2024-12-31T00:00:00Z");
    return Withdrawals.retrieveTimeBoundEntries(start, end).pipe(
      Effect.map((rows) => {
        expect(rows).toHaveLength(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});
