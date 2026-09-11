/**
 * Unit tests for database/utils/ledger.ts's SQL-touching functions.
 *
 * The default alias for "@/database/utils/ledger.js" under the coverage
 * config points at a stub (see vitest/node.coverage.config.mts), so this file
 * imports the real module by relative path to exercise its actual body
 * against a lightweight in-process SQL mock, following the same pattern as
 * ledger-transitions.test.ts and db-sql-utils.test.ts.
 */
import { describe, expect, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Exit } from "effect";

import * as Ledger from "../../src/database/utils/ledger.js";
import { makeLedgerEntry } from "./harness/fixtures.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
});

const tableName = "test_ledger";
const testEntry = makeLedgerEntry(0x01);
const outrefA = testEntry[Ledger.Columns.OUTREF];

describe("insertEntry", () => {
  it.effect("succeeds with mock SQL", () =>
    Ledger.insertEntry(tableName, testEntry).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("insertEntries", () => {
  it.effect("with entries succeeds", () =>
    Ledger.insertEntries(tableName, [testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("with empty array is a no-op", () =>
    Ledger.insertEntries(tableName, []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("insertEntriesOrIgnore", () => {
  it.effect("with entries succeeds", () =>
    Ledger.insertEntriesOrIgnore(tableName, [testEntry]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("with empty array is a no-op", () =>
    Ledger.insertEntriesOrIgnore(tableName, []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("retrieveByOutRef", () => {
  it.effect("returns the entry when a row is found", () => {
    sqlHarness.setRows([testEntry]);
    return Ledger.retrieveByOutRef(tableName, outrefA).pipe(
      Effect.map((entry) => {
        expect(entry).toEqual(testEntry);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("fails with NotFoundError when no row is found", () => {
    sqlHarness.setRows([]);
    return Ledger.retrieveByOutRef(tableName, outrefA).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(Exit.isFailure(exit)).toBe(true);
        if (Exit.isFailure(exit)) {
          const failure = exit.cause;
          expect(JSON.stringify(failure)).toContain("NotFoundError");
        }
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("retrieveByOutRefs", () => {
  it.effect("returns matching entries", () => {
    sqlHarness.setRows([testEntry]);
    return Ledger.retrieveByOutRefs(tableName, [outrefA]).pipe(
      Effect.map((entries) => {
        expect(entries.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("returns empty array when no rows match", () => {
    sqlHarness.setRows([]);
    return Ledger.retrieveByOutRefs(tableName, [outrefA]).pipe(
      Effect.map((entries) => {
        expect(entries.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });
});

describe("retrieveAllEntries", () => {
  it.effect("returns mock rows", () => {
    sqlHarness.setRows([testEntry, testEntry]);
    return Ledger.retrieveAllEntries(tableName).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(2);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("returns empty array when no rows", () =>
    Ledger.retrieveAllEntries(tableName).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("retrieveAllEntriesNoTimeStamps", () => {
  it.effect("returns mock rows", () => {
    sqlHarness.setRows([testEntry]);
    return Ledger.retrieveAllEntriesNoTimeStamps(tableName).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("returns empty array when no rows", () =>
    Ledger.retrieveAllEntriesNoTimeStamps(tableName).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("retrieveEntriesWithAddress", () => {
  it.effect("returns entries for the address", () => {
    sqlHarness.setRows([testEntry]);
    return Ledger.retrieveEntriesWithAddress(tableName, testEntry.address).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(1);
      }),
      Effect.provide(sqlHarness.layer),
    );
  });

  it.effect("returns empty array when no rows match", () =>
    Ledger.retrieveEntriesWithAddress(tableName, testEntry.address).pipe(
      Effect.map((rows) => {
        expect(rows.length).toBe(0);
      }),
      Effect.provide(sqlHarness.layer),
    ),
  );
});

describe("delEntries", () => {
  it.effect("with outrefs succeeds", () =>
    Ledger.delEntries(tableName, [outrefA]).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );

  it.effect("with empty array is a no-op", () =>
    Ledger.delEntries(tableName, []).pipe(
      Effect.map(() => expect(sqlHarness.getCallCount()).toBe(0)),
      Effect.provide(sqlHarness.layer),
    ),
  );
});
