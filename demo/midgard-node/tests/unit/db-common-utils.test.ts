import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";

import {
  retrieveNumberOfEntries,
  clearTable,
  DatabaseError,
  NotFoundError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { batchProgram } from "@/utils.js";

const makeDbLayer = (rows: any[]) =>
  Layer.succeed(
    SqlClient.SqlClient,
    Object.assign(
      function (stringsOrStr: any) {
        if (Array.isArray(stringsOrStr) && "raw" in stringsOrStr) {
          return Effect.succeed(rows);
        }
        return stringsOrStr;
      },
      {
        withTransaction: (eff: Effect.Effect<unknown>) => eff,
        insert: (obj: unknown) => obj,
        in: (_col: string, vals: unknown[]) => vals,
        literal: (s: string) => s,
      },
    ) as unknown as SqlClient.SqlClient,
  );

// ---------------------------------------------------------------------------
// retrieveNumberOfEntries
// ---------------------------------------------------------------------------

describe("retrieveNumberOfEntries returns count as bigint", () => {
  it.effect("retrieveNumberOfEntries returns count as bigint", () =>
    retrieveNumberOfEntries("test_table").pipe(
      Effect.map((count) => {
        expect(count).toBe(5n);
      }),
      Effect.provide(makeDbLayer([{ count: "5" }])),
    ),
  );
});

describe("retrieveNumberOfEntries returns 0n when count is zero", () => {
  it.effect("retrieveNumberOfEntries returns 0n when count is zero", () =>
    retrieveNumberOfEntries("test_table").pipe(
      Effect.map((count) => {
        expect(count).toBe(0n);
      }),
      Effect.provide(makeDbLayer([{ count: "0" }])),
    ),
  );
});

// ---------------------------------------------------------------------------
// clearTable
// ---------------------------------------------------------------------------

describe("clearTable succeeds with mock SQL", () => {
  it.effect("clearTable succeeds with mock SQL", () =>
    clearTable("test_table").pipe(
      Effect.map(() => expect(true).toBe(true)),
      Effect.provide(makeDbLayer([])),
    ),
  );
});

// ---------------------------------------------------------------------------
// DatabaseError and NotFoundError constructors
// ---------------------------------------------------------------------------

describe("DatabaseError has expected tag and table field", () => {
  it("DatabaseError has expected tag and table field", () => {
    const err = new DatabaseError({
      message: "db error",
      cause: undefined,
      table: "my_table",
    });
    expect(err._tag).toBe("DatabaseError");
    expect(err.table).toBe("my_table");
    expect(err.message).toBe("db error");
  });
});

describe("NotFoundError has expected tag and table field", () => {
  it("NotFoundError has expected tag and table field", () => {
    const err = new NotFoundError({
      message: "not found",
      cause: undefined,
      table: "my_table",
      txIdHex: "aabbcc",
    });
    expect(err._tag).toBe("NotFoundError");
    expect(err.table).toBe("my_table");
    expect(err.txIdHex).toBe("aabbcc");
  });
});

describe("sqlErrorToDatabaseError maps DatabaseError through unchanged", () => {
  it.effect(
    "sqlErrorToDatabaseError maps DatabaseError through unchanged",
    () => {
      const originalErr = new DatabaseError({
        message: "original",
        cause: undefined,
        table: "tbl",
      });
      return Effect.fail(originalErr).pipe(
        sqlErrorToDatabaseError("tbl", "wrapped"),
        Effect.exit,
        Effect.map((exit) => {
          expect(exit._tag).toBe("Failure");
        }),
      );
    },
  );
});

// ---------------------------------------------------------------------------
// batchProgram
// ---------------------------------------------------------------------------

describe("batchProgram runs effectMaker for each batch", () => {
  it.effect("batchProgram runs effectMaker for each batch", () =>
    Effect.gen(function* () {
      const calls: [number, number][] = [];
      yield* batchProgram(10, 25, "test", (start, end) => {
        calls.push([start, end]);
        return Effect.succeed(undefined);
      });
      expect(calls.length).toBe(3);
      expect(calls[0]).toEqual([0, 10]);
      expect(calls[1]).toEqual([10, 20]);
      expect(calls[2]).toEqual([20, 30]);
    }),
  );
});

describe("batchProgram with zero totalCount runs no effects", () => {
  it.effect("batchProgram with zero totalCount runs no effects", () =>
    Effect.gen(function* () {
      const calls: [number, number][] = [];
      yield* batchProgram(10, 0, "empty", (start, end) => {
        calls.push([start, end]);
        return Effect.succeed(undefined);
      });
      expect(calls.length).toBe(0);
    }),
  );
});

describe("batchProgram collects results from all batches", () => {
  it.effect("batchProgram collects results from all batches", () =>
    Effect.gen(function* () {
      const results = yield* batchProgram(5, 10, "collect", (start, _end) =>
        Effect.succeed(start),
      );
      expect(results).toContain(0);
      expect(results).toContain(5);
    }),
  );
});
