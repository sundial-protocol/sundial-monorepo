import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

import {
  retrieveNumberOfEntries,
  clearTable,
  DatabaseError,
  NotFoundError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { batchProgram } from "@/utils.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const makeDbHarness = (count: string) =>
  createMockSqlHarness([{ count }] as const);

const clearTableHarness = createMockSqlHarness();

describe("retrieveNumberOfEntries", () => {
  it.effect("returns count as bigint", () =>
    retrieveNumberOfEntries("test_table").pipe(
      Effect.map((count) => {
        expect(count).toBe(5n);
      }),
      Effect.provide(makeDbHarness("5").layer),
    ),
  );

  it.effect("returns 0n when count is zero", () =>
    retrieveNumberOfEntries("test_table").pipe(
      Effect.map((count) => {
        expect(count).toBe(0n);
      }),
      Effect.provide(makeDbHarness("0").layer),
    ),
  );
});

describe("clearTable", () => {
  it.effect("succeeds with mock SQL", () =>
    clearTable("test_table").pipe(
      Effect.map(() =>
        expect(clearTableHarness.getCallCount()).toBeGreaterThan(0),
      ),
      Effect.provide(clearTableHarness.layer),
    ),
  );
});

describe("error constructors", () => {
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

describe("batchProgram", () => {
  it.effect("runs effectMaker for each batch", () =>
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

  it.effect("with zero totalCount runs no effects", () =>
    Effect.gen(function* () {
      const calls: [number, number][] = [];
      yield* batchProgram(10, 0, "empty", (start, end) => {
        calls.push([start, end]);
        return Effect.succeed(undefined);
      });
      expect(calls.length).toBe(0);
    }),
  );

  it.effect("collects results from all batches", () =>
    Effect.gen(function* () {
      const results = yield* batchProgram(5, 10, "collect", (start, _end) =>
        Effect.succeed(start),
      );
      expect(results).toContain(0);
      expect(results).toContain(5);
    }),
  );
});
