// PGlite-backed Effect SqlClient layer for node integration tests.
//
// Provides a real in-process PostgreSQL-compatible SqlClient using PGlite.
// This is the integration test equivalent of the real PgClient layer.
//
// Usage:
//   const sqlLayer = makeTestSqlLayer();
//   const layers = Layer.mergeAll(sqlLayer, nodeConfigLayer, ...);
//   yield* DBInitialization.program.pipe(Effect.provide(layers));

import { PGlite } from "@electric-sql/pglite";
import * as Client from "@effect/sql/SqlClient";
import * as Statement from "@effect/sql/Statement";
import { SqlError } from "@effect/sql/SqlError";
import * as Reactivity from "@effect/experimental/Reactivity";
import { Context, Effect, Layer, Stream } from "effect";

// Escape an identifier for use in SQL queries.
const escape = (s: string) => `"${s.replace(/"/g, '""')}"`;

// Build a PostgreSQL compiler compatible with PGlite's $N placeholder style.
const makeCompiler = () =>
  Statement.makeCompiler({
    dialect: "pg",
    placeholder: (n) => `$${n}`,
    onIdentifier: escape,
    onRecordUpdate(placeholders, valueAlias, valueColumns, values, returning) {
      return [
        `(values ${placeholders}) AS ${valueAlias}${valueColumns}${returning ? ` RETURNING ${returning[0]}` : ""}`,
        returning ? values.flat().concat(returning[1]) : values.flat(),
      ];
    },
    onCustom: () => ["", []],
  });

// Connection implementation backed by a PGlite instance.
class PGliteConnection {
  constructor(
    private readonly db: PGlite,
    private readonly compiler: ReturnType<typeof makeCompiler>,
  ) {}

  private run(sql: string, params: ReadonlyArray<unknown>) {
    return Effect.tryPromise({
      try: () =>
        this.db.query<Record<string, unknown>>(sql, params as unknown[]),
      catch: (cause) =>
        new SqlError({ cause, message: "PGlite: Failed to execute statement" }),
    }).pipe(Effect.map((r) => r.rows));
  }

  execute(
    sql: string,
    params: ReadonlyArray<unknown>,
    transformRows:
      | ((rows: ReadonlyArray<unknown>) => ReadonlyArray<unknown>)
      | undefined,
  ) {
    return transformRows
      ? Effect.map(this.run(sql, params), transformRows)
      : this.run(sql, params);
  }

  executeRaw(sql: string, params: ReadonlyArray<unknown>) {
    return this.run(sql, params);
  }

  executeWithoutTransform(sql: string, params: ReadonlyArray<unknown>) {
    return this.run(sql, params);
  }

  executeValues(sql: string, params: ReadonlyArray<unknown>) {
    return Effect.tryPromise({
      try: () =>
        this.db
          .query<Record<string, unknown>>(sql, params as unknown[])
          .then((r) =>
            r.rows.map((row) =>
              row !== null && typeof row === "object"
                ? Object.values(row)
                : [row],
            ),
          ),
      catch: (cause) =>
        new SqlError({ cause, message: "PGlite: Failed to execute values" }),
    });
  }

  executeUnprepared(
    sql: string,
    params: ReadonlyArray<unknown>,
    transformRows:
      | ((rows: ReadonlyArray<unknown>) => ReadonlyArray<unknown>)
      | undefined,
  ) {
    return this.execute(sql, params, transformRows);
  }

  executeStream(
    sql: string,
    params: ReadonlyArray<unknown>,
    transformRows:
      | ((rows: ReadonlyArray<unknown>) => ReadonlyArray<unknown>)
      | undefined,
  ) {
    return Stream.fromEffect(this.execute(sql, params, transformRows));
  }
}

// Creates a scoped PGlite-backed SqlClient Effect layer.
const makePGliteLayer = (databasePath: string | undefined) =>
  Layer.scopedContext(
    Effect.gen(function* () {
      const db = new PGlite(databasePath);
      yield* Effect.tryPromise({
        try: () => db.waitReady,
        catch: (cause) =>
          new SqlError({ cause, message: "PGlite: Failed to initialize" }),
      });
      yield* Effect.acquireRelease(Effect.void, () =>
        Effect.promise(() => db.close()),
      );
      const compiler = makeCompiler();
      const connection = new PGliteConnection(db, compiler);
      const client = yield* Client.make({
        acquirer: Effect.succeed(connection),
        compiler,
        spanAttributes: [["db.system", "postgresql"]],
        transformRows: undefined,
      });
      return Context.make(Client.SqlClient, client);
    }),
  ).pipe(Layer.provide(Reactivity.layer));

export const makeTestSqlLayer = () => makePGliteLayer(undefined);

export const makeTestSqlLayerWithPath = (dbPath: string) =>
  makePGliteLayer(dbPath);
