// PGlite-backed Effect SqlClient layer for SDK integration tests.
//
// Provides a real in-process PostgreSQL-compatible SqlClient.  Schema is
// initialized through the real DBInitialization.program so that all repository
// methods execute against a correctly structured schema.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   // In a test:
//   const sqlLayer = makeTestSqlLayer();
//   const layers = Layer.mergeAll(sqlLayer, nodeConfigLayer, ...);
//   yield* DBInitialization.program.pipe(Effect.provide(layers));
//
// ─── Required packages ────────────────────────────────────────────────────────
//
// Add to midgard-node devDependencies (or pnpm workspace root) before
// implementing:
//
//   @electric-sql/pglite      — in-process Postgres engine
//   @effect/sql-pglite        — Effect SQL adapter for PGlite
//
// The adapter exposes PgLiteClient.layer({ database: "memory://" }) which
// returns a Layer<SqlClient.SqlClient>.  Use "memory://" for tests that do
// not need persistence across process restarts (most SDK integration tests).
// Use a temp filesystem path for tests that verify reopen behaviour.
//
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation):
//   import { PgLiteClient } from "@effect/sql-pglite";
//   import { SqlClient } from "@effect/sql";
//   import { Layer } from "effect";
//
//   export const makeTestSqlLayer = () =>
//     PgLiteClient.layer({ database: "memory://" });
//
//   // For persistence-across-reopen tests:
//   export const makeTestSqlLayerWithPath = (path: string) =>
//     PgLiteClient.layer({ database: path });
//
// When PGlite is not yet available, the integration tests that use SQL layers
// will fail at runtime (not compile time).  Add a skip.todo comment in those
// tests until the package is installed.

export type AnyLayer = any;

/**
 * Returns a PGlite-backed SqlClient Effect layer for use in integration tests.
 *
 * The database is in-memory and isolated per test run.  Initialize schema with
 * DBInitialization.program before calling any repository methods.
 *
 * TODO (implementation): replace stub with the real PgLiteClient.layer call
 * as described in the module JSDoc above.
 */
export const makeTestSqlLayer = (): AnyLayer => {
  // TODO (implementation): return PgLiteClient.layer({ database: "memory://" })
  throw new Error(
    "makeTestSqlLayer: not yet implemented — install @electric-sql/pglite and @effect/sql-pglite",
  );
};

/**
 * Returns a PGlite-backed SqlClient layer that stores data at `dbPath`.
 * Use for tests that need to verify persistence across reopen.
 *
 * TODO (implementation): return PgLiteClient.layer({ database: dbPath })
 */
export const makeTestSqlLayerWithPath = (_dbPath: string): AnyLayer => {
  throw new Error(
    "makeTestSqlLayerWithPath: not yet implemented — install @electric-sql/pglite and @effect/sql-pglite",
  );
};
