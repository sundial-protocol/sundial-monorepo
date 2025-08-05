import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";

export enum KVColumns {
  KEY = "key",
  VALUE = "value",
}

export type KVPair = {
  [kvCols in KVColumns]: Buffer;
};

export const createKeyValueTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(KVColumns.KEY)} BYTEA NOT NULL,
      ${sql(KVColumns.VALUE)} BYTEA NOT NULL,
      PRIMARY KEY (${sql(KVColumns.KEY)})
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delMultiple = (
  tableName: string,
  keys: Buffer[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    const result = yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      KVColumns.KEY,
    )} IN ${sql.in(keys)} RETURNING ${sql(KVColumns.KEY)}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(Effect.withLogSpan(`delMutiple table ${tableName}`), mapSqlError);

export const retrieveValue = (
  tableName: string,
  key: Buffer,
): Effect.Effect<Buffer, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<Buffer>`SELECT ${sql(KVColumns.VALUE)} FROM ${sql(
      tableName,
    )} WHERE ${sql(KVColumns.KEY)} = ${key}`;

    if (result.length <= 0) {
      yield* Effect.fail(
        new SqlError.SqlError({ cause: `No value found for key ${key}` }),
      );
    }

    return result[0];
  }).pipe(
    Effect.withLogSpan(`retrieve value ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving value error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveValues = (
  tableName: string,
  keys: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Buffer[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const result = yield* sql<Buffer>`SELECT ${sql(KVColumns.VALUE)} FROM ${sql(
      tableName,
    )} WHERE ${sql.in(KVColumns.KEY, keys)}`;

    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve values ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving values error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clearTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: clearing error: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const insertKeyValue = (
  tableName: string,
  kvPair: KVPair,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertKeyValue`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(
      kvPair,
    )} ON CONFLICT (${sql(KVColumns.KEY)}) DO UPDATE SET ${sql(KVColumns.VALUE)} = ${kvPair.value}`;
  }).pipe(
    Effect.withLogSpan(`insertKeyValue ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertKeyValue: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const insertKeyValues = (
  tableName: string,
  pairs: KVPair[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertKeyValues`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(pairs)}`;
  }).pipe(
    Effect.withLogSpan(`insertKeyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertKeyValues: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveKeyValues = (
  tableName: string,
): Effect.Effect<readonly KVPair[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<KVPair>`SELECT ${sql(KVColumns.KEY)}, ${sql(KVColumns.VALUE)} FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveKeyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveKeyValues: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<number, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to get number of entries`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      count: number;
    }>`SELECT COUNT(*) FROM ${sql(tableName)}`;
    return rows[0].count ?? 0;
  }).pipe(
    Effect.withLogSpan(`retrieveNumberOfEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveNumberOfEntries: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const mapSqlError = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, Exclude<E, SqlError.SqlError> | Error, R> =>
  effect.pipe(
    Effect.catchAll(
      (e): Effect.Effect<A, Exclude<E, SqlError.SqlError> | Error, R> => {
        if (e instanceof SqlError.SqlError) {
          return Effect.fail(
            new Error(`SQL Error (${e._tag}): ${JSON.stringify(e)}`),
          );
        } else return Effect.fail(e as Exclude<E, SqlError.SqlError>);
      },
    ),
  );
