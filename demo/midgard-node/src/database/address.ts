import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { mapSqlError } from "@/database/utils/common.js"

export enum Columns {
  TX_HASH = "tx",
  ADDRESS = "address",
}

export type Entry = {
  [Columns.TX_HASH]: Buffer;
  [Columns.ADDRESS]: Buffer;
};

export const createTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.TX_HASH)} BYTEA NOT NULL,
      ${sql(Columns.ADDRESS)} BYTEA NOT NULL,
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delTxHash = (
  tableName: string,
  tx_hash: Buffer,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete all entries with tx_hash`,
    );
    const result = yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.TX_HASH,
    )} = ${tx_hash}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(Effect.withLogSpan(`delTxHash table ${tableName}`), mapSqlError);

export const retrieveOne = (
  tableName: string,
  tx_hash: Buffer,
): Effect.Effect<Buffer, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<Buffer>`SELECT ${sql(Columns.ADDRESS)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.TX_HASH)} = ${tx_hash}`;

    if (result.length <= 0) {
      yield* Effect.fail(
        new SqlError.SqlError({ cause: `No value found for tx_hash ${tx_hash}` }),
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

export const retrieveMultiple = (
  tableName: string,
  tx_hashes: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Buffer[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const result = yield* sql<Buffer>`SELECT ${sql(Columns.ADDRESS)} FROM ${sql(
      tableName,
    )} WHERE ${sql.in(Columns.TX_HASH, tx_hashes)}`;

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

export const insertEntries = (
  tableName: string,
  entries: Entry[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert entries`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`entries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert entries: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveEntries = (
  tableName: string,
): Effect.Effect<readonly Entry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve entries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieve: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );
