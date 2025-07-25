import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect, Option } from "effect";
import { CML, UTxO } from "@lucid-evolution/lucid";

export const mkKeyValueCreateQuery = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      key BYTEA NOT NULL,
      value BYTEA NOT NULL,
      PRIMARY KEY (key)
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delMultiple = (
  tableName: string,
  keys: Uint8Array[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE key IN ${sql.in(keys)} RETURNING key`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(Effect.withLogSpan(`delMutiple table ${tableName}`), mapSqlError);

export const retrieveValue = (
  tableName: string,
  key: Uint8Array,
): Effect.Effect<Option.Option<Uint8Array>, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<Uint8Array[]>`SELECT value FROM ${sql(
      tableName,
    )} WHERE key = ${Buffer.from(key)} LIMIT 1 `;
    return Option.fromNullable(result[0]?.[0]);
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
  keys: Uint8Array[],
): Effect.Effect<Uint8Array[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const result = yield* sql`SELECT value FROM ${sql(
      tableName,
    )} WHERE ${sql.in("key", keys.map(Buffer.from))}`;
    return result.map((row: any) => Uint8Array.from(row.value));
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
  key: Uint8Array,
  value: Uint8Array,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert keyValue`);
    const sql = yield* SqlClient.SqlClient;
    const valueBuffer: Buffer = Buffer.from(value);
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
      key: Buffer.from(key),
      value: valueBuffer,
    })} ON CONFLICT (key) DO UPDATE SET value = ${valueBuffer}`;
  }).pipe(
    Effect.withLogSpan(`insert keyValue ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert keyValue: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const insertKeyValues = (
  tableName: string,
  values: { key: Uint8Array; value: Uint8Array }[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert keyValues`);
    const sql = yield* SqlClient.SqlClient;
    const pairs = values.map((kv) => ({
      key: Buffer.from(kv.key),
      value: Buffer.from(kv.value),
    }));
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(pairs)}`;
  }).pipe(
    Effect.withLogSpan(`insert keyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert keyValues: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveKeyValues = (
  tableName: string,
): Effect.Effect<
  readonly {
    key: Uint8Array;
    value: Uint8Array;
  }[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      key: Uint8Array;
      value: Uint8Array;
    }>`SELECT key, value FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`insert keyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert keyValues: ${JSON.stringify(e)}`,
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

export const mkLedgerCreateQuery = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      outref BYTEA NOT NULL,
      output BYTEA NOT NULL,
      address TEXT NOT NULL,
      PRIMARY KEY (outref)
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

const txOutputBytesToAddress = (txOutputBytes: Uint8Array): string => {
  const txOutput = CML.TransactionOutput.from_cbor_bytes(txOutputBytes);
  const address = txOutput.address().to_bech32();
  return address;
};

export const insertLedgerUTxO = (
  tableName: string,
  outReferenceBytes: Uint8Array,
  txOutputBytes: Uint8Array,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert LedgerUTxO`);
    const sql = yield* SqlClient.SqlClient;

    const address = txOutputBytesToAddress(txOutputBytes);

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
      outref: Buffer.from(outReferenceBytes),
      output: Buffer.from(txOutputBytes),
      address: Buffer.from(address),
    })} ON CONFLICT (outref) DO UPDATE SET output = ${Buffer.from(txOutputBytes)}, address = ${Buffer.from(address)}`;
  }).pipe(
    Effect.withLogSpan(`insert LedgerUTxO ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert LedgerUTxO: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const insertLedgerUTxOs = (
  tableName: string,
  values: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert LedgerUTxOs`);
    const sql = yield* SqlClient.SqlClient;

    const triples = values.map((kv) => ({
      outref: Buffer.from(kv.outReferenceBytes),
      output: Buffer.from(kv.txOutputBytes),
      address: Buffer.from(txOutputBytesToAddress(kv.txOutputBytes)),
    }));
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(triples)}`;
  }).pipe(
    Effect.withLogSpan(`insert LedgerUTxOs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert LedgerUTxOs: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveLedgerUTxOs = (
  tableName: string,
): Effect.Effect<
  readonly {
    outReferenceBytes: Uint8Array;
    txOutputBytes: Uint8Array;
    address: string;
  }[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve LedgerUTxOs`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      outReferenceBytes: Uint8Array;
      txOutputBytes: Uint8Array;
      address: string;
    }>`SELECT outref, output, address FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`insert LedgerUTxOs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert LedgerUTxOs: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveLedgerUTxOsWithAddress = (
  tableName: string,
  address: string,
): Effect.Effect<
  readonly {
    outReferenceBytes: Uint8Array;
    txOutputBytes: Uint8Array;
    address: string;
  }[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve LedgerUTxOs`);
    const sql = yield* SqlClient.SqlClient;

    return yield* sql<{
      outReferenceBytes: Uint8Array;
      txOutputBytes: Uint8Array;
      address: string;
    }>`SELECT outref, output, address FROM ${sql(tableName)} WHERE address = ${Buffer.from(address)}`;
  }).pipe(
    Effect.withLogSpan(`insert LedgerUTxOs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert LedgerUTxOs: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );
