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

export enum LedgerColumns {
  TX_ID = "tx_id",
  OUTREF = "outref",
  OUTPUT = "output",
  ADDRESS = "address",
}

export type LedgerEntry = {
  [LedgerColumns.TX_ID]: Buffer; // for linking the tables
  [LedgerColumns.OUTREF]: Buffer; // for root calc and updating the ledger
  [LedgerColumns.OUTPUT]: Buffer; // for root calc
  [LedgerColumns.ADDRESS]: Address; // for provider
};

export type MinimalLedgerEntry = {
  [LedgerColumns.OUTREF]: Buffer; // for root calc and updating the ledger
  [LedgerColumns.OUTPUT]: Buffer; // for root calc
};

export enum InputsColumns {
  OUTREF = "spent_outref",
  SPENDING_TX = "spending_tx_hash",
}

export type SpentInput = {
  [inputsCols in InputsColumns]: Buffer;
};

export enum ProcessedTxColumns {
  TX_ID = "ptx_id",
  TX_CBOR = "ptx_cbor",
  INPUTS = "ptx_inputs",
  OUTPUTS = "ptx_outputs",
}

export type ProcessedTx = {
  [ProcessedTxColumns.TX_ID]: Buffer;
  [ProcessedTxColumns.TX_CBOR]: Buffer;
  [ProcessedTxColumns.INPUTS]: Buffer[];
  [ProcessedTxColumns.OUTPUTS]: MinimalLedgerEntry[];
};

/** Since `@effect/sql` seems incapable of auto-parsing nested record types, we
 * need to parse the returning list of strings into `ProcessedTx[]`.
 *
 * (modified version of an AI generated code)
 */
export const parseLedgerEntryString = (
  rowString: string,
): Effect.Effect<MinimalLedgerEntry, Error> =>
  Effect.gen(function* () {
    // Strip any occurrences of `x`, `\`, `"`, and outer parentheses
    const fields = rowString.replace(/[\\\"x()]/g, "").split(",");
    if (fields.length !== 2) {
      yield* Effect.fail(
        new Error(
          "parseLedgerEntryString: Unexpected number of fields in ROW string",
        ),
      );
    }
    const [raw_outref, raw_cbor] = fields;
    return {
      [LedgerColumns.OUTREF]: Buffer.from(raw_outref, "hex"),
      [LedgerColumns.OUTPUT]: Buffer.from(raw_cbor, "hex"),
    };
  });

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
    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(KVColumns.KEY)} IN ${sql.in(keys)} RETURNING ${sql(KVColumns.KEY)}`;
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

export const createLedgerTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(LedgerColumns.TX_ID)} BYTEA NOT NULL,
      ${sql(LedgerColumns.OUTREF)} BYTEA NOT NULL,
      ${sql(LedgerColumns.OUTPUT)} BYTEA NOT NULL,
      ${sql(LedgerColumns.ADDRESS)} TEXT NOT NULL,
      PRIMARY KEY (${sql(LedgerColumns.OUTREF)})
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const insertLedgerEntry = (
  tableName: string,
  entry: LedgerEntry,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert LedgerUTxO`);
    const sql = yield* SqlClient.SqlClient;
    // No need to handle conflicts.
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}`;
  }).pipe(
    Effect.withLogSpan(`insertLedgerEntry ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insertLedgerEntry: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const insertLedgerEntries = (
  tableName: string,
  entries: LedgerEntry[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert LedgerUTxOs`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`insertLedgerEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insertLedgerEntries: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveLedgerEntries = (
  tableName: string,
): Effect.Effect<readonly LedgerEntry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieveLedgerEntries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<LedgerEntry>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveLedgerEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveLedgerEntries: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveLedgerEntriesWithAddress = (
  tableName: string,
  address: Address,
): Effect.Effect<readonly LedgerEntry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve LedgerUTxOs`);
    const sql = yield* SqlClient.SqlClient;

    return yield* sql<LedgerEntry>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(LedgerColumns.ADDRESS)} = ${address}`;
  }).pipe(
    Effect.withLogSpan(`retrieveLedgerEntriesWithAddress ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveLedgerEntriesWithAddress: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const createInputsTable = (
  inputsTableName: string,
  parentTableName: string,
  parentTableKeyLabel?: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(inputsTableName)} (
      ${sql(InputsColumns.OUTREF)} BYTEA NOT NULL,
      ${sql(InputsColumns.SPENDING_TX)} BYTEA NOT NULL,
      PRIMARY KEY (${sql(InputsColumns.OUTREF)}),
      FOREIGN KEY (${sql(InputsColumns.SPENDING_TX)}) REFERENCES ${sql(parentTableName)}(${sql.unsafe(parentTableKeyLabel ?? KVColumns.KEY)}) ON DELETE CASCADE
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${inputsTableName}`), mapSqlError);

export const insertSpentInput = (
  tableName: string,
  spentInput: SpentInput,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertKeyValue`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(
      spentInput,
    )} ON CONFLICT (${sql(InputsColumns.OUTREF)}) DO UPDATE SET ${sql(
      InputsColumns.SPENDING_TX,
    )} = ${spentInput.spending_tx_hash}`;
  }).pipe(
    Effect.withLogSpan(`insertSpentInput ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insertSpentInput: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );
