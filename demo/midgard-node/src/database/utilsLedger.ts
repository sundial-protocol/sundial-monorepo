import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";
import { mapSqlError } from "./utils.js";

export enum LedgerColumns {
  TX_ID = "tx_id",
  OUTREF = "outref",
  OUTPUT = "output",
  ADDRESS = "address",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type LedgerEntry = {
  [LedgerColumns.TX_ID]: Buffer; // for linking the tables
  [LedgerColumns.OUTREF]: Buffer; // for root calc and updating the ledger
  [LedgerColumns.OUTPUT]: Buffer; // for root calc and updating the ledger
  [LedgerColumns.ADDRESS]: Address; // for provider
  [LedgerColumns.TIMESTAMPTZ]: Date; // for provider
};

export type MinimalLedgerEntry = {
  [LedgerColumns.OUTREF]: Buffer;
  [LedgerColumns.OUTPUT]: Buffer;
};

export enum InputsColumns {
  OUTREF = "spent_outref",
  SPENDING_TX = "spending_tx_hash",
}

export type SpentInput = {
  [inputsCols in InputsColumns]: Buffer;
};

export const createLedgerTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
        ${sql(LedgerColumns.TX_ID)} BYTEA NOT NULL,
        ${sql(LedgerColumns.OUTREF)} BYTEA NOT NULL,
        ${sql(LedgerColumns.OUTPUT)} BYTEA NOT NULL,
        ${sql(LedgerColumns.ADDRESS)} TEXT NOT NULL,
        ${sql(LedgerColumns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
        PRIMARY KEY (${sql(LedgerColumns.OUTREF)})
      );`;
        yield* sql`CREATE INDEX ${sql(
          `idx_${tableName}_${LedgerColumns.ADDRESS}`,
        )} ON ${sql(tableName)} (${sql(LedgerColumns.ADDRESS)});`;
      }),
    );
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

export const delLedgerEntries = (
  tableName: string,
  outrefs: Buffer[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      LedgerColumns.OUTREF,
    )} IN ${sql.in(outrefs)}`;
  });
