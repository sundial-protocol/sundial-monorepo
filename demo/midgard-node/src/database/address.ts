import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { mapSqlError } from "@/database/utils/common.js"
import { Address, CML } from "@lucid-evolution/lucid";
import * as MempoolDB from "@/database/mempool.js"
import * as ImmutableDB from "@/database/immutable.js"
import * as Tx from "@/database/utils/tx.js"
import * as Ledger from "@/database/utils/ledger.js"
import { ConfirmedLedgerDB, LatestLedgerDB, MempoolLedgerDB } from "./index.js";

export enum Columns {
  TX_ID = "tx_id",
  ADDRESS = "address",
}

export type Entry = {
  [Columns.TX_ID]: Buffer;
  [Columns.ADDRESS]: Address;
};

export const createTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Columns.ADDRESS)} BYTEA NOT NULL,
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);


/**
 * Inserts transaction entry by looking up
 * addresses and output references in the ledger tables
 * for provided transaction IDs.
 *
 * Looks up all output addresses from ledger databases,
 * then takes transaction ids in found output references
 * and repeats the process on their transaction ids as well.
 */
export const insert = (
  tableName: string,
  transactionIds: Buffer[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert entries`);
    const sql = yield* SqlClient.SqlClient;

    const allDataForEntries = yield* sql<Ledger.EntryWithTimeStamp>
    `SELECT * FROM (
      SELECT *
      FROM ${sql(MempoolLedgerDB.tableName)}
      UNION
      SELECT *
      FROM ${sql(LatestLedgerDB.tableName)}
      UNION
      SELECT *
      FROM ${sql(ConfirmedLedgerDB.tableName)}
    ) AS ledger_union
    WHERE ${sql(Columns.TX_ID)} IN ${sql.in(transactionIds)}`;

    const outputEntries : Entry[] = allDataForEntries.map((e) => ({
      [Columns.TX_ID]: e[Ledger.Columns.TX_ID],
      [Columns.ADDRESS]: e[Ledger.Columns.ADDRESS]
    }))

    const outRefs = allDataForEntries.map((e) => e[Ledger.Columns.OUTREF]).map((buf) => CML.TransactionInput.from_cbor_bytes(buf))
    const refTxIds = outRefs.map((i) => i.transaction_id()).map(tx_id => Buffer.from(tx_id.to_raw_bytes()))

    const allDataForOutRefs = yield* sql<Ledger.EntryWithTimeStamp>
    `SELECT ${sql(Ledger.Columns.ADDRESS)} FROM (
      SELECT *
      FROM ${sql(MempoolLedgerDB.tableName)}
      UNION
      SELECT *
      FROM ${sql(LatestLedgerDB.tableName)}
      UNION
      SELECT *
      FROM ${sql(ConfirmedLedgerDB.tableName)}
    ) AS ledger_union
    WHERE ${sql(Columns.TX_ID)} IN ${sql.in(refTxIds)}`;

    const inputEntries : Entry[] = allDataForOutRefs.map((e) => ({
      [Columns.TX_ID]: e[Ledger.Columns.TX_ID],
      [Columns.ADDRESS]: e[Ledger.Columns.ADDRESS]
    }))

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert([...inputEntries, ...outputEntries])}`;
  }).pipe(
    Effect.withLogSpan(`entries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert entries: ${JSON.stringify(e)}`),
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
      Columns.TX_ID,
    )} = ${tx_hash}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(Effect.withLogSpan(`delTxHash table ${tableName}`), mapSqlError);

/**
 * Retreives all cbors from MempoolDB and ImmutableDB
 * which mention provided address.
 *
 * Works by doing an inner join with tables
 * [tx_id | address] and [tx_id | tx],
 * getting [address | tx] as a result.
 */
export const retrieve = (
  tableName: string,
  address: Address,
): Effect.Effect<Buffer, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);
    const result = yield* sql<Buffer>`SELECT ${sql(Tx.Columns.TX)} FROM (
    SELECT ${sql(Tx.Columns.TX_ID)}
    FROM ${MempoolDB.tableName}
    UNION
    SELECT ${Tx.Columns.TX_ID}
    FROM ${sql(ImmutableDB.tableName)}
    ) AS tx_union
    INNER JOIN ${sql(tableName)} ON tx_union.${Tx.Columns.TX_ID} = ${sql(tableName)}.${Columns.TX_ID};
    WHERE ${sql(Columns.ADDRESS)} = ${address}`;

    if (result.length <= 0) {
      yield* Effect.fail(
        new SqlError.SqlError({ cause: `No value found for address ${address}` }),
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
