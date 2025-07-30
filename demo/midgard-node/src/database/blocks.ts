import { Effect } from "effect";
import {
  InputsColumns,
  KVColumns,
  LedgerColumns,
  ProcessedTx,
  ProcessedTxColumns,
  clearTable,
  createInputsTable,
  mapSqlError,
} from "./utils.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Database } from "@/services/database.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as ImmutableDB from "./immutable.js";

export const tableName = "blocks";

export enum Columns {
  HEADER_HASH = "header_hash",
  TX_ID = "tx_id",
}

type Entry = {
  [blockCols in Columns]: Buffer;
};

export const inputsTableName = "blocks_spent_inputs";

// Using mempool ledger as a sort of archive ledger. TODO
export const outputsTableName = MempoolLedgerDB.tableName;

export const init = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    ${sql(Columns.HEADER_HASH)} BYTEA NOT NULL,
    ${sql(Columns.TX_ID)} BYTEA NOT NULL UNIQUE
  );`;
  yield* createInputsTable(inputsTableName, tableName, Columns.TX_ID);
});

export const retrieveByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<readonly ProcessedTx[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<ProcessedTx>`
      SELECT
        bs.${sql(Columns.TX_ID)} as ${sql(ProcessedTxColumns.TX_ID)},
        (
          SELECT ${sql(KVColumns.VALUE)}
          FROM ${sql(ImmutableDB.tableName)} im
          WHERE im.${sql(KVColumns.KEY)} = bs.${sql(Columns.TX_ID)}
        ) as ${sql(ProcessedTxColumns.TX_CBOR)},
        COALESCE(
          ARRAY(
            SELECT bsi.${sql(InputsColumns.OUTREF)}
            FROM ${sql(inputsTableName)} bsi
            WHERE bsi.${sql(InputsColumns.SPENDING_TX)} = bs.${sql(Columns.TX_ID)}
          ),
          ARRAY[]::BYTEA[]
        ) AS ${sql(ProcessedTxColumns.INPUTS)},
        COALESCE(
          ARRAY(
            SELECT ROW(ml.${sql(LedgerColumns.TX_ID)}, ml.${sql(LedgerColumns.OUTREF)}, ml.${sql(LedgerColumns.OUTPUT)}, ml.${sql(LedgerColumns.ADDRESS)})::${sql(outputsTableName)}
            FROM ${MempoolLedgerDB.tableName} ml
            WHERE ml.${sql(LedgerColumns.TX_ID)} = bs.${sql(Columns.TX_ID)}
          ),
          ARRAY[]::${sql(outputsTableName)}[]
        ) AS ${sql(ProcessedTxColumns.OUTPUTS)}
      FROM ${sql(tableName)} bs
      WHERE bs.${sql(Columns.HEADER_HASH)} = ${headerHash};`;

    return rows;
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving by txHashes error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveTxHashesByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<readonly Buffer[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve txHashes for block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const result = yield* sql<Buffer>`SELECT ${sql(Columns.TX_ID)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}`;

    yield* Effect.logDebug(
      `${tableName} db: retrieved ${result.length} txHashes for block ${headerHash}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTxHashesByHeaderHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving txHashes error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveHeaderHashByTxHash = (
  txHash: Buffer,
): Effect.Effect<Buffer, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve headerHash for txHash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<Buffer>`SELECT ${sql(
      Columns.HEADER_HASH,
    )} FROM ${sql(tableName)} WHERE ${sql(Columns.TX_ID)} = ${txHash} LIMIT 1`;

    if (rows.length <= 0) {
      const msg = `No headerHash found for ${txHash} txHash`;
      const err = new SqlError.SqlError({ cause: msg, message: msg });
      yield* Effect.logDebug(msg);
      yield* Effect.fail(err);
    }
    const result = rows[0];
    yield* Effect.logDebug(
      `${tableName} db: retrieved headerHash for tx ${txHash}: ${result}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveBlockHashByTxHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving headerHash error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

/** Associated inputs are also deleted.
 */
export const clearBlock = (
  headerHash: Buffer,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt clear block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}`;
    // yield* Effect.logInfo(
    //   `${tableName} db: cleared ${result.entries()} rows for block ${toHex(headerHash)}`,
    // );
    return Effect.void;
  }).pipe(
    Effect.withLogSpan(`clearBlock ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: clearing block error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieve = (): Effect.Effect<readonly Entry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
    const sql = yield* SqlClient.SqlClient;
    const result = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
    yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} rows.`);
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clear = (): Effect.Effect<void, Error, Database> =>
  clearTable(tableName).pipe(Effect.withLogSpan(`clear ${tableName}`));
