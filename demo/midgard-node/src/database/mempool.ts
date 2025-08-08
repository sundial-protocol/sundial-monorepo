import { Database } from "@/services/database.js";
import {
  clearTable,
  insertTx,
  delMultiple,
  retrieveValues,
  retrieveNumberOfEntries,
  retrieveValue,
  TxEntry,
  TxColumns,
  mapSqlError,
} from "@/utils/tx.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Effect } from "effect";
import { fromHex } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { breakDownTx } from "@/utils/common.js";

export const tableName = "mempool";

export const insert = (
  txString: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const txCborBytes = fromHex(txString);
    const { txId, txCbor, spent, produced } = yield* breakDownTx(txCborBytes);
    // Insert the tx itself in `MempoolDB`.
    yield* insertTx(tableName, {
      tx_id: txId,
      tx: txCbor,
    });
    // Insert produced UTxOs in `MempoolLedgerDB`.
    yield* MempoolLedgerDB.insert(produced);
    // Remove spent inputs from MempoolLedgerDB.
    yield* MempoolLedgerDB.clearUTxOs(spent);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) =>
      Effect.logError(`${tableName} db: insert: ${JSON.stringify(e)}`),
    ),
  );

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = (): Effect.Effect<readonly TxEntry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<TxEntry>`SELECT ${sql(
      TxColumns.TX_ID,
    )}, ${sql(TxColumns.TX)} FROM ${sql(tableName)} LIMIT 100000`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveTxCount = () => retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, Error, Database> => delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
