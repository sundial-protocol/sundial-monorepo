import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";
import {
  clearTable,
  sqlErrorToDatabaseError,
  DatabaseError,
  retrieveNumberOfEntries,
} from "@/database/utils/common.js";
import { ProcessedTx } from "@/utils.js";
import { AddressHistoryDB, MempoolLedgerDB, Tx } from "./index.js";

export const tableName = "mempool";

const normalizeTxIdToHex = (txId: Buffer | Uint8Array | string): string =>
  typeof txId === "string"
    ? txId.startsWith("\\x")
      ? txId.slice(2)
      : txId
    : Buffer.from(txId).toString("hex");

/**
 * Along with insertions to MempoolDB, applies transactions to MempoolLedgerDB,
 * updating it. Also adds corresponding entries to AddressHistoryDB.
 */
export const insertMultiple = (
  processedTxs: ProcessedTx[],
): Effect.Effect<
  number,
  SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return 0;
    }

    const txEntries: Tx.EntryNoTimeStamp[] = processedTxs.map((tx) => ({
      [Tx.Columns.TX_ID]: tx.txId,
      [Tx.Columns.TX]: tx.txCbor,
    }));

    const sql = yield* SqlClient.SqlClient;
    const insertedTxRows = yield* sql<{
      [Tx.Columns.TX_ID]: Buffer | Uint8Array | string;
    }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(txEntries)}
      ON CONFLICT (${sql(Tx.Columns.TX_ID)}) DO NOTHING
      RETURNING ${sql(Tx.Columns.TX_ID)}`;

    if (insertedTxRows.length === 0) {
      return 0;
    }

    const insertedTxIdsHex = new Set(
      insertedTxRows.map((row) => normalizeTxIdToHex(row[Tx.Columns.TX_ID])),
    );
    const newlyInsertedProcessedTxs = processedTxs.filter((processedTx) =>
      insertedTxIdsHex.has(processedTx.txId.toString("hex")),
    );

    if (newlyInsertedProcessedTxs.length === 0) {
      return 0;
    }

    const { addressHistoryEntries, collectiveProduced, collectiveSpent } =
      yield* AddressHistoryDB.aggregateProcessedTxs(
        MempoolLedgerDB.tableName,
        newlyInsertedProcessedTxs,
        AddressHistoryDB.Status.SLATED,
      );

    // TODO: Batching might be needed.
    yield* Effect.all(
      [
        // Insert transactions corresponding entries to `AddressHistoryDB`.
        AddressHistoryDB.upsertEntries(addressHistoryEntries),
        // Insertion to `MempoolLedgerDB` followed by removal of spent outrefs in
        // sequence.
        Effect.all([
          MempoolLedgerDB.insert(collectiveProduced),
          MempoolLedgerDB.clearUTxOs(collectiveSpent),
        ]),
      ],
      { concurrency: "unbounded" },
    );

    return insertedTxRows.length;
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert sql error: ${JSON.stringify(e)}`,
      ),
    ),
    Effect.tapError((e) => Effect.logError(`${tableName} db: insert: ${e}`)),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert the given transactions",
    ),
  );

export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  Tx.retrieveValues(tableName, txHashes);

export const retrieve: Effect.Effect<
  readonly Tx.EntryWithTimeStamp[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Tx.EntryWithTimeStamp>`SELECT ${sql(
    Tx.Columns.TX_ID,
  )}, ${sql(Tx.Columns.TX)} FROM ${sql(tableName)} ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} DESC LIMIT 100000`; // Add ordering by time
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
  ),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve given transactions"),
);

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Tx.Entry[], DatabaseError, Database> =>
  Tx.retrieveTimeBoundEntries(tableName, startTime, endTime);

export const retrieveEntriesBeforeTime = (endTime: Date) =>
  Tx.retrieveEntriesBeforeTime(tableName, endTime);

export const retrieveTxCount: Effect.Effect<bigint, DatabaseError, Database> =
  retrieveNumberOfEntries(tableName);

export const touchTxs = (
  txHashes: readonly Buffer[],
  timestamp: Date,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txHashes.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Tx.Columns.TIMESTAMPTZ)} = ${timestamp}
      WHERE ${sql.in(Tx.Columns.TX_ID, txHashes)}`;
  }).pipe(
    Effect.withLogSpan(`touchTxs ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to update timestamps for mempool transactions",
    ),
  );

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
