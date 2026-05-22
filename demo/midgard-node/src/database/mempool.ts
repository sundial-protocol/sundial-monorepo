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
const INSERT_MULTIPLE_CHUNK_SIZE = 100;

const normalizeTxIdToHex = (txId: Buffer | Uint8Array | string): string =>
  typeof txId === "string"
    ? txId.startsWith("\\x")
      ? txId.slice(2)
      : txId
    : Buffer.from(txId).toString("hex");

const chunkProcessedTxs = (processedTxs: ProcessedTx[]): ProcessedTx[][] => {
  const chunks: ProcessedTx[][] = [];
  for (
    let startIndex = 0;
    startIndex < processedTxs.length;
    startIndex += INSERT_MULTIPLE_CHUNK_SIZE
  ) {
    chunks.push(
      processedTxs.slice(startIndex, startIndex + INSERT_MULTIPLE_CHUNK_SIZE),
    );
  }
  return chunks;
};

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

    const sql = yield* SqlClient.SqlClient;
    let totalInsertedRows = 0;
    const txChunks = chunkProcessedTxs(processedTxs);

    for (const [chunkIndex, txChunk] of txChunks.entries()) {
      const insertedChunkRows = yield* sql.withTransaction(
        Effect.gen(function* () {
          const txEntries: Tx.EntryNoTimeStamp[] = txChunk.map((tx) => ({
            [Tx.Columns.TX_ID]: tx.txId,
            [Tx.Columns.TX]: tx.txCbor,
          }));
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
            insertedTxRows.map((row) =>
              normalizeTxIdToHex(row[Tx.Columns.TX_ID]),
            ),
          );
          const newlyInsertedProcessedTxs = txChunk.filter((processedTx) =>
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

          // Apply projection changes in the same transaction as row insertion.
          yield* AddressHistoryDB.upsertEntries(addressHistoryEntries);
          yield* MempoolLedgerDB.insert(collectiveProduced);
          yield* MempoolLedgerDB.clearUTxOs(collectiveSpent);

          return insertedTxRows.length;
        }),
      );

      totalInsertedRows += insertedChunkRows;
      yield* Effect.logInfo(
        `${tableName} db: insertMultiple chunk ${chunkIndex + 1}/${txChunks.length} inserted_rows=${insertedChunkRows}`,
      );
    }

    return totalInsertedRows;
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
