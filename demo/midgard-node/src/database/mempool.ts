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
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";

export const tableName = "mempool";
const INSERT_MULTIPLE_CHUNK_SIZE = 100;

export enum Columns {
  TX_SIZE_BYTES = "tx_size_bytes",
  SPENT_OUTREFS = "spent_outrefs",
  PRODUCED_OUTREFS = "produced_outrefs",
  PRODUCED_OUTPUTS = "produced_outputs",
  PRODUCED_ADDRESSES = "produced_addresses",
}

export type EntryWithEffects = Tx.EntryWithTimeStamp & {
  [Columns.TX_SIZE_BYTES]: number | null;
  [Columns.SPENT_OUTREFS]: readonly (Buffer | Uint8Array | string)[] | null;
  [Columns.PRODUCED_OUTREFS]:
    | readonly (Buffer | Uint8Array | string)[]
    | null;
  [Columns.PRODUCED_OUTPUTS]:
    | readonly (Buffer | Uint8Array | string)[]
    | null;
  [Columns.PRODUCED_ADDRESSES]: readonly string[] | null;
};

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

const toProducedColumns = (
  processedTx: ProcessedTx,
): {
  [Columns.PRODUCED_OUTREFS]: Buffer[];
  [Columns.PRODUCED_OUTPUTS]: Buffer[];
  [Columns.PRODUCED_ADDRESSES]: string[];
} => ({
  [Columns.PRODUCED_OUTREFS]: processedTx.produced.map(
    (entry) => entry[Ledger.Columns.OUTREF],
  ),
  [Columns.PRODUCED_OUTPUTS]: processedTx.produced.map(
    (entry) => entry[Ledger.Columns.OUTPUT],
  ),
  [Columns.PRODUCED_ADDRESSES]: processedTx.produced.map(
    (entry) => entry[Ledger.Columns.ADDRESS],
  ),
});

const hasPersistedEffects = (
  entry: EntryWithEffects,
): entry is EntryWithEffects & {
  [Columns.TX_SIZE_BYTES]: number;
  [Columns.SPENT_OUTREFS]: readonly (Buffer | Uint8Array | string)[];
  [Columns.PRODUCED_OUTREFS]: readonly (Buffer | Uint8Array | string)[];
  [Columns.PRODUCED_OUTPUTS]: readonly (Buffer | Uint8Array | string)[];
  [Columns.PRODUCED_ADDRESSES]: readonly string[];
} =>
  entry[Columns.TX_SIZE_BYTES] !== null &&
  entry[Columns.SPENT_OUTREFS] !== null &&
  entry[Columns.PRODUCED_OUTREFS] !== null &&
  entry[Columns.PRODUCED_OUTPUTS] !== null &&
  entry[Columns.PRODUCED_ADDRESSES] !== null &&
  entry[Columns.PRODUCED_OUTREFS].length ===
    entry[Columns.PRODUCED_OUTPUTS].length &&
  entry[Columns.PRODUCED_OUTREFS].length ===
    entry[Columns.PRODUCED_ADDRESSES].length;

const decodeBytea = (value: Buffer | Uint8Array | string): Buffer =>
  typeof value === "string"
    ? Buffer.from(
        value.startsWith("\\x") ? value.slice(2) : value,
        "hex",
      )
    : Buffer.from(value);

const normalizeTxIdToBuffer = (
  txId: Buffer | Uint8Array | string,
): Buffer => decodeBytea(txId);

const toProcessedTxFromPersistedEffects = (
  entry: EntryWithEffects & {
    [Columns.SPENT_OUTREFS]: readonly Buffer[];
    [Columns.PRODUCED_OUTREFS]: readonly Buffer[];
    [Columns.PRODUCED_OUTPUTS]: readonly Buffer[];
    [Columns.PRODUCED_ADDRESSES]: readonly string[];
  },
): ProcessedTx => {
  const produced: Ledger.Entry[] = entry[Columns.PRODUCED_OUTREFS].map(
    (outRef, index) => ({
      [Ledger.Columns.TX_ID]: normalizeTxIdToBuffer(entry[Tx.Columns.TX_ID]),
      [Ledger.Columns.OUTREF]: decodeBytea(outRef),
      [Ledger.Columns.OUTPUT]: decodeBytea(entry[Columns.PRODUCED_OUTPUTS][index]),
      [Ledger.Columns.ADDRESS]: entry[Columns.PRODUCED_ADDRESSES][index],
    }),
  );
  return {
    txId: normalizeTxIdToBuffer(entry[Tx.Columns.TX_ID]),
    txCbor: decodeBytea(entry[Tx.Columns.TX]),
    spent: entry[Columns.SPENT_OUTREFS].map(decodeBytea),
    produced,
  };
};

export const ensureNormalizedColumns: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql.withTransaction(
    Effect.gen(function* () {
      yield* sql`ALTER TABLE ${sql(tableName)}
        ADD COLUMN IF NOT EXISTS ${sql(Columns.TX_SIZE_BYTES)} INTEGER`;
      yield* sql`ALTER TABLE ${sql(tableName)}
        ADD COLUMN IF NOT EXISTS ${sql(Columns.SPENT_OUTREFS)} BYTEA[]`;
      yield* sql`ALTER TABLE ${sql(tableName)}
        ADD COLUMN IF NOT EXISTS ${sql(Columns.PRODUCED_OUTREFS)} BYTEA[]`;
      yield* sql`ALTER TABLE ${sql(tableName)}
        ADD COLUMN IF NOT EXISTS ${sql(Columns.PRODUCED_OUTPUTS)} BYTEA[]`;
      yield* sql`ALTER TABLE ${sql(tableName)}
        ADD COLUMN IF NOT EXISTS ${sql(Columns.PRODUCED_ADDRESSES)} TEXT[]`;
    }),
  );
}).pipe(
  Effect.withLogSpan(`ensureNormalizedColumns ${tableName}`),
  sqlErrorToDatabaseError(
    tableName,
    "Failed to ensure normalized effect columns in mempool",
  ),
);

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
          const txEntries = txChunk.map((tx) => ({
            [Tx.Columns.TX_ID]: tx.txId,
            [Tx.Columns.TX]: tx.txCbor,
            [Columns.TX_SIZE_BYTES]: tx.txCbor.length,
            [Columns.SPENT_OUTREFS]: tx.spent,
            ...toProducedColumns(tx),
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
): Effect.Effect<readonly EntryWithEffects[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithEffects>`SELECT
      ${sql(Tx.Columns.TX_ID)},
      ${sql(Tx.Columns.TX)},
      ${sql(Tx.Columns.TIMESTAMPTZ)},
      ${sql(Columns.TX_SIZE_BYTES)},
      ${sql(Columns.SPENT_OUTREFS)},
      ${sql(Columns.PRODUCED_OUTREFS)},
      ${sql(Columns.PRODUCED_OUTPUTS)},
      ${sql(Columns.PRODUCED_ADDRESSES)}
      FROM ${sql(tableName)}
      WHERE ${startTime} <= ${sql(Tx.Columns.TIMESTAMPTZ)}
      AND ${sql(Tx.Columns.TIMESTAMPTZ)} < ${endTime}
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve time-bound mempool transactions",
    ),
  );

export const retrieveEntriesBeforeTime = (endTime: Date) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithEffects>`SELECT
      ${sql(Tx.Columns.TX_ID)},
      ${sql(Tx.Columns.TX)},
      ${sql(Tx.Columns.TIMESTAMPTZ)},
      ${sql(Columns.TX_SIZE_BYTES)},
      ${sql(Columns.SPENT_OUTREFS)},
      ${sql(Columns.PRODUCED_OUTREFS)},
      ${sql(Columns.PRODUCED_OUTPUTS)},
      ${sql(Columns.PRODUCED_ADDRESSES)}
      FROM ${sql(tableName)}
      WHERE ${sql(Tx.Columns.TIMESTAMPTZ)} < ${endTime}
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesBeforeTime ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve mempool transactions before the given time",
    ),
  );

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

export const toProcessedTx = (
  entry: EntryWithEffects,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  hasPersistedEffects(entry)
    ? Effect.succeed(toProcessedTxFromPersistedEffects(entry))
    : breakDownTx(entry[Tx.Columns.TX]);

export const toTxEntry = (entry: EntryWithEffects): Tx.Entry => ({
  [Tx.Columns.TX_ID]: normalizeTxIdToBuffer(entry[Tx.Columns.TX_ID]),
  [Tx.Columns.TX]: decodeBytea(entry[Tx.Columns.TX]),
});

export const clear = clearTable(tableName);
