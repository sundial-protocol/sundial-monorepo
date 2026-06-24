import { Database } from "@/services/database.js";
import { NodeConfig } from "@/services/config.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
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
import type { TxIngressMessage } from "@/services/index.js";

export const tableName = "mempool";
const INSERT_MULTIPLE_CHUNK_SIZE = 100;
const TX_ACCEPTANCE_ADVISORY_LOCK_KEY = 1_348_021_588;

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
  [Columns.PRODUCED_OUTREFS]: readonly (Buffer | Uint8Array | string)[] | null;
  [Columns.PRODUCED_OUTPUTS]: readonly (Buffer | Uint8Array | string)[] | null;
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

export type AcceptanceCandidate = {
  readonly arrivalSeq: bigint;
  readonly message: TxIngressMessage;
  readonly processedTx: ProcessedTx;
};

export type AcceptanceRejection = {
  readonly message: TxIngressMessage;
  readonly reason: string;
};

export type AcceptanceResult = {
  readonly acceptedMessages: readonly TxIngressMessage[];
  readonly insertedCount: number;
  readonly rejected: readonly AcceptanceRejection[];
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
    ? Buffer.from(value.startsWith("\\x") ? value.slice(2) : value, "hex")
    : Buffer.from(value);

const normalizeTxIdToBuffer = (txId: Buffer | Uint8Array | string): Buffer =>
  decodeBytea(txId);

const networkIdForValidation = (network: NodeConfig["Type"]["NETWORK"]): 0 | 1 =>
  network === "Mainnet" ? 1 : 0;

const defaultPhaseAConfig = (
  nodeConfig: NodeConfig["Type"],
): SDK.PhaseAConfig => {
  const networkId = networkIdForValidation(nodeConfig.NETWORK);
  return {
    expectedNetworkId: networkId,
    cardanoNetwork: networkId,
    // Cardano min-fee defaults currently used by Midgard transactions.
    minFeeA: 44n,
    minFeeB: 155381n,
  };
};

const txIdHex = (txId: Uint8Array): string => Buffer.from(txId).toString("hex");

const formatValidationReason = (rejection: SDK.RejectedTx): string =>
  rejection.detail === null
    ? `validation_rejected:${rejection.code}`
    : `validation_rejected:${rejection.code}:${rejection.detail}`;

const decodeLedgerState = (
  entries: readonly Ledger.Entry[],
): SDK.UTxOState => {
  const state: SDK.UTxOState = new Map();
  for (const entry of entries) {
    const outRef = CML.TransactionInput.from_cbor_bytes(entry[Ledger.Columns.OUTREF]);
    const key = SDK.outRefKey({
      tx_id: outRef.transaction_id().to_raw_bytes(),
      index: Number(outRef.index()),
    });
    const output = SDK.cmlOutputToMidgard(
      CML.TransactionOutput.from_cbor_bytes(entry[Ledger.Columns.OUTPUT]),
    );
    state.set(key, output);
  }
  return state;
};

const insertValidatedChunk = (
  sql: SqlClient.SqlClient,
  txChunk: readonly ProcessedTx[],
): Effect.Effect<number, SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError, Database> =>
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
      insertedTxRows.map((row) => normalizeTxIdToHex(row[Tx.Columns.TX_ID])),
    );
    const projectedTxIdsHex = new Set<string>();
    const newlyInsertedProcessedTxs = txChunk.filter((processedTx) => {
      const currentTxIdHex = processedTx.txId.toString("hex");
      if (
        !insertedTxIdsHex.has(currentTxIdHex) ||
        projectedTxIdsHex.has(currentTxIdHex)
      ) {
        return false;
      }
      projectedTxIdsHex.add(currentTxIdHex);
      return true;
    });

    if (newlyInsertedProcessedTxs.length === 0) {
      return 0;
    }

    const { addressHistoryEntries, collectiveProduced, collectiveSpent } =
      yield* AddressHistoryDB.aggregateProcessedTxs(
        MempoolLedgerDB.tableName,
        newlyInsertedProcessedTxs,
        AddressHistoryDB.Status.SLATED,
      );
    yield* AddressHistoryDB.upsertEntries(addressHistoryEntries);
    yield* MempoolLedgerDB.insert(collectiveProduced);
    yield* MempoolLedgerDB.clearUTxOs(collectiveSpent);

    return insertedTxRows.length;
  });

const insertProcessedTxsWithinTransaction = (
  sql: SqlClient.SqlClient,
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<number, SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return 0;
    }

    let totalInsertedRows = 0;
    const txChunks = chunkProcessedTxs([...processedTxs]);

    for (const [chunkIndex, txChunk] of txChunks.entries()) {
      const insertedChunkRows = yield* insertValidatedChunk(sql, txChunk);
      totalInsertedRows += insertedChunkRows;
      yield* Effect.logInfo(
        `${tableName} db: insertMultiple chunk ${chunkIndex + 1}/${txChunks.length} inserted_rows=${insertedChunkRows}`,
      );
    }

    return totalInsertedRows;
  });

const toProcessedTxFromPersistedEffects = (
  entry: EntryWithEffects & {
    [Columns.SPENT_OUTREFS]: readonly (Buffer | Uint8Array | string)[];
    [Columns.PRODUCED_OUTREFS]: readonly (Buffer | Uint8Array | string)[];
    [Columns.PRODUCED_OUTPUTS]: readonly (Buffer | Uint8Array | string)[];
    [Columns.PRODUCED_ADDRESSES]: readonly string[];
  },
): ProcessedTx => {
  const produced: Ledger.Entry[] = entry[Columns.PRODUCED_OUTREFS].map(
    (outRef, index) => ({
      [Ledger.Columns.TX_ID]: normalizeTxIdToBuffer(entry[Tx.Columns.TX_ID]),
      [Ledger.Columns.OUTREF]: decodeBytea(outRef),
      [Ledger.Columns.OUTPUT]: decodeBytea(
        entry[Columns.PRODUCED_OUTPUTS][index],
      ),
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
    return yield* sql.withTransaction(
      insertProcessedTxsWithinTransaction(sql, processedTxs),
    );
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

export const validateAndInsertMultiple = (
  candidates: readonly AcceptanceCandidate[],
): Effect.Effect<
  AcceptanceResult,
  SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    if (candidates.length === 0) {
      return {
        acceptedMessages: [],
        insertedCount: 0,
        rejected: [],
      };
    }

    const sql = yield* SqlClient.SqlClient;
    const nodeConfig = yield* NodeConfig;

    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SELECT pg_advisory_xact_lock(${TX_ACCEPTANCE_ADVISORY_LOCK_KEY})`;

        const queuedCandidates: Array<{
          arrivalSeq: bigint;
          message: TxIngressMessage;
          processedTx: ProcessedTx;
          txIdHex: string;
          queuedTx: SDK.QueuedTx;
        }> = [];
        const rejectedByArrivalSeq = new Map<bigint, AcceptanceRejection>();

        for (const candidate of candidates) {
          try {
            const cmlTx = CML.Transaction.from_cbor_bytes(candidate.processedTx.txCbor);
            const computedTxId = Buffer.from(
              CML.hash_transaction(cmlTx.body()).to_raw_bytes(),
            );
            queuedCandidates.push({
              arrivalSeq: candidate.arrivalSeq,
              message: candidate.message,
              processedTx: candidate.processedTx,
              txIdHex: computedTxId.toString("hex"),
              queuedTx: {
                txId: computedTxId,
                tx: SDK.cmlToMidgard(cmlTx),
                arrivalSeq: candidate.arrivalSeq,
              },
            });
          } catch (error) {
            rejectedByArrivalSeq.set(candidate.arrivalSeq, {
              message: candidate.message,
              reason: `validation_decode_failed:${error instanceof Error ? error.message : String(error)}`,
            });
          }
        }

        const phaseA = SDK.runPhaseAValidation(
          queuedCandidates.map((candidate) => candidate.queuedTx),
          defaultPhaseAConfig(nodeConfig),
        );
        const preState = decodeLedgerState(yield* MempoolLedgerDB.retrieve);
        const phaseB = SDK.runPhaseBValidationWithPatch(
          phaseA.accepted,
          preState,
          { nowMillis: Date.now() },
        );

        const acceptedArrivalSeqs = new Set(
          phaseB.accepted.map((accepted) => accepted.arrivalSeq),
        );
        const rejectionReasonsByTxId = new Map<string, string[]>();

        for (const rejection of [...phaseA.rejected, ...phaseB.rejected]) {
          const txHash = txIdHex(rejection.txId);
          const existing = rejectionReasonsByTxId.get(txHash) ?? [];
          existing.push(formatValidationReason(rejection));
          rejectionReasonsByTxId.set(txHash, existing);
        }

        const acceptedCandidates = queuedCandidates.filter((candidate) =>
          acceptedArrivalSeqs.has(candidate.arrivalSeq),
        );

        const insertedCount = yield* insertProcessedTxsWithinTransaction(
          sql,
          acceptedCandidates.map((candidate) => candidate.processedTx),
        );

        for (const candidate of queuedCandidates) {
          if (
            acceptedArrivalSeqs.has(candidate.arrivalSeq) ||
            rejectedByArrivalSeq.has(candidate.arrivalSeq)
          ) {
            continue;
          }
          const rejectionReasons =
            rejectionReasonsByTxId.get(candidate.txIdHex) ?? [];
          rejectedByArrivalSeq.set(candidate.arrivalSeq, {
            message: candidate.message,
            reason:
              rejectionReasons.shift() ??
              "validation_rejected:E_UNSPECIFIED",
          });
        }

        return {
          acceptedMessages: acceptedCandidates.map((candidate) => candidate.message),
          insertedCount,
          rejected: Array.from(rejectedByArrivalSeq.values()),
        };
      }),
    );
  }).pipe(
    Effect.withLogSpan(`validateAndInsert ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to validate and insert the given transactions",
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
  retrieveTimeBoundEntriesLimited(startTime, endTime);

export const retrieveTimeBoundEntriesLimited = (
  startTime: Date,
  endTime: Date,
  limit?: number,
): Effect.Effect<readonly EntryWithEffects[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const limitClause =
      limit !== undefined ? sql`LIMIT ${sql.literal(String(limit))}` : sql``;
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
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC
      ${limitClause}`;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntriesLimited ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve time-bound mempool transactions",
    ),
  );

export const retrieveEntriesBeforeTime = (endTime: Date) =>
  retrieveEntriesBeforeTimeLimited(endTime);

export const retrieveEntriesBeforeTimeLimited = (
  endTime: Date,
  limit?: number,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const limitClause =
      limit !== undefined ? sql`LIMIT ${sql.literal(String(limit))}` : sql``;
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
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC
      ${limitClause}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesBeforeTimeLimited ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve mempool transactions before the given time",
    ),
  );

export const countTimeBoundEntries = (startTime: Date, endTime: Date) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      count: bigint | string;
    }>`SELECT COUNT(*)::bigint AS count
      FROM ${sql(tableName)}
      WHERE ${startTime} <= ${sql(Tx.Columns.TIMESTAMPTZ)}
      AND ${sql(Tx.Columns.TIMESTAMPTZ)} < ${endTime}`;
    if (rows.length === 0) {
      return 0;
    }
    const raw = rows[0].count;
    return typeof raw === "bigint" ? Number(raw) : Number(raw);
  }).pipe(
    Effect.withLogSpan(`countTimeBoundEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count time-bound mempool transactions",
    ),
  );

export const countEntriesBeforeTime = (endTime: Date) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      count: bigint | string;
    }>`SELECT COUNT(*)::bigint AS count
      FROM ${sql(tableName)}
      WHERE ${sql(Tx.Columns.TIMESTAMPTZ)} < ${endTime}`;
    if (rows.length === 0) {
      return 0;
    }
    const raw = rows[0].count;
    return typeof raw === "bigint" ? Number(raw) : Number(raw);
  }).pipe(
    Effect.withLogSpan(`countEntriesBeforeTime ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count mempool transactions before the given time",
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
