import { Database } from "@/services/database.js";
import * as Tx from "@/database/utils/tx.js";
import { clearTable, mapSqlError } from "@/database/utils/common.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Effect, Queue } from "effect";
import { fromHex } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { breakDownTx } from "@/utils.js";

export const tableName = "mempool";


export class MempoolQueue extends Effect.Service<MempoolQueue>()("MempoolQueue", {
  effect: Effect.gen(function* () {
    const queue = yield* Queue.unbounded<string>();

    return {
      queue
    };
  }),
}) {}


const incomingQueueEffect = Queue.unbounded<string>()

export const addToQueue = (txString: string) =>
  Effect.gen(function* () {
    const mQueue = yield* MempoolQueue
    mQueue.queue.offer(txString)
})

export const init = Effect.gen(function* () {
  yield* Effect.logInfo(`  Init MempoolDB`)
  yield* Tx.createTable(tableName)
  yield* Effect.forkDaemon(
    Effect.gen(function* () {
      yield* Effect.logInfo(`  Start daemon`);
      const mQueue = yield* MempoolQueue
      Effect.forever(Effect.gen(function* () {
        const txString = yield* mQueue.queue.take
        yield* Effect.logInfo(`  Took data from the incomingQueueEffect`);
        yield* insert(txString)
      }))
    })
  )
});

export const insert = (
  txString: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const txCborBytes = fromHex(txString);
    const { txId, txCbor, spent, produced } = yield* breakDownTx(txCborBytes);
    // Insert the tx itself in `MempoolDB`.
    yield* Tx.insertEntry(tableName, {
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
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  Tx.retrieveValues(tableName, txHashes);

export const retrieve = (): Effect.Effect<
  readonly Tx.Entry[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Tx.Entry>`SELECT ${sql(
      Tx.Columns.TX_ID,
    )}, ${sql(Tx.Columns.TX)} FROM ${sql(tableName)} LIMIT 100000`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveTxCount = () => Tx.retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, Error, Database> => Tx.delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
