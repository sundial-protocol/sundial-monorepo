import { Effect } from "effect";
import * as Tx from "@/database/utils/tx.js";
import { clearTable } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";
import { DatabaseError } from "./utils/common.js";

export const tableName = "immutable";

export const insertTx = (
  tx: Tx.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntry(tableName, tx);

export const insertTxs = (
  txs: Tx.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntries(tableName, txs);

export const retrieve = () => Tx.retrieveEntries(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => Tx.retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
