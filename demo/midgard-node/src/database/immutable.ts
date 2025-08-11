import { Effect } from "effect";
import * as TxUtils from "@/database/utils/tx.js";
import { clearTable } from "@/database/utils/common.js"
import { Database } from "@/services/database.js";

export const tableName = "immutable";

export const insertTransaction = (tx: TxUtils.Entry
): Effect.Effect<void, Error, Database> =>
  TxUtils.insertEntry(tableName, tx);

export const insertTransactions = (
  txs: TxUtils.Entry[],
): Effect.Effect<void, Error, Database> => TxUtils.insertEntries(tableName, txs);

export const retrieve = () => TxUtils.retrieveEntries(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  TxUtils.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => TxUtils.retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
