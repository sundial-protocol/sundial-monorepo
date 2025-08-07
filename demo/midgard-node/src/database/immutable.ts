import { Effect } from "effect";
import {
  TxEntries,
  clearTable,
  insertTx,
  insertTxs,
  retrieveValue,
  retrieveValues,
  retrieveTxEntries,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "immutable";

export const insertTransaction = (tx: TxEntries
): Effect.Effect<void, Error, Database> =>
  insertTx(tableName, tx);

export const insertTransactions = (
  txs: TxEntries[],
): Effect.Effect<void, Error, Database> => insertTxs(tableName, txs);

export const retrieve = () => retrieveTxEntries(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
