import { Effect } from "effect";
import {
  KVColumns,
  KVEntries,
  clearTable,
  insertKeyValue,
  insertKeyValues,
  retrieveValue,
  retrieveValues,
  retrieveKVEntries,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "immutable";

export const insert = (tx: Omit<KVEntries, KVColumns.TIMESTAMPTZ>
): Effect.Effect<void, Error, Database> =>
  insertKeyValue(tableName, tx);

export const insertTxs = (
  txs: Omit<KVEntries, KVColumns.TIMESTAMPTZ>[],
): Effect.Effect<void, Error, Database> => insertKeyValues(tableName, txs);

export const retrieve = () => retrieveKVEntries(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
