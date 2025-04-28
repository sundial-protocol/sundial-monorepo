import { Option } from "effect";
import { Sql } from "postgres";
import {
  clearTable,
  insertKeyValue,
  insertKeyValues,
  retrieveKeyValues,
  retrieveValue,
  retrieveValues,
} from "./utils.js";

export const tableName = "immutable";

export const insert = async (
  sql: Sql,
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Promise<void> => insertKeyValue(sql, tableName, txHash, txCbor);

export const insertTxs = async (
  sql: Sql,
  txs: { key: Uint8Array; value: Uint8Array }[],
): Promise<void> => insertKeyValues(sql, tableName, txs);

export const retrieve = async (
  sql: Sql,
): Promise<{ key: Uint8Array; value: Uint8Array }[]> =>
  retrieveKeyValues(sql, tableName);

export const retrieveTxCborByHash = async (
  sql: Sql,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => retrieveValue(sql, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  sql: Sql,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> => retrieveValues(sql, tableName, txHashes);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
