import { Option } from "effect";
import { Sql } from "postgres";
import {
  clearTable,
  retrieveValue,
  retrieveValues,
  delMultiple,
} from "./utils.js";

export const tableName = "mempool";

export const insert = async (
  sql: Sql,
  txHash: Uint8Array,
  txCbor: Uint8Array
): Promise<void> => {
  try {
    await sql`INSERT INTO ${sql(tableName)} ${sql({
      key: txHash,
      value: txCbor,
    })} ON CONFLICT (key) DO UPDATE SET value = ${txCbor}`;
  } catch (err) {
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  sql: Sql,
  txHash: Uint8Array
): Promise<Option.Option<Uint8Array>> => retrieveValue(sql, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  sql: Sql,
  txHashes: Uint8Array[]
): Promise<Uint8Array[]> => retrieveValues(sql, tableName, txHashes);

export const retrieve = async (
  sql: Sql
): Promise<{ key: Uint8Array; value: Uint8Array }[]> => {
  try {
    const result = await sql`SELECT * FROM ${sql(tableName)}`;
    return result.map((row) => ({
      key: row.key,
      value: row.value,
    }));
  } catch (err) {
    throw err;
  }
};

export const clearTxs = async (sql: Sql, txHashes: Uint8Array[]) =>
  delMultiple(sql, tableName, txHashes);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
