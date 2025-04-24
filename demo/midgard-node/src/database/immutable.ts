import { Option } from "effect";
import { Sql } from "postgres";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const tableName = "immutable";

export const insert = async (
  sql: Sql,
  txHash: Uint8Array,
  txCbor: Uint8Array
): Promise<void> => {
  try {
    await sql`INSERT INTO ${sql(tableName)} (key, value) VALUES (${txHash}, ${txCbor})`;
  } catch (err) {
    throw err;
  }
};

export const insertTxs = async (
  sql: Sql,
  txs: { key: Uint8Array; value: Uint8Array }[]
): Promise<void> => {
  try {
    if (txs.length === 0) {
      return;
    }
    await sql`INSERT INTO ${sql(tableName)} ${sql(txs)}`;
  } catch (err) {
    throw err;
  }
};

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

export const retrieveTxCborByHash = async (
  sql: Sql,
  txHash: Uint8Array
): Promise<Option.Option<Uint8Array>> =>
  utils.retrieveValue(sql, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  sql: Sql,
  txHashes: Uint8Array[]
): Promise<Uint8Array[]> => utils.retrieveValues(sql, tableName, txHashes);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
