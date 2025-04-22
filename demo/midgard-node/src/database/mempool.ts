import { Option } from "effect";
import { Pool } from "pg";
import {
  clearTable,
  mkKeyValueCreateQuery,
  retrieveValue,
  retrieveValues,
  delMultiple,
} from "./utils.js";

export const tableName = "mempool";

export const createQuery = mkKeyValueCreateQuery(tableName);

export const insert = async (
  pool: Pool,
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Promise<void> => {
  const query = `INSERT INTO ${tableName} (key, value) VALUES ($1, $2)`;
  try {
    await pool.query(query, [txHash, txCbor]);
  } catch (err) {
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => retrieveValue(pool, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> => retrieveValues(pool, tableName, txHashes);

export const retrieve = async (
  pool: Pool,
): Promise<{ key: Uint8Array; value: Uint8Array }[]> => {
  const query = `SELECT * FROM ${tableName}`;
  try {
    const result = await pool.query(query);
    return result.rows.map((row) => ({
      key: row.key,
      value: row.value,
    }));
  } catch (err) {
    throw err;
  }
};

export const clearTxs = async (pool: Pool, txHashes: Uint8Array[]) =>
  delMultiple(pool, tableName, txHashes);

export const clear = async (pool: Pool) => clearTable(pool, tableName);
