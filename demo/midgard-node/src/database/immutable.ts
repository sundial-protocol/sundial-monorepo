import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const tableName = "immutable";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    key BYTEA NOT NULL UNIQUE,
    value BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (key)
  );
`;

export const insert = async (
  pool: Pool,
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Promise<void> => {
  const query = `INSERT INTO ${tableName} (key, value) VALUES ($1, $2)`;
  try {
    await pool.query(query, [txHash, txCbor]);
    logInfo(`${tableName} db: tx stored`);
  } catch (err) {
    logAbort(`${tableName} db: error inserting tx: ${err}`);
    throw err;
  }
};

export const insertTxs = async (
  pool: Pool,
  txs: { key: Uint8Array; value: Uint8Array }[],
): Promise<void> => {
  const query = `INSERT INTO ${tableName} (key, value) VALUES ($1, $2)`;
  try {
    for (const { key, value } of txs) {
      await pool.query(query, [key, value]);
    }
  } catch (err) {
    throw err;
  }
};

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

export const retrieveTxCborByHash = async (
  pool: Pool,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> =>
  utils.retrieveValue(pool, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> => utils.retrieveValues(pool, tableName, txHashes);

export const clear = async (pool: Pool) => clearTable(pool, tableName);
