import { Option } from "effect";
import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool (
    tx_hash BYTEA NOT NULL UNIQUE,
    tx_cbor BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );`;

export const insert = async (
  pool: Pool,
  txHash: string,
  txCbor: Uint8Array,
): Promise<void> => {
  const query = `INSERT INTO mempool (tx_hash, tx_cbor) VALUES ($1, $2)`;
  try {
    await pool.query(query, [Buffer.from(txHash, "hex"), Buffer.from(txCbor)]);
    // logInfo(`mempool db: tx stored`);
  } catch (err) {
    // logAbort(`mempool db: error inserting tx: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  txHash: string,
): Promise<Option.Option<Uint8Array>> =>
  utils.retrieveTxCborByHash(pool, "mempool", txHash);

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  txHashes: string[],
): Promise<Uint8Array[]> =>
  utils.retrieveTxCborsByHashes(pool, "mempool", txHashes);

export const retrieve = async (
  pool: Pool,
): Promise<{ txHash: string; txCbor: string }[]> => {
  const query = `SELECT * FROM mempool`;
  try {
    const result = await pool.query(query);
    return result.rows.map((row) => ({
      txHash: row.tx_hash.toString("hex"),
      txCbor: row.tx_cbor.toString("hex"),
    }));
  } catch (err) {
    // logAbort(`mempool db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearTxs = async (pool: Pool, txHashes: string[]) =>
  utils.clearTxs(pool, "mempool", txHashes);

export const clear = async (pool: Pool) => clearTable(pool, "mempool");
