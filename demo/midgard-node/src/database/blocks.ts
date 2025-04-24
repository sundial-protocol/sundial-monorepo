import { Option } from "effect";
import { Sql } from "postgres";
import { clearTable } from "./utils.js";

export const tableName = "blocks";

export const createQuery = (sql: Sql) => sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    header_hash BYTEA NOT NULL,
    tx_hash BYTEA NOT NULL UNIQUE
  );`;

export const insert = async (
  sql: Sql,
  headerHash: Uint8Array,
  txHashes: Uint8Array[],
): Promise<void> => {
  try {
    await sql`INSERT INTO ${sql(tableName)} ${sql(
      txHashes.map((txHash) => ({header_hash: headerHash, tx_hash: txHash})),
    )} ON CONFLICT (tx_hash) DO UPDATE SET header_hash = ${headerHash}`;
  } catch (err) {
    throw err;
  }
};

export const retrieveTxHashesByBlockHash = async (
  sql: Sql,
  blockHash: Uint8Array,
): Promise<Uint8Array[]> => {
  try {
    const result = await sql`SELECT tx_hash FROM ${sql(tableName)} WHERE header_hash = ${blockHash}`;
    return result.map((row) => row.tx_hash);
  } catch (err) {
    throw err;
  }
};

export const retrieveBlockHashByTxHash = async (
  sql: Sql,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => {
  try {
    const result = await sql`SELECT header_hash FROM ${sql(tableName)} WHERE tx_hash = ${txHash}`;
    if (result.length > 0) {
      return Option.some(result[0].header_hash);
    } else {
      return Option.none();
    }
  } catch (err) {
    throw err;
  }
};

export const clearBlock = async (
  sql: Sql,
  blockHash: Uint8Array,
): Promise<void> => {
  try {
    await sql`DELETE FROM ${sql(tableName)} WHERE header_hash = ${blockHash}`;
  } catch (err) {
    throw err;
  }
};

export const retrieve = async (
  sql: Sql,
): Promise<[Uint8Array, Uint8Array][]> => {
  try {
    const result = await sql`SELECT * FROM ${sql(tableName)}`;;
    return result.map((row) => [row.header_hash, row.tx_hash]);
  } catch (err) {
    throw err;
  }
};

export const clear = async (sql: Sql) => clearTable(sql, tableName);
