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
    const headerHashBuffer: Buffer = Buffer.from(headerHash);
    const pairs = txHashes.map((txHash) => ({
      header_hash: headerHashBuffer,
      tx_hash: Buffer.from(txHash),
    }));
    await sql`INSERT INTO ${sql(tableName)} ${sql(
      pairs,
    )} ON CONFLICT (tx_hash) DO NOTHING`;
  } catch (err) {
    throw err;
  }
};

export const retrieveTxHashesByBlockHash = async (
  sql: Sql,
  blockHash: Uint8Array,
): Promise<Uint8Array[]> => {
  try {
    const result = await sql`SELECT tx_hash FROM ${sql(
      tableName,
    )} WHERE header_hash = ${Buffer.from(blockHash)}`;
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
    const result = await sql`SELECT header_hash FROM ${sql(
      tableName,
    )} WHERE tx_hash = ${Buffer.from(txHash)}`;
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
    await sql`DELETE FROM ${sql(tableName)} WHERE header_hash = ${Buffer.from(blockHash)}`;
  } catch (err) {
    throw err;
  }
};

export const retrieve = async (
  sql: Sql,
): Promise<[Uint8Array, Uint8Array][]> => {
  try {
    const result = await sql`SELECT * FROM ${sql(tableName)}`;
    return result.map((row) => [
      Uint8Array.from(row.header_hash),
      Uint8Array.from(row.tx_hash),
    ]);
  } catch (err) {
    throw err;
  }
};

export const clear = async (sql: Sql) => clearTable(sql, tableName);
