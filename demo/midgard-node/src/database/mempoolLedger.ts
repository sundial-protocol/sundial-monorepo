import { Pool } from "pg";
import {
  clearTable,
  mkKeyValueCreateQuery,
  insertKeyValues,
  retrieveKeyValues,
  delMultiple,
} from "./utils.js";

export const tableName = "mempool_ledger";

export const createQuery = mkKeyValueCreateQuery(tableName);

export const insert = async (
  pool: Pool,
  utxosCBOR: { key: Uint8Array; value: Uint8Array }[],
) => insertKeyValues(pool, tableName, utxosCBOR);

export const retrieve = async (
  pool: Pool,
): Promise<{ key: Uint8Array; value: Uint8Array }[]> =>
  retrieveKeyValues(pool, tableName);

export const clearUTxOs = async (pool: Pool, refs: Uint8Array[]) =>
  delMultiple(pool, tableName, refs);

export const clear = async (pool: Pool) => clearTable(pool, tableName);
