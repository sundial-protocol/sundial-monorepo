import { Sql } from "postgres";
import {
  clearTable,
  insertKeyValues,
  retrieveKeyValues,
  delMultiple,
} from "./utils.js";

export const tableName = "latest_ledger";

export const insert = async (
  sql: Sql,
  utxosCBOR: { key: Uint8Array; value: Uint8Array }[],
) => insertKeyValues(sql, tableName, utxosCBOR);

export const retrieve = async (
  sql: Sql,
): Promise<{ key: Uint8Array; value: Uint8Array }[]> =>
  retrieveKeyValues(sql, tableName);

export const clearUTxOs = async (sql: Sql, refs: Uint8Array[]) =>
  delMultiple(sql, tableName, refs);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
