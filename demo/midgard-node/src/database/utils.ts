import { Option } from "effect";
import { Sql } from "postgres";

export const mkKeyValueCreateQuery = (sql: Sql, tableName: string) => sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    key BYTEA NOT NULL,
    value BYTEA NOT NULL,
    PRIMARY KEY (key)
  );`;

export const delMultiple = async (
  sql: Sql,
  tableName: string,
  keys: Uint8Array[]
): Promise<void> => {
  try {
    await sql`DELETE FROM ${sql(tableName)} WHERE key IN (${keys})`;
  } catch (err) {
    throw err;
  }
};

export const retrieveValue = async (
  sql: Sql,
  tableName: string,
  key: Uint8Array
): Promise<Option.Option<Uint8Array>> => {
  try {
    const result =
      await sql`SELECT value FROM ${sql(tableName)} WHERE key = ${key}`;
    if (result.length > 0) {
      return Option.some(result[0].value);
    } else {
      return Option.none();
    }
  } catch (err) {
    throw err;
  }
};

export const retrieveValues = async (
  sql: Sql,
  tableName: string,
  keys: Uint8Array[]
): Promise<Uint8Array[]> => {
  try {
    const result =
      await sql`SELECT value FROM ${sql(tableName)} WHERE key = ANY(${keys})`;
    return result.map((row) => row.value);
  } catch (err) {
    throw err;
  }
};

export const clearTable = async (
  sql: Sql,
  tableName: string
): Promise<void> => {
  try {
    await sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;
  } catch (err) {
    throw err;
  }
};

export const insertKeyValues = async (
  sql: Sql,
  tableName: string,
  utxosCBOR: { key: Uint8Array; value: Uint8Array }[]
): Promise<void> => {
  if (utxosCBOR.length === 0) {
    return;
  }
  await sql`INSERT INTO ${sql(tableName)} (key, value) VALUES ${sql(utxosCBOR)}`;
};

export const retrieveKeyValues = async (
  sql: Sql,
  tableName: string
): Promise<{ key: Uint8Array; value: Uint8Array }[]> => {
  const rows = await sql`SELECT * FROM ${sql(tableName)}`;
  return rows.map((row) => ({
    key: row.key,
    value: row.value,
  }));
};
