import { Option } from "effect";
import { Pool, PoolClient } from "pg";

export const mkKeyValueCreateQuery = (tableName: string) => `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    key BYTEA NOT NULL,
    value BYTEA NOT NULL,
    PRIMARY KEY (key)
  );`;

export const delMultiple = async (
  pool: Pool | PoolClient,
  tableName: string,
  keys: Uint8Array[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE key IN (${keys
    .map((_, i) => `$${i + 1}`)
    .join(", ")})`;
  try {
    await pool.query(query, keys);
  } catch (err) {
    throw err;
  }
};

export const retrieveValue = async (
  pool: Pool | PoolClient,
  tableName: string,
  key: Uint8Array,
): Promise<Option.Option<Uint8Array>> => {
  const query = `SELECT value FROM ${tableName} WHERE key = $1`;
  try {
    const result = await pool.query(query, [key]);
    if (result.rows.length > 0) {
      return Option.some(result.rows[0].value);
    } else {
      return Option.none();
    }
  } catch (err) {
    throw err;
  }
};

export const retrieveValues = async (
  pool: Pool | PoolClient,
  tableName: string,
  keys: Uint8Array[],
): Promise<Uint8Array[]> => {
  const query = `SELECT value FROM ${tableName} WHERE key = ANY($1)`;
  try {
    const result = await pool.query(query, [keys]);
    return result.rows.map((row) => row.value);
  } catch (err) {
    throw err;
  }
};

export const clearTable = async (
  pool: Pool | PoolClient,
  tableName: string,
): Promise<void> => {
  const query = `TRUNCATE TABLE ${tableName} CASCADE;`;

  try {
    await pool.query(query);
  } catch (err) {
    throw err;
  }
};

export const insertKeyValues = async (
  pool: Pool | PoolClient,
  tableName: string,
  utxosCBOR: { key: Uint8Array; value: Uint8Array }[],
): Promise<void> => {
  const query = `
  INSERT INTO ${tableName} (key, value)
  VALUES
  ${utxosCBOR.map((_, i) => `($${i * 2 + 1}, $${i * 2 + 2})`).join(", ")}
  `;
  const values = utxosCBOR.flatMap((u) => [u.key, u.value]);
  await pool.query(query, values);
};

export const retrieveKeyValues = async (
  pool: Pool | PoolClient,
  tableName: string,
): Promise<{ key: Uint8Array; value: Uint8Array }[]> => {
  const query = `SELECT * FROM ${tableName}`;
  const rows = await pool.query(query);
  const result: { key: Uint8Array; value: Uint8Array }[] = rows.rows.map(
    (r) => ({
      key: r.key,
      value: r.value,
    }),
  );
  return result;
};
