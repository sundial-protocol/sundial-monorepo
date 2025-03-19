import { OutRef, toHex } from "@lucid-evolution/lucid";
import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import * as blocks from "./blocks.js";
import * as confirmedLedger from "./confirmedLedger.js";
import * as immutable from "./immutable.js";
import * as latestLedger from "./latestLedger.js";
import * as mempool from "./mempool.js";
import * as mempoolLedger from "./mempoolLedger.js";

export async function initializeDb(pool: Pool) {
  try {
    await pool.query(`
      SET default_transaction_isolation TO 'serializable';
    `);

    await pool.query(blocks.createQuery);
    await pool.query(mempool.createQuery);
    await pool.query(mempoolLedger.createQuery);
    await pool.query(immutable.createQuery);
    await pool.query(confirmedLedger.createQuery);
    await pool.query(latestLedger.createQuery);

    logInfo("Connected to the PostgreSQL database");
    return pool;
  } catch (err) {
    logAbort(`Error initializing database: ${err}`);
    throw err;
  }
}

export const clearUTxOs = async (
  pool: Pool,
  tableName: string,
  refs: OutRef[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE (tx_hash, output_index) IN (${refs
    .map((_, i) => `($${i * 2 + 1}, $${i * 2 + 2})`)
    .join(", ")})`;
  const values = refs.flatMap((r) => [
    Buffer.from(r.txHash, "hex"),
    r.outputIndex,
  ]);

  try {
    const result = await pool.query(query, values);
    // logInfo(`${tableName} db: ${result.rowCount} utxos removed`);
  } catch (err) {
    // logAbort(`${tableName} db: utxos removing error: ${err}`);
    throw err;
  }
};

export const clearTxs = async (
  pool: Pool,
  tableName: string,
  txHashes: string[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE tx_hash IN (${txHashes
    .map((_, i) => `$${i + 1}`)
    .join(", ")})`;
  const values = txHashes.flatMap((h) => [Buffer.from(h, "hex")]);
  try {
    const result = await pool.query(query, values);
    logInfo(`${tableName} db: ${result.rowCount} txs removed`);
  } catch (err) {
    logAbort(`${tableName} db: txs removing error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  tableName: string,
  txHash: string,
): Promise<Option.Option<Uint8Array>> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = $1`;
  try {
    const result = await pool.query(query, [Buffer.from(txHash, "hex")]);
    if (result.rows.length > 0) {
      return Option.some(result.rows[0].tx_cbor);
    } else {
      return Option.none();
    }
  } catch (err) {
    // logAbort(`db: retrieving error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  tableName: string,
  txHashes: string[],
): Promise<Uint8Array[]> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = ANY($1)`;
  try {
    const result = await pool.query(query, [
      txHashes.map((hash) => Buffer.from(hash, "hex")),
    ]);
    return result.rows.map((row) => row.tx_cbor);
  } catch (err) {
    // logAbort(`${tableName} db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearTable = async (
  pool: Pool,
  tableName: string,
): Promise<void> => {
  const query = `TRUNCATE TABLE ${tableName} CASCADE;`;

  try {
    await pool.query(query);
    // logInfo(`${tableName} db: cleared`);
  } catch (err) {
    // logAbort(`${tableName} db: clearing error: ${err}`);
    throw err;
  }
};

export const insertUTxOsCBOR = async (
  pool: Pool,
  tableName: string,
  utxosCBOR: { outRef: OutRef; utxoCBOR: Uint8Array }[],
): Promise<void> => {
  const query = `
  INSERT INTO ${tableName} (tx_hash, output_index, utxo_cbor)
  VALUES
  ${utxosCBOR.map((_, i) => `($${i * 3 + 1}, $${i * 3 + 2}, $${i * 3 + 3})`).join(", ")}
  `;
  const values = utxosCBOR.flatMap((u) => [
    Buffer.from(u.outRef.txHash, "hex"),
    u.outRef.outputIndex,
    Buffer.from(u.utxoCBOR),
  ]);
  await pool.query(query, values);
};

export const retrieveUTxOsCBOR = async (
  pool: Pool,
  tableName: string,
): Promise<{ outRef: OutRef; utxoCBOR: Uint8Array }[]> => {
  const query = `SELECT * FROM ${tableName}`;
  const rows = await pool.query(query);
  const result: { outRef: OutRef; utxoCBOR: Uint8Array }[] = rows.rows.map(
    (r) => ({
      outRef: { txHash: toHex(r.tx_hash), outputIndex: r.output_index },
      utxoCBOR: Buffer.from(r.utxo_cbor),
    }),
  );
  return result;
};
