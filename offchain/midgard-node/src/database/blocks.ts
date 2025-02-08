import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";
import { clearTable } from "./utils.js";
import { Option } from "effect";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS blocks (
    header_hash BLOB NOT NULL,
    tx_hash BLOB NOT NULL UNIQUE
  );`;

export const insert = async (
  db: sqlite3.Database,
  header_hash: string,
  tx_hashes: string[]
): Promise<void> => {
  const query = `
    INSERT INTO blocks (header_hash, tx_hash)
    VALUES
    ${tx_hashes.map(() => `(?, ?)`).join(", ")}`;
  const values = tx_hashes.flatMap((tx_hash) => [header_hash, tx_hash]);
  await new Promise<void>((resolve, reject) => {
    db.run(query, values, function (err) {
      if (err) {
        logAbort(`blocks db: inserting error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`blocks db: ${tx_hashes.length} new tx_hashes added`);
        resolve();
      }
    });
  });
};

export const retrieveTxHashesByBlockHash = async (
  db: sqlite3.Database,
  blockHash: string
): Promise<string[]> => {
  const query = `SELECT tx_hash FROM blocks WHERE header_hash = ?`;
  const txHashes = await new Promise<string[]>((resolve, reject) => {
    db.all(query, [blockHash], (err, rows: string[]) => {
      if (err) {
        logAbort(`blocks db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return txHashes;
};

export const retrieveBlockHashByTxHash = async (
  db: sqlite3.Database,
  txHash: string
): Promise<Option.Option<string>> => {
  const query = `SELECT header_hash FROM blocks WHERE tx_hash = ?`;
  const blockHash = await new Promise<string[]>((resolve, reject) => {
    db.all(query, [txHash], (err, rows: string[]) => {
      if (err) {
        logAbort(`blocks db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return Option.fromIterable(blockHash);
};

export const clearBlock = async (
  db: sqlite3.Database,
  blockHash: string
): Promise<void> => {
  const query = `DELETE from blocks WHERE header_hash = ?`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [blockHash], function (err) {
      if (err) {
        logAbort(`blocks db: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`blocks db: cleared`);
        resolve();
      }
    });
  });
};

export const retrieve = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM blocks`;
  const blocks = await new Promise<[string, string][]>((resolve, reject) => {
    db.all(query, (err, rows: [string, string][]) => {
      if (err) {
        logAbort(`blocks db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return blocks;
};

export const clear = async (db: sqlite3.Database) => clearTable(db, `blocks`);
