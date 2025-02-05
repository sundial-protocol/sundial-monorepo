import {
  logAbort,
  logInfo,
} from "../utils.js";
import sqlite3 from "sqlite3";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool (
    tx_hash BLOB NOT NULL UNIQUE,
    tx_cbor BLOB NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
    FOREIGN KEY (tx_hash)
      REFERENCES blocks(tx_hash)
      ON DELETE CASCADE
  );`;

export const insert = async (
  db: sqlite3.Database,
  tx_hash: string,
  tx_cbor: string
) => {
  const query = `INSERT INTO mempool (tx_hash, tx_cbor) VALUES (?, ?)`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [tx_hash, tx_cbor], function (err) {
      if (err) {
        logAbort(`mempool db: error inserting tx: ${err.message}`);
        reject(err);
      } else {
        logInfo(`mempool db: tx stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
};

export const retrieve = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM mempool`;
  const mempool = await new Promise<[string, string][]>((resolve, reject) => {
    db.all(query, (err, rows: [string, string][]) => {
      if (err) {
        logAbort(`mempool db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return mempool;
};

export const clear = async (db: sqlite3.Database) => clearTable(db, "mempool");