import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS immutable (
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
  const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES (?, ?)`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [tx_hash, tx_cbor], function (err) {
      if (err) {
        logAbort(`immutable db: error inserting tx: ${err.message}`);
        reject(err);
      } else {
        logInfo(`immutable db: tx stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
};

export const retrieve = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM immutable`;
  const mempool = await new Promise<[string, string][]>((resolve, reject) => {
    db.all(query, (err, rows: [string, string][]) => {
      if (err) {
        logAbort(`immutable db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return mempool;
};

export const clear = async (db: sqlite3.Database) => clearTable(db, "immutable");