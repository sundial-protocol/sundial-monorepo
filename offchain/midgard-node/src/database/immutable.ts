import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS archive_block (
    tx_hash BLOB NOT NULL UNIQUE,
    block_hash BLOB NOT NULL,
    PRIMARY KEY (tx_hash)
  );
  CREATE TABLE IF NOT EXISTS archive_tx (
    tx_hash BLOB NOT NULL UNIQUE,
    tx_cbor BLOB NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
    FOREIGN KEY (tx_hash)
      REFERENCES archive_block(tx_hash)
      ON DELETE CASCADE
  );`;

export const insert = async (
  db: sqlite3.Database,
  blockHash: string,
  txs: ArchiveTxRow[]
) => {
  const blockQuery = `
      INSERT INTO archive_block (tx_hash, block_hash) VALUES
      ${txs.map((tx) => `(?, ?)`).join(", ")}
    `;
  const blockValues = txs.flatMap((tx) => [tx.tx_hash, blockHash]);
  await new Promise<void>((resolve, reject) => {
    db.run(blockQuery, blockValues, function (err) {
      if (err) {
        logAbort(`Archive: error inserting block: ${err.message}`);
        reject(err);
      } else {
        logInfo(`Archive: block stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
  const txQuery = `
      INSERT INTO archive_tx (tx_hash, tx_cbor) VALUES
      ${txs.map((_) => `(?, ?)`).join(", ")}
      `;
  db.run(
    txQuery,
    txs.flatMap((tx) => [tx.tx_hash, tx.tx_cbor]),
    function (err) {
      if (err) {
        logAbort(`Archive: error inserting txs: ${err.message}`);
      } else {
        logInfo(
          `Archive: ${txs.length} txs stored, last rowid: ${this.lastID}`
        );
      }
    }
  );
};

export const retrieve = async (db: sqlite3.Database) => {
  const query = `
    SELECT *
    FROM archive_block
    JOIN archive_tx ON archive_block.tx_hash = archive_tx.tx_hash;`;
  return new Promise<ArchiveRow[]>((resolve, reject) => {
    db.all(query, (err, rows: ArchiveRow[]) => {
      if (err) {
        logAbort(`Error retrieving archive: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
};

export const clear = (db: sqlite3.Database): Promise<void> => {
  const query = `DELETE FROM archive_block;`;
  return new Promise((resolve, reject) => {
    db.run(query, (err) => {
      if (err) {
        logAbort(`Archive: clearing error: ${err.message}`);
        return reject(err);
      } else {
        logInfo(`Archive: cleared`);
        resolve();
      }
    });
  });
};

export interface ArchiveTxRow {
  tx_hash: string;
  tx_cbor: string;
}

export interface AchiveBlockRow {
  block_hash: string;
  tx_hash: string;
}

export interface ArchiveRow {
  block_hash: string;
  tx_hash: string;
  tx_cbor: string;
}
