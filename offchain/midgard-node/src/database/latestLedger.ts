import { Address, fromHex, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import {
  clearTable,
  insertUtxos,
  retrieveUtxos,
} from "./utils.js";
import * as utils from "./utils.js";
import { logAbort, logInfo } from "../utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS latest_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type VARCHAR (8),
    script_ref_script BLOB,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS latest_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit BLOB,
    quantity BIGINT NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES latest_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );
  `;

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUtxos(db, "latest_ledger", "latest_ledger_assets", utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> =>
  retrieveUtxos(db, "latest_ledger", "latest_ledger_assets");

export const retrieveUtxosOnAddress = async (
  db: sqlite3.Database,
  address: Address
): Promise<UTxO[]> =>
  utils.retrieveUtxosOnAddress(
    db,
    "latest_ledger",
    "latest_ledger_assets",
    address
  );

export const clearTx = async (
  db: sqlite3.Database,
  txHash: string
): Promise<void> => {
  const query = `DELETE FROM latest_ledger WHERE tx_hash = ?;`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [fromHex(txHash)], function (err) {
      if (err) {
        logAbort(`latest_ledger db: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`latest_ledger db: cleared`);
        resolve();
      }
    });
  });
};

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "latest_ledger");
