import { UTxO } from "@lucid-evolution/lucid";
import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";
import { clearTable, insertUtxosIntoTable, retrieveUtxosFromTable, utxoToRow } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    assets TEXT,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type TEXT (8),
    script_ref_script TEXT,
    PRIMARY KEY (tx_hash, output_index)
      FOREIGN KEY (tx_hash)
      REFERENCES blocks(tx_hash)
      ON DELETE CASCADE
  );`;


export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUtxosIntoTable(db, "confirmed_ledger", utxos)

export const retrieve = async (
  db: sqlite3.Database
): Promise<UTxO[]> => {
  return retrieveUtxosFromTable(db, "confirmed_ledger");
};

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "confirmed_ledger");
