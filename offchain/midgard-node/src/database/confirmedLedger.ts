import { UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import { clearTable, insertUtxos, retrieveUtxos } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type TEXT (8),
    script_ref_script TEXT,
    PRIMARY KEY (tx_hash, output_index)
      FOREIGN KEY (tx_hash)
      REFERENCES blocks(tx_hash)
      ON DELETE CASCADE
  );
  CREATE TABLE IF NOT EXISTS confirmed_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit TEXT,
    quantity BIGINT NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES confirmed_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );
  `;

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUtxos(db, "confirmed_ledger", "confirmed_ledger_assets", utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> =>
  retrieveUtxos(db, "confirmed_ledger", "confirmed_ledger_assets");

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "confirmed_ledger");
