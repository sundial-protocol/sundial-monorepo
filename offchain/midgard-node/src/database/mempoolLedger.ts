import { Address, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import * as utils from "./utils.js";
import { clearTable, insertUtxos, retrieveUtxos } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type VARCHAR (8),
    script_ref_script BLOB,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS mempool_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit BLOB,
    quantity BIGINT NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES mempool_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );`;

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUtxos(db, "mempool_ledger", "mempool_ledger_assets", utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> =>
  retrieveUtxos(db, "mempool_ledger", "mempool_ledger_assets");

export const retrieveUtxosOnAddress = async (
  db: sqlite3.Database,
  address: Address
): Promise<UTxO[]> =>
  utils.retrieveUtxosOnAddress(
    db,
    "mempool_ledger",
    "mempool_ledger_assets",
    address
  );

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "mempool_ledger");
