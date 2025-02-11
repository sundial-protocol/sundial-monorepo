import { OutRef, UTxO, Address, TxSignBuilder } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import { clearTable, insertUtxos, retrieveUtxos } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS latest_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type VARCHAR (8),
    script_ref_script TEXT,
    PRIMARY KEY (tx_hash, output_index)
      FOREIGN KEY (tx_hash)
      REFERENCES blocks(tx_hash)
      ON DELETE CASCADE
  );
  CREATE TABLE IF NOT EXISTS latest_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit VARCHAR (120),
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

export const retrieveByAddr  = async (db: sqlite3.Database, addr: Address): Promise<[UTxO][]> => {
    const utxos = await new Promise<[UTxO][]>((resolve, reject) => {
      // TODO get all utxos with addr
    });
    return utxos
  }

export const updateByTx = async (db: sqlite3.Database, txSignBuilder: TxSignBuilder): Promise<void> => {
  const utxos = await new Promise<[UTxO][]>((resolve, reject) => {
    // TODO remove all used UTxOs, throw if `txSignBuilder` has bad configuration
  });
  return
}

  export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "latest_ledger");
