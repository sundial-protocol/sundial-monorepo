import { UTxO } from "@lucid-evolution/lucid";
import {
  logAbort,
  logInfo,
} from "../utils.js";
import sqlite3 from "sqlite3";
import { clearTable, retrieveBlockHashWithUtxosFromTable, utxoToRow } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_state_utxo (
    block_hash BLOB NOT NULL,
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    assets TEXT,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type TEXT (8),
    script_ref_script TEXT
  );`

export const insert = async (
  db: sqlite3.Database,
  blockHash: string,
  utxos: UTxO[]
) => {
  const query = `
      INSERT INTO confirmed_state_utxo
        ( block_hash
        , tx_hash
        , output_index
        , address
        , assets
        , datum_hash
        , datum
        , script_ref_type
        , script_ref_script
        ) VALUES
      ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?, ?, ?)`).join(", ")}
    `;
  const values = utxos.flatMap((utxo) => {
    const row = utxoToRow(utxo);
    return [blockHash, ...Object.values(utxoToRow(utxo))];
  });
  return new Promise<void>((resolve, reject) => {
    db.run(query, values, (err) => {
      if (err) {
        logAbort(`Confirmed state: error inserting utxos: ${err.message}`);
        reject();
      } else {
        logInfo(`Confirmed state: ${utxos.length} new utxos added`);
        resolve();
      }
    });
  });
};

export const retrieve = async (
  db: sqlite3.Database
): Promise<({ blockHash: string } & UTxO)[]> => {
  return retrieveBlockHashWithUtxosFromTable(db, "confirmed_state_utxo");
};

export const clear = async (db: sqlite3.Database) => clearTable(db, "confirmed_state_utxo");