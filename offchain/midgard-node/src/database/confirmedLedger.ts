import { UTxO } from "@lucid-evolution/lucid";
import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";
import {
  clearTable,
  insertUtxos,
  UtxoFromRow,
  utxoFromRow,
  utxoToNormalizedAssets,
  utxoToRow,
} from "./utils.js";

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
  insertUtxos(db, utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> => {
  const query = `
    SELECT
      l.tx_hash,
      l.output_index,
      address,
      json_group_array(json_object('unit', a.unit, 'quantity', a.quantity)) AS assets,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    FROM confirmed_ledger AS l
      LEFT JOIN confirmed_ledger_assets AS a
        ON l.tx_hash = a.tx_hash AND l.output_index = a.output_index
    GROUP BY
      l.tx_hash,
      l.output_index,
      address,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    ORDER BY
      l.tx_hash,
      l.output_index;
    ;
    `;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows: UtxoFromRow[]) => {
      if (err) {
        logAbort(`confirmed_ledger: error retrieving utxos: ${err.message}`);
        return reject(err);
      }
      resolve(rows.map(r => utxoFromRow(r)));
      });
})};

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "confirmed_ledger");
