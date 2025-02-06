import { UTxO } from "@lucid-evolution/lucid";
import { logAbort, logInfo } from "../utils.js";
import sqlite3 from "sqlite3";
import {
  clearTable,
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

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) => {
  const values = utxos.flatMap((utxo) => Object.values(utxoToRow(utxo)));
  const query = `
    INSERT INTO confirmed_ledger
      (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
    VALUES
    ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}
  `;
  const normalizedAssets = utxos.flatMap(utxo => (utxoToNormalizedAssets(utxo)))
  const assetQuery = `
    INSERT INTO confirmed_ledger_assets
      (tx_hash, output_index, unit, quantity)
    VALUES
     ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}
  `;
  const assetValues = normalizedAssets.flatMap(v => Object.values(v))
  return new Promise<void>((resolve, reject) => {
    db.run("BEGIN TRANSACTION;", (err) => {
      if (err) {
        logAbort(`Error starting transaction: ${err.message}`);
        return reject(err);
      }
      db.run(query, values, (err) => {
        if (err) {
          logAbort(`Error inserting UTXOs: ${err.message}`);
          db.run("ROLLBACK;", () => reject(err));
        } else {
          logInfo(`${utxos.length} new UTXOs added to confirmed_ledger`);
          db.run(assetQuery, assetValues, (err) => {
            if (err) {
              logAbort(`Error inserting assets: ${err.message}`);
              db.run("ROLLBACK;", () => reject(err));
            } else {
              logInfo(`Assets added to confirmed_ledger_assets`);
              db.run("COMMIT;", (err) => {
                if (err) {
                  logAbort(`Error committing transaction: ${err.message}`);
                  return reject(err);
                }
                resolve();
              });
            }
          });
        }
      });
    });
  });
};

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> => {
  const query = `
    SELECT l.tx_hash
         , json_group_array(json_object('unit', a.unit, 'quantity', a.quantity)) AS assets
    FROM confirmed_ledger AS l
      LEFT JOIN confirmed_ledger_assets AS a
        ON l.tx_hash = a.tx_hash
          & l.output_index = a.output_index
    GROUP BY l.tx_hash
    ;
    `;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows) => {
      if (err) {
        logAbort(
          `confirmed_ledger: error retrieving utxos from table: ${err.message}`
        );
        return reject(err);
        console.log(rows);
        // }
        // const result = rows.map((utxoRow) => {
        //   return utxoFromRow(utxoRow);
        // });
        // resolve(result);
        return [];
      }
    });
  });
};

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "confirmed_ledger");
