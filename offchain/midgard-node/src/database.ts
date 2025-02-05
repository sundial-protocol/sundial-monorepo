import {
  Result,
  errorToString,
  fail,
  isHexString,
  logAbort,
  logInfo,
  logWarning,
  ok,
  setupLucid,
} from "./utils.js";
import * as MPF from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  LucidEvolution,
  OutRef,
  ScriptType,
  UTxO,
} from "@lucid-evolution/lucid";
import express from "express";
import sqlite3 from "sqlite3";
import { utxoToRow } from "./database/utils.js";
import * as mempool from "./database/mempool.js"
import * as immutable from "./database/immutable.js"
import * as confirmedLedger from "./database/confirmedLedger.js"
import * as latestLedger from "./database/latestLedger.js"

export const changeLatestBlock = async (
  db: sqlite3.Database,
  blockHash: string,
  utxos: UTxO[]
) => {
  const query = `
      INSERT INTO latest_block_utxo
        ( block_hash
        , tx_hash
        , output_index
        , address
        , assets
        , datum_hash
        , datum
        , script_ref_type
        , script_ref_script
        ) VALUES ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?, ?, ?)`).join(", ")}
    `;
  const values = utxos.flatMap((utxo) => {
    return [blockHash, ...Object.values(utxoToRow(utxo))];
  });
  return new Promise<void>((resolve, reject) => {
    db.run("BEGIN TRANSACTION;", (err) => {
      db.run(query, values, (err) => {
        if (err) {
          logAbort(`Latest block utxos: error inserting utxos: ${err.message}`);
          db.run("ROLLBACK;", () => reject(err));
        } else {
          logInfo(
            `Latest block utxos: new latest block ${blockHash} with ${utxos.length} utxos`
          );

          db.run(
            `DELETE FROM latest_block_utxo WHERE NOT (block_hash = ?)`,
            [blockHash],
            (err) => {
              if (err) {
                logAbort(
                  `Latest block utxos: error deleting old utxos: ${err.message}`
                );
                db.run("ROLLBACK;", () => reject(err));
              } else {
                db.run("COMMIT;", (err) => {
                  if (err) {
                    logAbort(
                      `Latest block utxos: error committing transaction: ${err.message}`
                    );
                    return reject(err);
                  }
                  resolve();
                });
              }
            }
          );
        }
      });
    });
  });
};

export async function initializeDb(dbFilePath: string) {
  const db = new sqlite3.Database(dbFilePath, (err) => {
    if (err) {
      logAbort(`Error opening database: ${err.message}`);
    } else {
      logInfo("Connected to the SQLite database");
    }
  });
  db.exec(`
      PRAGMA foreign_keys = ON;
      PRAGMA read_uncommitted=false;
  `)
  db.exec(mempool.createQuery);
  db.exec(immutable.createQuery);
  db.exec(confirmedLedger.createQuery)
  db.exec(latestLedger.createQuery)
  return db;
}
