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
} from "../utils.js";
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

export interface UtxoRow {
  tx_hash: string;
  output_index: number;
  address: string;
  assets: string;
  datum_hash?: string | null;
  datum?: string | null;
  script_ref_type?: string | null;
  script_ref_script?: string | null;
}

export function utxoFromRow(row: UtxoRow): UTxO {
  const scriptRefType: ScriptType | null =
    row.script_ref_type == "Native"
      ? "Native"
      : row.script_ref_type == "PlutusV1"
      ? "PlutusV1"
      : row.script_ref_type == "PlutusV2"
      ? "PlutusV2"
      : row.script_ref_type == "PlutusV3"
      ? "PlutusV3"
      : null;
  const assets = JSON.parse(row.assets, (_, v) => {
    try {
      return BigInt(v);
    } catch {
      return v;
    }
  });
  return {
    txHash: row.tx_hash,
    outputIndex: row.output_index,
    address: row.address,
    assets: assets,
    datumHash: row.datum_hash || null,
    datum: row.datum || null,
    scriptRef:
      scriptRefType && row.script_ref_script
        ? { type: scriptRefType, script: row.script_ref_script }
        : null,
  };
}

export function utxoToRow(utxo: UTxO): UtxoRow {
  return {
    tx_hash: utxo.txHash,
    output_index: utxo.outputIndex,
    address: utxo.address,
    assets: JSON.stringify(utxo.assets, (_, v) =>
      typeof v === "bigint" ? v.toString() : v
    ),
    datum_hash: utxo.datumHash,
    datum: utxo.datum,
    script_ref_type: utxo.scriptRef?.type || null,
    script_ref_script: utxo.scriptRef?.script || null,
  };
}

export const retrieveBlockHashWithUtxosFromTable = async (
  db: sqlite3.Database,
  tableName: string
): Promise<({ blockHash: string } & UTxO)[]> => {
  const query = `SELECT * FROM ${tableName}`;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows: ({ block_hash: string } & UtxoRow)[]) => {
      if (err) {
        logAbort(`Error retrieving block hash with utxos from table ${tableName}
            : ${err.message}`);
        return reject(err);
      }
      const result = rows.map(({ block_hash, ...utxoRow }) => {
        return { blockHash: block_hash, ...utxoFromRow(utxoRow) };
      });
      resolve(result);
    });
  });
};

export const clearTable = async (db: sqlite3.Database, tableName: string) => {
  const query = `DELETE FROM ${tableName};`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, function (err) {
      if (err) {
        logAbort(`${tableName}: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`${tableName}: cleared`);
        resolve();
      }
    });
  });
};