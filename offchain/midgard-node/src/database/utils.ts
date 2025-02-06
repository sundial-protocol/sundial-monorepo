import {
  logAbort,
  logInfo,
} from "../utils.js";
import {
  ScriptType,
  UTxO,
} from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";

export interface UtxoToRow {
  tx_hash: string;
  output_index: number;
  address: string;
  datum_hash?: string | null;
  datum?: string | null;
  script_ref_type?: string | null;
  script_ref_script?: string | null;
}

export interface UtxoFromRow {
  tx_hash: string;
  output_index: number;
  address: string;
  assets: string;
  datum_hash?: string | null;
  datum?: string | null;
  script_ref_type?: string | null;
  script_ref_script?: string | null;
}

export function utxoFromRow(row: UtxoFromRow): UTxO {
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

export function utxoToRow (utxo: UTxO): UtxoToRow {
  return {
    tx_hash: utxo.txHash,
    output_index: utxo.outputIndex,
    address: utxo.address,
    // assets: JSON.stringify(utxo.assets, (_, v) =>
    //   typeof v === "bigint" ? v.toString() : v
    // ),
    datum_hash: utxo.datumHash,
    datum: utxo.datum,
    script_ref_type: utxo.scriptRef?.type || null,
    script_ref_script: utxo.scriptRef?.script || null,
  };
}

export interface NormalizedAsset {
  tx_hash: string;
  output_index: number;
  unit: string;
  quantity: number;
};

//TODO: figure out how to store bigInt in the database
export function utxoToNormalizedAssets (utxo: UTxO): NormalizedAsset[] {
  return Object.entries(utxo.assets).flatMap(([unit, quantity]) => {
    const asset: NormalizedAsset = {tx_hash: utxo.txHash
                                   , output_index: utxo.outputIndex
                                   , unit: unit
                                   , quantity: Number(quantity)
                                   }
    return asset;
    }
  );
}

export const clearTable = async (db: sqlite3.Database, tableName: string) => {
  const query = `DELETE FROM ${tableName};`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, function (err) {
      if (err) {
        logAbort(`${tableName} db: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`${tableName} db: cleared`);
        resolve();
      }
    });
  });
};
