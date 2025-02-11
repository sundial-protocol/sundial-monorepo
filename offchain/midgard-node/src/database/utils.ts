import {
  Address,
  fromHex,
  ScriptType,
  toHex,
  UTxO,
} from "@lucid-evolution/lucid";
import { logAbort, logInfo } from "../utils.js";

import sqlite3 from "sqlite3";

export const insertUtxos = async (
  db: sqlite3.Database,
  tableName: string,
  assetTableName: string,
  utxos: UTxO[]
) => {
  const values = utxos.flatMap((utxo) => Object.values(utxoToRow(utxo)));
  const query = `
    INSERT INTO ${tableName}
      (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
    VALUES
    ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}
  `;
  const normalizedAssets = utxos.flatMap((utxo) =>
    utxoToNormalizedAssets(utxo)
  );
  const assetQuery = `
    INSERT INTO ${assetTableName}
      (tx_hash, output_index, unit, quantity)
    VALUES
     ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}
  `;
  const assetValues = normalizedAssets.flatMap((v) => Object.values(v));
  return new Promise<void>((resolve, reject) => {
    db.run("BEGIN TRANSACTION;", (err) => {
      if (err) {
        logAbort(`${tableName} db: error starting transaction: ${err.message}`);
        return reject(err);
      }
      db.run(query, values, (err) => {
        if (err) {
          logAbort(`${tableName} db: error inserting UTXOs: ${err.message}`);
          db.run("ROLLBACK;", () => reject(err));
        } else {
          logInfo(`${tableName} db: ${utxos.length} new UTXOs added`);
          db.run(assetQuery, assetValues, (err) => {
            if (err) {
              logAbort(
                `${tableName} db: error inserting assets: ${err.message}`
              );
              db.run("ROLLBACK;", () => reject(err));
            } else {
              logInfo(
                `${tableName}: ${normalizedAssets.length} assets added to ${assetTableName}`
              );
              db.run("COMMIT;", (err) => {
                if (err) {
                  logAbort(
                    `${tableName}: error committing transaction: ${err.message}`
                  );
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

export const retrieveUtxos = async (
  db: sqlite3.Database,
  tableName: string,
  assetTableName: string
): Promise<UTxO[]> => {
  const query = `
    SELECT
      t.tx_hash,
      t.output_index,
      address,
      json_group_array(json_object('unit', hex(a.unit), 'quantity', a.quantity)) AS assets,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    FROM ${tableName} AS t
      LEFT JOIN ${assetTableName} AS a
        ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
    GROUP BY
      t.tx_hash,
      t.output_index,
      address,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    ORDER BY
      t.tx_hash,
      t.output_index;
    ;
    `;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows: UtxoFromRow[]) => {
      if (err) {
        logAbort(`${tableName} db: error retrieving utxos: ${err.message}`);
        return reject(err);
      }
      resolve(rows.map((r) => utxoFromRow(r)));
    });
  });
};

export const retrieveUtxosOnAddress = async (
  db: sqlite3.Database,
  tableName: string,
  assetTableName: string,
  address: Address
): Promise<UTxO[]> => {
  const query = `
      SELECT
        t.tx_hash,
        t.output_index,
        address,
        json_group_array(json_object('unit', hex(a.unit), 'quantity', a.quantity)) AS assets,
        datum_hash,
        datum,
        script_ref_type,
        script_ref_script
      FROM ${tableName} AS t
        LEFT JOIN ${assetTableName} AS a
          ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
      WHERE address = ?
      GROUP BY
        t.tx_hash,
        t.output_index,
        address,
        datum_hash,
        datum,
        script_ref_type,
        script_ref_script
      ORDER BY
        t.tx_hash,
        t.output_index
      ;
      `;
  return new Promise((resolve, reject) => {
    db.all(query, [address], (err, rows: UtxoFromRow[]) => {
      if (err) {
        logAbort(`${tableName} db: error retrieving utxos: ${err.message}`);
        return reject(err);
      }
      resolve(rows.map((r) => utxoFromRow(r)));
    });
  });
};

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

export interface UtxoFromRow {
  tx_hash: Uint8Array;
  output_index: number;
  address: string;
  assets: string;
  datum_hash?: Uint8Array | null;
  datum?: Uint8Array | null;
  script_ref_type?: string | null;
  script_ref_script?: Uint8Array | null;
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
    txHash: toHex(row.tx_hash),
    outputIndex: row.output_index,
    address: row.address,
    assets: transformAssetsToObject(row.assets),
    datumHash: row.datum_hash != null ? toHex(row.datum_hash) : null,
    datum: row.datum != null ? toHex(row.datum) : null,
    scriptRef:
      scriptRefType && row.script_ref_script
        ? { type: scriptRefType, script: toHex(row.script_ref_script) }
        : null,
  };
}

// transforms [{"unit":u1,"quantity":q1},...]-like string into assets
const transformAssetsToObject = (
  assetsString: string
): Record<string, bigint> => {
  if (!assetsString || assetsString === "null") {
    return {};
  }

  try {
    const assetsArray: { unit: string; quantity: any }[] =
      JSON.parse(assetsString);
    const assetsObject: Record<string, bigint> = {};
    assetsArray.forEach((asset) => {
      const unit =
        asset.unit == "6C6F76656C616365"
          ? "lovelace"
          : asset.unit.toLowerCase();
      assetsObject[unit] = BigInt(asset.quantity);
    });
    return assetsObject;
  } catch (error) {
    logAbort("error parsing assets:" + error);
    return {};
  }
};

export interface UtxoToRow {
  tx_hash: Uint8Array;
  output_index: number;
  address: string;
  datum_hash?: Uint8Array | null;
  datum?: Uint8Array | null;
  script_ref_type?: string | null;
  script_ref_script?: Uint8Array | null;
}

export function utxoToRow(utxo: UTxO): UtxoToRow {
  return {
    tx_hash: fromHex(utxo.txHash),
    output_index: utxo.outputIndex,
    address: utxo.address,
    datum_hash: utxo.datumHash != null ? fromHex(utxo.datumHash) : null,
    datum: utxo.datum != null ? fromHex(utxo.datum) : null,
    script_ref_type: utxo.scriptRef?.type || null,
    script_ref_script:
      utxo.scriptRef?.script != null ? fromHex(utxo.scriptRef?.script) : null,
  };
}

export interface NormalizedAsset {
  tx_hash: Uint8Array;
  output_index: number;
  unit: Uint8Array;
  quantity: string; //sqlite threats bigInt as null
}

export function utxoToNormalizedAssets(utxo: UTxO): NormalizedAsset[] {
  return Object.entries(utxo.assets).flatMap(([unit, quantity]) => {
    const asset: NormalizedAsset = {
      tx_hash: fromHex(utxo.txHash),
      output_index: utxo.outputIndex,
      unit: unit == "lovelace" ? fromHex("6c6f76656c616365") : fromHex(unit),
      quantity: quantity.toString(), //sqlite threats bigInt as null
    };
    return asset;
  });
}
