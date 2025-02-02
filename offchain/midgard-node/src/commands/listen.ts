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
import { Data, LucidEvolution, OutRef, UTxO } from "@lucid-evolution/lucid";
import express from "express";
import sqlite3 from "sqlite3";

// TODO: Placehoder, must be imported from SDK.
const fetchLatestBlock = async (
  _lucid: LucidEvolution
): Promise<Result<UTxO>> => {
  return ok({
    txHash: "",
    outputIndex: 0,
    address: "",
    assets: {},
  });
};

// TODO: Placehoder, must be imported from SDK.
const fetchConfirmedState = async (
  _lucid: LucidEvolution
): Promise<Result<UTxO>> => {
  return ok({
    txHash: "",
    outputIndex: 0,
    address: "",
    assets: {},
  });
};

const readEndTimeOfConfirmedState = (utxo: UTxO): Result<number> => {
  if (utxo.datum) {
    // const confirmedState = Data.castFrom(
    //   utxo.datum,
    //   SDK.LedgerState.ConfirmedState
    // );
    // return ok(Number(confirmedState.endTime));
    return ok(0);
  } else {
    return fail("Missing datum of the confirmed state.");
  }
};

const utxoToOutRef = (utxo: UTxO): OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

const outRefsAreEqual = (outRef0: OutRef, outRef1: OutRef): boolean => {
  return (
    outRef0.txHash === outRef1.txHash &&
    outRef0.outputIndex === outRef1.outputIndex
  );
};

export const listen = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  port: number,
  pollingInterval: number,
  confirmedStatePollingInterval: number
) => {
  const app = express();
  app.get("/", (req, res) => {
    res.type("text/plain");
    const txHex = req.query.tx;
    const txIsString = typeof txHex === "string";
    if (txIsString && isHexString(txHex)) {
      res.send(`Transaction received: ${req.query.tx}`);
    } else {
      res.send("Please provide a valid transaction CBOR.");
    }
  });
  app.listen(port, () => {});
  logInfo(`Server running at http://localhost:${port}`);
};

const monitorStateQueue = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  pollingInterval: number
) => {
  let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };
  setInterval(async () => {
    const latestBlockOutRefRes = await fetchLatestBlock(lucid);
    if (latestBlockOutRefRes.type === "ok") {
      const fetchedBlocksOutRef = utxoToOutRef(latestBlockOutRefRes.data);
      if (!outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
        latestBlockOutRef = fetchedBlocksOutRef;
        await submitBlock(lucid, latestBlockOutRefRes.data);
      }
    } else {
      logWarning(`Something went wrong while fetching the latest block:
${errorToString(latestBlockOutRefRes.error)}`);
    }
  }, pollingInterval);
};

export const storeTx = async (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  tx: string
) => {
  const txHash = lucid.fromTx(tx).toHash();
  await addToMempool(db, txHash, tx);
};

const submitBlock = async (lucid: LucidEvolution, latestBlock: UTxO) => {
  logWarning("submitBlock: TODO");
};

const monitorConfirmedState = (
  lucid: LucidEvolution,
  pollingInterval: number
) => {
  logWarning("mergeOldestBlock: TODO");
};

export const addBlockUtxosToConfirmedState = (
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
      , scriptRef
      ) VALUES
    ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?, ?)`).join(", ")}
  `;
  const values = utxos.flatMap((utxo) => [
    blockHash,
    utxo.txHash,
    utxo.outputIndex,
    utxo.address,
    JSON.stringify(utxo.assets, (_, v) =>
      typeof v === "bigint" ? v.toString() : v
    ),
    utxo.datumHash,
    utxo.datum,
    utxo.scriptRef,
  ]);
  return new Promise<void>((resolve, reject) => {
    db.run(query, values, err => {
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

export const clearConfirmedState = (db: sqlite3.Database) => {
  const query = `DELETE FROM confirmed_state_utxo;`;
  db.run(query, err => {
    if (err) {
      logAbort(`Confirmed state: clearing error: ${err.message}`);
    } else {
      logInfo(`Confirmed state: cleared`);
    }
  });
};

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
      , scriptRef
      ) VALUES ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?, ?)`).join(", ")}
  `;
  const values = utxos.flatMap((utxo) => [
    blockHash,
    utxo.txHash,
    utxo.outputIndex,
    utxo.address,
    JSON.stringify(utxo.assets, (_, v) =>
      typeof v === "bigint" ? v.toString() : v
    ),
    utxo.datumHash,
    utxo.datum,
    utxo.scriptRef,
  ]);
  return new Promise<void>((resolve, reject) => {
    db.run("BEGIN TRANSACTION;");
    db.run(query, values, err => {
      if (err) {
        logAbort(`Confirmed state: error inserting utxos: ${err.message}`);
        db.run("ROLLBACK;");
        reject(err);
      } else {
        logInfo(
          `Latest block utxos: new latest block ${blockHash} with ${utxos.length} utxos`
        );
        db.run(
          `DELETE FROM latest_block_utxo WHERE NOT (block_hash = '${blockHash}');`
        );
        db.run("COMMIT;");
        resolve();
      }
    });
  });
};

export interface ArchiveTxRow {
  tx_hash: string;
  tx_cbor: string;
}

export const addToArchive = async (
  db: sqlite3.Database,
  blockHash: string,
  txs: ArchiveTxRow[]
) => {
  const blockQuery = `
    INSERT INTO archive_block (tx_hash, block_hash) VALUES
    ${txs.map((tx) => `(?, ?)`).join(", ")}
  `;
  const blockValues = txs.flatMap((tx) => [tx.tx_hash, blockHash]);
  await new Promise<void>((resolve, reject) => {
    db.run(blockQuery, blockValues, function (err) {
      if (err) {
        logAbort(`Archive: error inserting block: ${err.message}`);
        reject(err);
      } else {
        logInfo(`Archive: block stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
  const txQuery = `
    INSERT INTO archive_tx (tx_hash, tx_cbor) VALUES
    ${txs.map(_ => `(?, ?)`).join(", ")}
    `;
  db.run(
    txQuery,
    txs.flatMap((tx) => [tx.tx_hash, tx.tx_cbor]),
    function (err) {
      if (err) {
        logAbort(`Archive: error inserting txs: ${err.message}`);
      } else {
        logInfo(
          `Archive: ${txs.length} txs stored, last rowid: ${this.lastID}`
        );
      }
    }
  );
};

export const clearArchive = (db: sqlite3.Database): Promise<void> => {
  const query = `DELETE FROM archive_block;`;
  return new Promise((resolve, reject) => {
    db.run(query, err => {
      if (err) {
        logAbort(`Archive: clearing error: ${err.message}`);
        return reject(err);
      } else {
        logInfo(`Archive: cleared`);
        resolve();
      }
    });
  });
};

export const addToMempool = async (
  db: sqlite3.Database,
  txHash: string,
  tx_cbor: string
) => {
  const query = `INSERT INTO mempool (tx_hash, tx_cbor) VALUES (?, ?)`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [txHash, tx_cbor], function (err) {
      if (err) {
        logAbort(`Mempool: error inserting tx to mempool: ${err.message}`);
        reject(err);
      } else {
        logInfo(`Mempool: tx stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
};

export interface MempoolRow {
  tx_hash: string;
  tx_cbor: string;
}

export const retrieveMempool = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM mempool`;
  const mempool = await new Promise<MempoolRow[]>((resolve, reject) => {
    db.all(query, (err, rows: MempoolRow[]) => {
      if (err) {
        logAbort(`Mempool: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows);
    });
  });
  return mempool;
};

export const clearMempool = async (db: sqlite3.Database) => {
  const query = `DELETE FROM mempool;`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, function (err) {
      if (err) {
        logAbort(`Mempool: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`Mempool: cleared`);
        resolve();
      }
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
    CREATE TABLE IF NOT EXISTS mempool (
      tx_hash BLOB NOT NULL UNIQUE,
      tx_cbor BLOB NOT NULL UNIQUE,
      PRIMARY KEY (tx_hash)
    );
    CREATE TABLE IF NOT EXISTS archive_block (
      tx_hash BLOB NOT NULL UNIQUE,
      block_hash BLOB NOT NULL,
      PRIMARY KEY (tx_hash)
    );
    CREATE TABLE IF NOT EXISTS archive_tx (
      tx_hash BLOB NOT NULL UNIQUE,
      tx_cbor BLOB NOT NULL UNIQUE,
      PRIMARY KEY (tx_hash)
      FOREIGN KEY (tx_hash)
        REFERENCES archive_block(tx_hash)
        ON DELETE CASCADE
    );
    CREATE TABLE IF NOT EXISTS latest_block_utxo (
      block_hash BLOB NOT NULL,
      tx_hash BLOB NOT NULL,
      output_index INTEGER NOT NULL,
      address TEXT NOT NULL,
      assets TEXT,
      datum_hash BLOB,
      datum BLOB,
      scriptRef BLOB
    );
    CREATE TABLE IF NOT EXISTS confirmed_state_utxo (
      block_hash BLOB NOT NULL,
      tx_hash BLOB NOT NULL,
      output_index INTEGER NOT NULL,
      address TEXT NOT NULL,
      assets TEXT,
      datum_hash BLOB,
      datum BLOB,
      scriptRef BLOB
    );
  `);
  return db;
}
