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
import * as mempool from "../database/mempool.js"

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
  await mempool.insert(db, txHash, tx);
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
