import {
  Result,
  errorToString,
  isHexString,
  logInfo,
  logWarning,
  ok,
  setupLucid,
} from "../utils.js";
import * as MPF from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, OutRef, UTxO } from "@lucid-evolution/lucid";
import express from "express";

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
  port: number,
  pollingInterval: number
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

const monitorStateQueue = (lucid: LucidEvolution, pollingInterval: number) => {
  let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };
  setInterval(async () => {
    let latestBlockOutRefRes = await fetchLatestBlock(lucid);
    if (latestBlockOutRefRes.type === "ok") {
      if (!outRefsAreEqual(latestBlockOutRef, latestBlockOutRefRes.data)) {
        await submitBlock(lucid, latestBlockOutRefRes.data);
      }
    } else {
      logWarning(`Something went wrong while fetching the latest block:
${errorToString(latestBlockOutRefRes.error)}`);
    }
  }, pollingInterval);
};

const storeTx = (dbFilePath: string, tx: string) => {
  logWarning("storeTx: TODO");
};

const submitBlock = async (lucid: LucidEvolution, latestBlock: UTxO) => {
  logWarning("submitBlock: TODO");
};

const mergeOldestBlock = () => {
  logWarning("mergeOldestBlock: TODO");
};
