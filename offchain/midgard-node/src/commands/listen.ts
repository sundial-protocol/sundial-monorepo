import { isHexString, logInfo, logWarning } from "../utils.js";
import * as MPF from "@aiken-lang/merkle-patricia-forestry";
import express from "express";

export const listen = (port: number) => {
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

const monitorStateQueue = () => {
  logWarning("monitorStateQueue: TODO");
};

const storeTx = (dbFilePath: string, tx: string) => {
  logWarning("storeTx: TODO");
};

const submitBlock = () => {
  logWarning("submitBlock: TODO");
};

const mergeOldestBlock = () => {
  logWarning("mergeOldestBlock: TODO");
};
