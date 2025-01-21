import { logInfo, logWarning } from "../utils.js";
import * as MPF from "@aiken-lang/merkle-patricia-forestry";
import express from "express";

export const listen = (port: number) => {
  const app = express();
  app.get("/", (req, res) => {
    logWarning("TODO");
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
