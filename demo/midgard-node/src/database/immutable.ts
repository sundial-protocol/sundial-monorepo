import { Effect } from "effect";
import {
  TXColumns,
  TXEntries,
  clearTable,
  insertTX,
  insertTXs,
  retrieveValue,
  retrieveValues,
  retrieveTXEntries,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "immutable";

export const insert = (tx: TXEntries
): Effect.Effect<void, Error, Database> =>
  insertTX(tableName, tx);

export const insertTxs = (
  txs: TXEntries[],
): Effect.Effect<void, Error, Database> => insertTXs(tableName, txs);

export const retrieve = () => retrieveTXEntries(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
