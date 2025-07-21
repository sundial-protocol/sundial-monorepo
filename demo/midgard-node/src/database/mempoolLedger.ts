import { Effect } from "effect";
import {
  clearTable,
  insertKeyValuesUTxO,
  retrieveKeyValuesUTxO,
  retrieveKeyValuesUTxOWithAddress,
  delMultiple,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (utxosCBOR: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[]) =>
  insertKeyValuesUTxO(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<
  { key: Uint8Array; txOutputBytes: Uint8Array; address: String }[],
  Error,
  Database
> => retrieveKeyValuesUTxO(tableName);

export const retrieveByAddress = (address: String): Effect.Effect<
  { key: Uint8Array; txOutputBytes: Uint8Array; }[],
  Error,
  Database
> => retrieveKeyValuesUTxOWithAddress(tableName, address);


export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
