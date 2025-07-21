import { Effect } from "effect";
import {
  clearTable,
  insertKeyValuesUTxO,
  retrieveKeyValuesUTxO,
  delMultiple,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "confirmed_ledger";

export const insert = (utxosCBOR: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[]) =>
  insertKeyValuesUTxO(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<void, Error, Database> =>
  retrieveKeyValuesUTxO(tableName);

export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
