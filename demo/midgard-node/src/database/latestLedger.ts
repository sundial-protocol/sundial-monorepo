import {
  clearTable,
  insertKeyValuesUTxO,
  retrieveKeyValuesUTxO,
  delMultiple,
} from "./utils.js";

export const tableName = "latest_ledger";

export const insert = (utxosCBOR: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[]) =>
  insertKeyValuesUTxO(tableName, utxosCBOR);

export const retrieve = () => retrieveKeyValuesUTxO(tableName);

export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
