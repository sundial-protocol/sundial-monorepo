import {
  clearTable,
  insertLedgerUTxOs,
  retrieveLedgerUTxOs,
  delMultiple,
} from "./utils.js";

export const tableName = "latest_ledger";

export const insert = (utxosCBOR: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[]) =>
  insertLedgerUTxOs(tableName, utxosCBOR);

export const retrieve = () => retrieveLedgerUTxOs(tableName);

export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
