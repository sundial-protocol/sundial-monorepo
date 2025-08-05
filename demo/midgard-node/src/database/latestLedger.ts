import {
  clearTable,
  delMultiple,
} from "./utils.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  LedgerEntry,
} from "./utilsLedger.js"

export const tableName = "latest_ledger";

export const insertMultiple = (entries: LedgerEntry[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = () => retrieveLedgerEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
