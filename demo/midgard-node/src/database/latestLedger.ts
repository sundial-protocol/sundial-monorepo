import {
  clearTable,
} from "./utils.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  delLedgerEntries,
  LedgerEntry,
  LedgerColumns,
} from "./utilsLedger.js"

export const tableName = "latest_ledger";

export const insertMultiple = (entries: Omit<LedgerEntry, LedgerColumns.TIMESTAMPTZ>[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = () => retrieveLedgerEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => delLedgerEntries(tableName, refs);

export const clear = () => clearTable(tableName);
