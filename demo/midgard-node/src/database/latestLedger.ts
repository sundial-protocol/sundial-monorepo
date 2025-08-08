import {
  clearTable,
} from "@/utils/tx.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  delLedgerEntries,
  LedgerEntry,
} from "@/utils/ledger.js"

export const tableName = "latest_ledger";

export const insertMultiple = (entries: LedgerEntry[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = () => retrieveLedgerEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => delLedgerEntries(tableName, refs);

export const clear = () => clearTable(tableName);
