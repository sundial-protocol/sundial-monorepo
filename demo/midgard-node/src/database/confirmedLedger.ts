import { Effect } from "effect";
import {
  clearTable,
} from "./utils.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  delLedgerEntries,
  LedgerEntry,
  LedgerEntryTimeStamped,
} from "./utilsLedger.js"

import { Database } from "@/services/database.js";

export const tableName = "confirmed_ledger";

export const insertMultiple = (entries: LedgerEntry[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = () => retrieveLedgerEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => delLedgerEntries(tableName, refs);

export const clear = () => clearTable(tableName);
