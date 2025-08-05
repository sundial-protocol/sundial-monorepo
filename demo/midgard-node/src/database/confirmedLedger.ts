import { Effect } from "effect";
import {
  clearTable,
  delMultiple,
} from "./utils.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  LedgerEntry,
} from "./utilsLedger.js"

import { Database } from "@/services/database.js";

export const tableName = "confirmed_ledger";

export const insertMultiple = (entries: LedgerEntry[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = (): Effect.Effect<void, Error, Database> =>
  retrieveLedgerEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
