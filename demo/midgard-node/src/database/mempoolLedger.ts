import { Effect } from "effect";
import {
  clearTable,
} from "@/utils/tx.js";
import {
  insertLedgerEntries,
  retrieveLedgerEntries,
  retrieveLedgerEntriesWithAddress,
  delLedgerEntries,
  LedgerEntry,
} from "@/utils/ledger.js"

import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (entries: LedgerEntry[]) =>
  insertLedgerEntries(tableName, entries);

export const retrieve = (): Effect.Effect<
  readonly LedgerEntry[],
  Error,
  Database
> => retrieveLedgerEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly LedgerEntry[], Error, Database> =>
  retrieveLedgerEntriesWithAddress(tableName, address);

export const clearUTxOs = (refs: Buffer[]) => delLedgerEntries(tableName, refs);

export const clear = () => clearTable(tableName);
