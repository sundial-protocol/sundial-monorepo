import { Effect } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import {
  clearTable,
  DatabaseError,
  NotFoundError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

// ON CONFLICT DO NOTHING: genesis seeding and deposit application can both
// try to insert the same outref; the second insert is a correct no-op.
export const insert = (entries: readonly Ledger.Entry[]) =>
  Ledger.insertEntriesOrIgnore(tableName, entries);

export const retrieve: Effect.Effect<
  readonly Ledger.Entry[],
  DatabaseError,
  Database
> = Ledger.retrieveAllEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Ledger.retrieveEntriesWithAddress(tableName, address);

export const retrieveByOutRefs = (
  outRefs: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Ledger.retrieveByOutRefs(tableName, outRefs);

retrieve;

export const retrieveByOutRef = (
  spentOutRef: Buffer,
): Effect.Effect<Ledger.Entry, DatabaseError | NotFoundError, Database> =>
  Ledger.retrieveByOutRef(tableName, spentOutRef);

export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
