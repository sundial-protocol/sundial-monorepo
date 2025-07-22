import { Effect } from "effect";
import {
  clearTable,
  insertLedgerUTxOs,
  retrieveLedgerUTxOs,
  retrieveLedgerUTxOsWithAddress,
  delMultiple,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (utxosCBOR: { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array }[]) =>
  insertLedgerUTxOs(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<
  readonly { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array; address: String }[],
  Error,
  Database
> => retrieveLedgerUTxOs(tableName);

export const retrieveByAddress = (address: String): Effect.Effect<
  { outReferenceBytes: Uint8Array; txOutputBytes: Uint8Array; }[],
  Error,
  Database
> => retrieveLedgerUTxOsWithAddress(tableName, address);


export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
