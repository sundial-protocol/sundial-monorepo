import { OutRef } from "@lucid-evolution/lucid";
import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable, insertUTxOsCBOR, retrieveUTxOsCBOR } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_ledger (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    utxo_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_hash, output_index)
  );
  `;

export const insert = async (pool: Pool, utxosCBOR: [OutRef, Uint8Array][]) =>
  insertUTxOsCBOR(pool, "confirmed_ledger", utxosCBOR);

export const retrieve = async (pool: Pool): Promise<[OutRef, Uint8Array][]> =>
  retrieveUTxOsCBOR(pool, "confirmed_ledger");

export const clearUTxOs = async (pool: Pool, refs: OutRef[]) =>
  utils.clearUTxOs(pool, "confirmed_ledger", refs);

export const clear = async (pool: Pool): Promise<void> =>
  clearTable(pool, "confirmed_ledger");
