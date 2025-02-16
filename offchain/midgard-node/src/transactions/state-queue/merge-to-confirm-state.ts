/**
 * This script performs the following tasks to merge the first block into the confirmed state:
 *
 * 1. Fetch transactions of the first block by querying ImmutableDB.
 * 2. Apply those transactions to ConfirmedLedgerDB and update the table to store the updated UTxO set.
 * 3. Remove all header hashes from BlocksDB associated with the merged block.
 * 4. Build and submit the merge transaction.
 */

import { Database } from "sqlite3";
import { LucidEvolution, OutRef, UTxO } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { fetchFirstBlockTxs, handleSignSubmit } from "../utils.js";
import { findSpentAndProducedUTxOs } from "@/utils.js";
import * as BlocksDB from "@/database/blocks.js";
import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import { modifyMultipleTables } from "@/database/utils.js";

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param db - The database instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param header_hash - The header hash of the block to be merged.
 * @returns An Effect that resolves when the merge transaction is built and submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  db: Database,
  fetchConfig: SDK.Types.FetchConfig,
) =>
  Effect.gen(function* ($) {
    // Fetch transactions from the first block
    const { txs: firstBlockTxs, headerHash } = yield* $(
      fetchFirstBlockTxs(lucid, fetchConfig, db),
    );

    // Build the transaction
    const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
      lucid,
      fetchConfig,
    );
    // Submit the transaction
    yield* handleSignSubmit(lucid, txBuilder);

    const spentOutRefs: OutRef[] = [];
    const producedUTxOs: UTxO[] = [];
    for (const txCBOR of firstBlockTxs) {
      const { spent, produced } = findSpentAndProducedUTxOs(txCBOR);
      spentOutRefs.push(...spent);
      producedUTxOs.push(...produced);
    }

    // - Clear all the spent UTxOs from the confirmed ledger
    // - Add all the produced UTxOs from the confirmed ledger
    // - Remove all the tx hashes of the merged block from BlocksDB
    yield* Effect.tryPromise({
      try: () =>
        modifyMultipleTables(
          db,
          [ConfirmedLedgerDB.clearUTxOs, spentOutRefs],
          [ConfirmedLedgerDB.insert, producedUTxOs],
          [BlocksDB.clearBlock, headerHash],
        ),
      catch: (e) => new Error(`Transaction failed: ${e}`),
    });
  });
