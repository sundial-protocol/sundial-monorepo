/**
 * This script performs the following tasks to merge the first block into the confirmed state:
 *
 * 1. Fetch transactions of the first block by querying ImmutableDB.
 * 2. Apply those transactions to ConfirmedLedgerDB and update the table to store the updated UTxO set.
 * 3. Remove all header hashes from BlocksDB associated with the merged block.
 * 4. Build and submit the merge transaction.
 */

import { Database } from "sqlite3";
import * as confirmedLedger from "../../database/confirmedLedger.js";
import {
  coreToTxOutput,
  Data,
  LucidEvolution,
  UTxO,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { retrieveByBlockHeaderHash } from "../../database/immutable.js";
import { clearByHeader } from "../../database/blocks.js";
import { handleSignSubmit } from "../utils.js";

/**
 * Fetch transactions of the first block by querying ImmutableDB.
 *
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param db - The database instance.
 * @returns An Effect that resolves to an array of transactions.
 */
export const fetchFirstBlockTxs = (
  lucid: LucidEvolution,
  fetchConfig: SDK.Types.FetchConfig,
  db: Database
): Effect.Effect<{ txHash: string; txCbor: string }[], unknown, unknown> =>
  Effect.gen(function* () {
    const { link: firstBlockUTxO } =
      yield* SDK.Endpoints.fetchConfirmedStateAndItsLinkProgram(
        lucid,
        fetchConfig
      );
    if (!firstBlockUTxO) {
      return yield* Effect.fail(new Error("No blocks in queue"));
    } else {
      const blockNodeDatum = yield* SDK.Utils.getNodeDatumFromUTxO(
        firstBlockUTxO
      );
      const blockHeader = yield* Effect.try({
        try: () =>
          Data.castFrom(blockNodeDatum.data, SDK.Types.LedgerState.Header),
        catch: (e) => new Error(`${e}`),
      });
      const headerHash = yield* SDK.Utils.hashHeader(blockHeader);
      const txs = yield* Effect.tryPromise(() =>
        retrieveByBlockHeaderHash(db, headerHash)
      );
      return txs;
    }
  });

/**
 * Apply the fetched transactions to ConfirmedLedgerDB.
 *
 * @param lucid - The LucidEvolution instance.
 * @param db - The database instance.
 * @param txs - An array of transactions to be applied.
 * @returns An Effect that resolves when the transactions are applied.
 */
export const applyTxsToConfirmedLedger = (
  lucid: LucidEvolution,
  db: Database,
  txs: { txHash: string; txCbor: string }[]
) =>
  Effect.gen(function* () {
    let utxos: UTxO[] = [];
    for (const rawTx of txs) {
      {
        const tx = lucid.fromTx(rawTx.txCbor);
        const outputs = tx.toTransaction().body().outputs();
        for (let i = 0; i < outputs.len(); i++) {
          const utxo: UTxO = {
            txHash: rawTx.txHash,
            outputIndex: i,
            ...coreToTxOutput(outputs.get(i)),
          };
          utxos.push(utxo);
        }
        return utxos;
      }
    }
    Effect.tryPromise(() => confirmedLedger.insert(db, utxos));
  });

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
  header_hash: string
) =>
  Effect.gen(function* ($) {
    // Fetch transactions from the first block
    const firstBlockTxs = yield* $(fetchFirstBlockTxs(lucid, fetchConfig, db));

    // Build the transaction
    const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
      lucid,
      fetchConfig
    );
    // Submit the transaction
    yield* $(handleSignSubmit(lucid, txBuilder));

    // Begin database transaction
    yield* $(
      Effect.tryPromise(
        () =>
          new Promise<void>((resolve, reject) => {
            db.run("BEGIN TRANSACTION;", (err) => {
              if (err)
                reject(new Error(`Error starting transaction: ${err.message}`));
              else resolve();
            });
          })
      )
    );

    try {
      // Apply transactions to the confirmed ledger
      yield* applyTxsToConfirmedLedger(lucid, db, firstBlockTxs);

      // Remove header hashes
      yield* Effect.tryPromise(() => clearByHeader(db, header_hash));

      // Commit transaction
      yield* $(
        Effect.tryPromise(
          () =>
            new Promise<void>((resolve, reject) => {
              db.run("COMMIT;", (err) => {
                if (err)
                  reject(
                    new Error(`Error committing transaction: ${err.message}`)
                  );
                else resolve();
              });
            })
        )
      );
    } catch (error) {
      // Rollback transaction on failure
      yield* $(
        Effect.tryPromise(
          () =>
            new Promise<void>((resolve) => {
              db.run("ROLLBACK;", () => resolve());
            })
        )
      );
      return yield* $(Effect.fail(new Error(`Transaction failed: ${error}`)));
    }
  });
