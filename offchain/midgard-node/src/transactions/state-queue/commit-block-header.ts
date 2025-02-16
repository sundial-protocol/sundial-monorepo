// Build a tx Merkle root with all the mempool txs

import { coreToTxOutput, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Database } from "sqlite3";
import * as SDK from "@al-ft/midgard-sdk";
import { buildMerkleRoots, handleSignSubmit } from "../utils.js";
import * as latestLedger from "../../database/latestLedger.js";
import * as mempool from "../../database/mempool.js";
import * as immutable from "../../database/immutable.js";

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear MempoolDB, and inject all the processed txs into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: Database,
  fetchConfig: SDK.Types.FetchConfig,
  endTime: number,
) =>
  Effect.gen(function* () {
    // Fetch transactions from the first block
    const txList = yield* Effect.tryPromise(() => mempool.retrieve(db));
    const txs = txList.map(([txHash, txCbor]) => ({ txHash, txCbor }));
    const { txRoot, utxoRoot } = yield* buildMerkleRoots(lucid, txs);
    // Build commitment block
    const commitBlockParams: SDK.Types.CommitBlockParams = {
      newUTxOsRoot: utxoRoot.hash.toString(),
      transactionsRoot: txRoot.hash.toString(),
      endTime: BigInt(endTime),
    };
    const aoUpdateCommitmentTimeParams = {};
    const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
      lucid,
      fetchConfig,
      commitBlockParams,
      aoUpdateCommitmentTimeParams,
    );
    // Submit the transaction
    yield* handleSignSubmit(lucid, txBuilder);

    try {
      // Begin database transaction
      yield* Effect.tryPromise(
        () =>
          new Promise<void>((resolve, reject) => {
            db.run("BEGIN TRANSACTION;", (err) => {
              if (err)
                reject(new Error(`Error starting transaction: ${err.message}`));
              else resolve();
            });
          }),
      );

      // Clear lastest ledger
      yield* Effect.tryPromise(() => latestLedger.clear(db));
      // Insert the new utxo set to the lastest ledger
      yield* applyTxsToLatestLedgerDB(lucid, db, txs);
      // Clear mempool
      yield* Effect.tryPromise(() => mempool.clear(db));
      // Insert the processed txs to ImmutableDB
      yield* Effect.tryPromise(() => immutable.insertTxs(db, txs));
      // Commit transaction
      yield* Effect.tryPromise(
        () =>
          new Promise<void>((resolve, reject) => {
            db.run("COMMIT;", (err) => {
              if (err)
                reject(
                  new Error(`Error committing transaction: ${err.message}`),
                );
              else resolve();
            });
          }),
      );
    } catch (error) {
      // Rollback transaction on failure
      yield* Effect.tryPromise(
        () =>
          new Promise<void>((resolve) => {
            db.run("ROLLBACK;", () => resolve());
          }),
      );
      return yield* Effect.fail(new Error(`Transaction failed: ${error}`));
    }
  });

export const applyTxsToLatestLedgerDB = (
  lucid: LucidEvolution,
  db: Database,
  txs: { txHash: string; txCbor: string }[],
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
      }
    }
    Effect.tryPromise(() => latestLedger.insert(db, utxos));
  });
