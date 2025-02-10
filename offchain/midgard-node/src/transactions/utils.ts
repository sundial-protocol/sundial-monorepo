import {
  coreToTxOutput,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
) =>
  Effect.gen(function* () {
    const signed = yield* signBuilder.sign.withWallet().completeProgram();
    const txHash = yield* signed.submitProgram();
    yield* Effect.logDebug(`🚀 Transaction submitted: ${txHash}`);
    yield* Effect.logDebug(`Confirming Transaction...`);
    yield* Effect.tryPromise(() => lucid.awaitTx(txHash, 40_000));
    yield* Effect.logDebug(`✅ Transaction confirmed: ${txHash}`);
    yield* Effect.logDebug("Pausing for 10 seconds...");
    yield* Effect.sleep("10 seconds");
  });

/**
 * Build both Merkle roots (transactions and updated UTxO set).
 *
 * @param lucid - The LucidEvolution instance.
 * @param txs - An array of transactions.
 * @returns An Effect that resolves to an object containing the transaction root and UTxO root.
 */
export const buildMerkleRoots = (
  lucid: LucidEvolution,
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
        return utxos;
      }
    }
    const txRoot = SDK.Utils.mptFromList(txs.map((tx) => tx.txCbor));
    const utxoRoot = SDK.Utils.mptFromList(utxos);
    return { txRoot, utxoRoot };
  });
