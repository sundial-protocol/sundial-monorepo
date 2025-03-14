import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";
import { UTxO, utxoToTransactionOutput, Data } from "@lucid-evolution/lucid";
import { OutputReference } from "@/tx-builder/common.js";

export const mptFromUTxOs = (utxos: UTxO[]): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const data = utxos.map((utxo) => {
      const outRef: OutputReference = {
        outputIndex: BigInt(utxo.outputIndex),
        txHash: { hash: utxo.txHash },
      };
      const cmlOutput = utxoToTransactionOutput(utxo);
      return {
        key: Data.to(outRef, OutputReference),
        value: cmlOutput.to_cbor_hex(),
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data),
      catch: (e) => new Error(`${e}`),
    });

    return trie;
  });

export const mptFromTxs = (
  txs: { txHash: string; txCbor: string }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const data = txs.map(({ txHash, txCbor }) => {
      return {
        key: txHash,
        value: txCbor,
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data),
      catch: (e) => new Error(`${e}`),
    });

    return trie;
  });

/**
 * Create a Merkle Patricia Trie (MPT) from a list of items.
 *
 * @param items - An array of items to be included in the trie.
 * @returns An Effect that resolves to the created Trie.
 */
export const mptFromList = <T>(items: T[]): Effect.Effect<Trie, never, never> =>
  Effect.gen(function* () {
    const data = items.map((item) => ({
      key: item,
      value: "",
    }));

    const trie = yield* Effect.promise(() => Trie.fromList(data));
    return trie;
  });
