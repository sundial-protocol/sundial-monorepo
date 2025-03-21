import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";

const tempFile = "./tmp";

export const mptFromUTxOs = (
  utxos: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const data = utxos.map(({ outputReference, output }) => {
      return {
        key: Buffer.from(outputReference),
        value: Buffer.from(output),
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data, new Store(tempFile)),
      catch: (e) => new Error(`${e}`),
    });

    return trie;
  });

export const mptFromTxs = (
  txs: { txHash: Uint8Array; txCbor: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    // const trie = yield* Effect.tryPromise({
    //   try: () => Trie.fromList([], new Store(tempFile)),
    //   catch: (e) => new Error(`${e}`),
    // });

    // yield* Effect.forEach(
    //   txs,
    //   ({ txHash, txCbor }) =>
    //     Effect.tryPromise({
    //       try: () =>
    //         trie.insert(
    //           Buffer.from(hexToBytes(txHash)),
    //           Buffer.from(hexToBytes(txCbor)),
    //         ),
    //       catch: (e) => new Error(`${e}`),
    //     }),
    //   { concurrency: 1 }, // omitting this is equivalent to sequential traversal.
    // );

    // return trie;

    const data = txs.map(({ txCbor, txHash }) => {
      return {
        key: Buffer.from(txHash),
        value: Buffer.from(txCbor),
      };
    });

    const trie = yield* Effect.tryPromise({
      try: () => Trie.fromList(data, new Store(tempFile)),
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

    const trie = yield* Effect.promise(() =>
      Trie.fromList(data, new Store(tempFile)),
    );
    return trie;
  });
