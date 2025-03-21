import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";
import { inspect } from "node:util";

export const mptFromUTxOs = (
  spentUtxos: Uint8Array[],
  producedUtxos: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    // const data = utxos.map(({ outputReference, output }) => {
    //   return {
    //     key: Buffer.from(outputReference),
    //     value: Buffer.from(output),
    //   };
    // });

    // const trie = yield* Effect.tryPromise({
    //   try: () => Trie.fromList(data, new Store(tempFile)),
    //   catch: (e) => new Error(`${e}`),
    // });

    // return trie;
    yield* Effect.logInfo(`SPENT UTXOS: ${spentUtxos}`);
    yield* Effect.logInfo(`PRODUCED UTXOS: ${producedUtxos}`);

    const store = new Store("utxosStore");

    yield* Effect.tryPromise({
      try: () => store.ready(),
      catch: (e) => new Error(`${e}`),
    });

    const trieProgram = Effect.tryPromise({
      try: async () => Trie.load(store),
      catch: (e) => new Error(`${e}`),
    });

    const trie: Trie = yield* Effect.catchAll(trieProgram, (e) => {
      console.log(`FAILED TO LOAD STORE, STARTING WITH A NEW ONE... ${e}`);
      return Effect.succeed(new Trie(store));
    });

    yield* Effect.logInfo(`UTxO trie loaded: ${inspect(trie)}`);

    yield* Effect.forEach(producedUtxos, ({ outputReference, output }) =>
      Effect.tryPromise({
        try: () =>
          trie.insert(Buffer.from(outputReference), Buffer.from(output)),
        catch: (e) => new Error(`${e}`),
      }),
    );

    // Silently ignoring failed removals.
    yield* Effect.allSuccesses(
      spentUtxos.map((outputReference) =>
        Effect.try(() => trie.delete(Buffer.from(outputReference))),
      ),
    );

    yield* Effect.logInfo(`Updated UTxO trie: ${inspect(trie)}`);

    return trie;
  });

export const mptFromTxs = (
  txs: { txHash: Uint8Array; txCbor: Uint8Array }[],
): Effect.Effect<Trie, Error> =>
  Effect.gen(function* () {
    const trie = new Trie(new Store("txsStore"));

    yield* Effect.forEach(
      txs,
      ({ txHash, txCbor }) =>
        Effect.tryPromise({
          try: () => trie.insert(Buffer.from(txHash), Buffer.from(txCbor)),
          catch: (e) => new Error(`${e}`),
        }),
      { concurrency: 1 }, // omitting this is equivalent to sequential traversal.
    );

    return trie;

    // const data = txs.map(({ txCbor, txHash }) => {
    //   return {
    //     key: Buffer.from(txHash),
    //     value: Buffer.from(txCbor),
    //   };
    // });

    // const trie = yield* Effect.tryPromise({
    //   try: () => Trie.fromList(data, new Store(tempFile)),
    //   catch: (e) => new Error(`${e}`),
    // });

    // return trie;
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
