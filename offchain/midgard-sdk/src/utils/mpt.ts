import { HeaderHash } from "../types/contract/ledger-state.js";
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { Effect } from "effect";

export const fromHeaderHashes = (
  hashes: HeaderHash[]
): Effect.Effect<Trie, never, never> =>
  Effect.gen(function* () {
    const data = hashes.map((hash) => ({
      key: hash,
      value: "",
    }));

    const trie = yield* Effect.promise(() => Trie.fromList(data));
    return trie;
  });
