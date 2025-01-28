import {
  Config as FetchConfig,
  fetchLatestCommitedBlockProgram,
} from "@/endpoints/state-queue/fetch-latest-block.js";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type Params = {};

/**
 * Commit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const commitTxBuilder = (
  lucid: LucidEvolution,
  config: FetchConfig,
  params: Params
): Effect.Effect<TxBuilder, string> =>
  Effect.gen(function* () {
    const latestBlock = yield* fetchLatestCommitedBlockProgram(lucid, config);
    const tx = lucid.newTx();
    return tx;
  });
