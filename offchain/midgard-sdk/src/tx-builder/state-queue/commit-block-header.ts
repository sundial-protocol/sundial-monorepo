import {
  Config as FetchConfig,
  fetchLatestCommitedBlockProgram,
} from "@/endpoints/state-queue/fetch-latest-block.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

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
    const eithLatestNodeDatum = getNodeDatumFromUTxO(latestBlock);
    const latestHeader = yield* Effect.try({
      try: () => {
        if (Either.isRight(eithLatestNodeDatum)) {
          return Data.castFrom(eithLatestNodeDatum.right.data, Header);
        } else {
          throw new Error();
        }
      },
      catch: (_) => "Failed coercing latest block's datum",
    });
    const newHeader = {
      ...latestHeader,
    };
    const tx = lucid.newTx();
    return tx;
  });
