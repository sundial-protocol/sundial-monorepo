import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { errorToString } from "@/utils/common.js";
import { CommitBlockParams, FetchConfig } from "@/types/state-queue.js";
import { fetchLatestCommitedBlockProgram } from "@/endpoints/state-queue/fetch-latest-block.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { hashHeader } from "@/utils/ledger-state.js";

/**
 * Builds portions of a tx required for submitting a new block, using the
 * provided `LucidEvolution` instance, fetch config, and required parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param commitParams - Parameters required for committing to state queue.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const commitTxBuilder = (
  lucid: LucidEvolution,
  config: FetchConfig,
  { newUTxOsRoot, transactionsRoot, endTime }: CommitBlockParams
): Effect.Effect<TxBuilder, string> =>
  Effect.gen(function* () {
    const latestBlock = yield* fetchLatestCommitedBlockProgram(lucid, config);
    const eithLatestNodeDatum = getNodeDatumFromUTxO(latestBlock);
    const latestHeaderProgram: Effect.Effect<Header, string> = Effect.try({
      try: () =>
        Data.castFrom(Either.getOrThrow(eithLatestNodeDatum).data, Header),
      catch: (e) => `Failed coercing latest block's datum: ${errorToString(e)}`,
    });
    const latestHeader: Header = yield* latestHeaderProgram;
    const eithPrevHeaderHash = hashHeader(latestHeader);
    if (Either.isRight(eithPrevHeaderHash)) {
      const newHeader = {
        ...latestHeader,
        prevUtxosRoot: latestHeader.utxosRoot,
        utxosRoot: newUTxOsRoot,
        transactionsRoot,
        startTime: latestHeader.endTime,
        endTime,
        prevHeaderHash: eithPrevHeaderHash.right,
      };
      const tx = lucid
        .newTx()
        .validFrom(Number(latestHeader.endTime))
        .validTo(Number(endTime))
        .collectFrom([latestBlock], "d87980") // TODO: Placeholder redeemer.
        .pay.ToContract(
          config.stateQueueAddress,
          { kind: "inline", value: Data.to(newHeader, Header) },
          latestBlock.assets
        );
      return tx;
    } else {
      return yield* Effect.fail(eithPrevHeaderHash.left);
    }
  });
