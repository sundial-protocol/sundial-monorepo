import {
  Config as FetchConfig,
  fetchLatestCommitedBlockProgram,
} from "@/endpoints/state-queue/fetch-latest-block.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import {
  Data,
  LucidEvolution,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { MerkleRoot, POSIXTime } from "@/types/contracts/common.js";
import { hashHexWithBlake2b224 } from "@/utils/helpers.js";

export type Params = {
  newUTxOsRoot: MerkleRoot;
  transactionsRoot: MerkleRoot;
  endTime: POSIXTime;
};

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
  { newUTxOsRoot, transactionsRoot, endTime }: Params
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
    const eithPrevHeaderHash = hashHexWithBlake2b224(
      Data.to(latestHeader, Header)
    );
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
        .collectFrom([latestBlock], "d87980")
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
