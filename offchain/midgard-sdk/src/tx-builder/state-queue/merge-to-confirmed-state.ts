import { Effect, Either } from "effect";
import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  toUnit,
} from "@lucid-evolution/lucid";
import { fetchConfirmedStateAndItsLinkProgram } from "@/endpoints/state-queue/fetch-confirmed-state-and-its-link.js";
import { FetchConfig } from "@/types/state-queue.js";
import { errorToString, getSingleAssetApartFromAda } from "@/utils/common.js";
import { ConfirmedState, Header } from "@/types/contracts/ledger-state.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { hashHeader } from "@/utils/ledger-state.js";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { Redeemer } from "@/types/contracts/state-queue.js";

/**
 * Merge
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const mergeTxBuilder = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
): Effect.Effect<TxBuilder, string> =>
  Effect.gen(function* () {
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig);
    if (!firstBlockUTxO) {
      return yield* Effect.fail("No blocks in queue");
    } else {
      const eithConfirmedNode = getNodeDatumFromUTxO(confirmedUTxO);
      const eithBlockNodeDatum = getNodeDatumFromUTxO(firstBlockUTxO);
      if (
        Either.isRight(eithConfirmedNode) &&
        Either.isRight(eithBlockNodeDatum)
      ) {
        const currentConfirmedNodeDatum = eithConfirmedNode.right;
        const currentConfirmedState: ConfirmedState = yield* Effect.try({
          try: () =>
            Data.castFrom(currentConfirmedNodeDatum.data, ConfirmedState),
          catch: (e) => errorToString(e),
        });
        const blockNodeDatum = eithBlockNodeDatum.right;
        const blockHeader: Header = yield* Effect.try({
          try: () => Data.castFrom(blockNodeDatum.data, Header),
          catch: (e) => errorToString(e),
        });
        const headerHash = yield* Effect.try({
          try: () => Either.getOrThrow(hashHeader(blockHeader)),
          catch: (e) => `Failed to hash header: ${errorToString(e)}`,
        });
        const newConfirmedState = {
          ...currentConfirmedState,
          headerHash,
          prevHeaderHash: currentConfirmedState.headerHash,
          utxoRoot: blockHeader.utxosRoot,
          startTime: currentConfirmedState.endTime,
          endTime: blockHeader.endTime,
        };
        const newConfirmedNodeDatum: NodeDatum = {
          ...currentConfirmedNodeDatum,
          data: Data.castTo(newConfirmedState, ConfirmedState),
        };
        const eithBlocksNFT = getSingleAssetApartFromAda(firstBlockUTxO.assets);
        const assetsToBurn: Assets = yield* Effect.try({
          try: () => {
            const [nftSym, nftName, _nftQty] = Either.getOrThrow(eithBlocksNFT);
            return {
              [toUnit(nftSym, nftName)]: -1n,
            };
          },
          catch: (e) =>
            `First blocks assets are not valid: ${errorToString(e)}`,
        });
        const tx = lucid
          .newTx()
          .collectFrom(
            [confirmedUTxO, firstBlockUTxO],
            Data.to("MergeToConfirmedState", Redeemer)
          )
          .pay.ToContract(
            fetchConfig.stateQueueAddress,
            {
              kind: "inline",
              value: Data.to(newConfirmedNodeDatum, NodeDatum),
            },
            confirmedUTxO.assets
          )
          .mintAssets(assetsToBurn);
        return tx;
      } else {
        return yield* Effect.fail(`
- Coercing confirmed node failed with:           ${
          Either.isLeft(eithConfirmedNode)
            ? eithConfirmedNode.left
            : "<did not fail>"
        }
- Coercing first block's node datum failed with: ${
          Either.isLeft(eithBlockNodeDatum)
            ? eithBlockNodeDatum.left
            : "<did not fail>"
        }
`);
      }
    }
  });
