import { Effect, Either } from "effect";
import { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
import { makeReturn } from "@/core.js";
import { getConfirmedStateFromUTxO } from "@/utils/state-queue.js";
import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { FetchConfig } from "@/types/state-queue.js";

export const fetchConfirmedStateAndItsLinkProgram = (
  lucid: LucidEvolution,
  config: FetchConfig
): Effect.Effect<{ confirmed: UTxO; link?: UTxO }, string> =>
  Effect.gen(function* () {
    const allBlocks = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId
    );
    let confirmedStateResult:
      | { data: ConfirmedState; link: NodeKey }
      | undefined;
    const filteredForConfirmedState = allBlocks.filter((u: UTxO) => {
      const eithConfirmedState = getConfirmedStateFromUTxO(u);
      if (Either.isRight(eithConfirmedState)) {
        confirmedStateResult = eithConfirmedState.right;
        return true;
      } else {
        return false;
      }
    });
    if (filteredForConfirmedState.length === 1 && confirmedStateResult) {
      const confirmedStateUTxO = filteredForConfirmedState[0];
      if (confirmedStateResult.link !== "Empty") {
        const firstLink = confirmedStateResult.link.Key;
        const filteredForLink = allBlocks.filter((u: UTxO) => {
          const eithNodeDatum = getNodeDatumFromUTxO(u);
          if (
            Either.isRight(eithNodeDatum) &&
            eithNodeDatum.right.key !== "Empty"
          ) {
            return eithNodeDatum.right.key.Key === firstLink;
          } else {
            return false;
          }
        });
        if (filteredForLink.length === 1) {
          return { confirmed: confirmedStateUTxO, link: filteredForLink[0] };
        } else {
          return yield* Effect.fail("Confirmed state's link not found");
        }
      } else {
        return { confirmed: confirmedStateUTxO };
      }
    } else {
      return yield* Effect.fail("Confirmed state not found");
    }
  });

/**
 * Attempts fetching the confirmed state, i.e. the root node of the state queue
 * linked list, along with its link (i.e. first non-root node in the list).
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which is the root node.
 */
export const fetchConfirmedStateAndItsLink = (
  lucid: LucidEvolution,
  config: FetchConfig
) =>
  makeReturn(fetchConfirmedStateAndItsLinkProgram(lucid, config)).unsafeRun();
