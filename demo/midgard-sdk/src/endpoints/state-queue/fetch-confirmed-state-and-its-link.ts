import { Effect } from "effect";
import { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { makeReturn } from "../../core.js";
import {
  StateQueueUTxO,
  getConfirmedStateFromStateQueueUTxO,
  utxosToStateQueueUTxOs,
  findLinkStateQueueUTxO,
} from "../../utils/state-queue.js";
import { StateQueue } from "../../tx-builder/index.js";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";

export const fetchConfirmedStateAndItsLinkProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig
): Effect.Effect<{ confirmed: UTxO; link?: UTxO }, Error> =>
  Effect.gen(function* () {
    const initUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId
    );
    const allUTxOs = yield* utxosToStateQueueUTxOs(initUTxOs, config.stateQueuePolicyId);
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      allUTxOs.map(getConfirmedStateFromStateQueueUTxO)
    );
    if (filteredForConfirmedState.length === 1) {
      const { utxo: confirmedStateUTxO, link: confirmedStatesLink } =
        filteredForConfirmedState[0];
      const linkUTxO = yield* findLinkStateQueueUTxO(confirmedStatesLink, allUTxOs);
      return {
        confirmed: confirmedStateUTxO.utxo,
        link: linkUTxO.utxo,
      };
    } else {
      return yield* Effect.fail(new Error("Confirmed state not found"));
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
  config: StateQueue.FetchConfig
) =>
  makeReturn(fetchConfirmedStateAndItsLinkProgram(lucid, config)).unsafeRun();
