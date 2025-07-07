import { LucidEvolution } from "@lucid-evolution/lucid";
import { StateQueue } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
import { makeReturn } from "@/core.js";
import { StateQueueUTxO, sortStateQueueUTxOs, utxosToStateQueueUTxOs } from "@/utils/state-queue.js";

export const fetchSortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
): Effect.Effect<StateQueueUTxO[], Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    const unsorted = yield* utxosToStateQueueUTxOs(allUTxOs, config.stateQueuePolicyId);
    return yield* sortStateQueueUTxOs(unsorted);
  });

/**
 * Attempts fetching the whole state queue linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO[]} - All the authentic node UTxOs.
 */
export const fetchSortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueue.FetchConfig,
) => makeReturn(fetchSortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();
