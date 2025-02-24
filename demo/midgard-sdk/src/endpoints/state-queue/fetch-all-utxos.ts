import { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { FetchConfig } from "@/types/state-queue.js";
import { Effect } from "effect";
import {
  getSingleAssetApartFromAda,
  utxosAtByNFTPolicyId,
} from "@/utils/common.js";
import { makeReturn } from "@/core.js";

export const fetchAllStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: FetchConfig,
): Effect.Effect<{ utxo: UTxO; assetName: string }[], Error> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    return yield* Effect.allSuccesses(
      allUTxOs.map((u: UTxO) => {
        const singleAssetProgram = getSingleAssetApartFromAda(u.assets);
        return Effect.map(singleAssetProgram, ([_sym, tn, _qty]) => {
          // Quantity of 1 and policy ID are already checked.
          return { utxo: u, assetName: tn };
        });
      }),
    );
  });

/**
 * Attempts fetching the whole state queue linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO[]} - All the authentic node UTxOs.
 */
export const fetchAllStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: FetchConfig,
) => makeReturn(fetchAllStateQueueUTxOsProgram(lucid, config)).unsafeRun();
