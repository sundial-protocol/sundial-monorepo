import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { errorToString } from "@/utils/common.js";
import { FetchConfig } from "@/types/state-queue.js";

export const mergeToConfirmedStateProgram = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig
): Effect.Effect<TxSignBuilder, string> =>
  Effect.gen(function* () {
    const completedTx = yield* StateQueue.mergeTxBuilder(lucid, fetchConfig);
    return yield* Effect.tryPromise({
      try: () => completedTx.complete(),
      catch: errorToString,
    });
  });

/**
 * Builds completed tx for merging the first block in queue to be merged into
 * the confirmed state.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const mergeToConfirmedState = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig
): Promise<TxSignBuilder> =>
  makeReturn(mergeToConfirmedStateProgram(lucid, fetchConfig)).unsafeRun();
