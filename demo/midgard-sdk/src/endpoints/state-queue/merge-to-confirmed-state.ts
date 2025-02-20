import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { FetchConfig, MergeParams } from "@/types/state-queue.js";

export const mergeToConfirmedStateProgram = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
  mergeParams: MergeParams,
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.gen(function* () {
    const completedTx = yield* StateQueue.mergeTxBuilder(
      lucid,
      fetchConfig,
      mergeParams,
    );
    return yield* Effect.tryPromise({
      try: () => completedTx.complete(),
      catch: (e) => new Error(`${e}`),
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
  fetchConfig: FetchConfig,
  mergeParams: MergeParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    mergeToConfirmedStateProgram(lucid, fetchConfig, mergeParams),
  ).unsafeRun();
