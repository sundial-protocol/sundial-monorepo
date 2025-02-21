import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { initTxBuilder } from "@/tx-builder/state-queue/init.js";
import { Effect } from "effect";
import { InitParams } from "@/types/state-queue.js";

export const initTxProgram = (
  lucid: LucidEvolution,
  initParams: InitParams,
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.gen(function* () {
    const completedTx = yield* initTxBuilder(lucid, initParams);
    return yield* Effect.tryPromise({
      try: () => completedTx.complete(),
      catch: (e) => new Error(`${e}`),
    });
  });

/**
 * Builds completed tx for iniitalizing the state queue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for menting the initialization NFT.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */

export const initTx = (
  lucid: LucidEvolution,
  initParams: InitParams,
): Promise<TxSignBuilder> =>
  makeReturn(initTxProgram(lucid, initParams)).unsafeRun();
