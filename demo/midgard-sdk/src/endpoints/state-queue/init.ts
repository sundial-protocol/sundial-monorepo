import { LucidEvolution, TxBuilder, Assets, PolicyId, toUnit, TxSignBuilder } from "@lucid-evolution/lucid";
import { InitParams, initTxBuilder } from "@/tx-builder/state-queue/init.js";
import { Effect } from "effect";

export const initTx = (
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
