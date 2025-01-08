import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type InitStateQueueParams = {};

export const initStateQueue = (
  lucid: LucidEvolution,
  params: InitStateQueueParams
): Promise<TxSignBuilder> => {
  return Effect.runPromise(initStateQueueProgram(lucid, params));
};

const initStateQueueProgram = (
  lucid: LucidEvolution,
  params: InitStateQueueParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
