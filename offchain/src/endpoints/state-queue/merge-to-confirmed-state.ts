import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type MergeParams = {};

export const merge = (
  lucid: LucidEvolution,
  params: MergeParams
): Promise<TxSignBuilder> => {
  return makeReturn(mergeProgram(lucid, params)).unsafeRun();
};

const mergeProgram = (
  lucid: LucidEvolution,
  params: MergeParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
