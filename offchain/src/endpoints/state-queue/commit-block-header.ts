import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type CommitParams = {};

export const commit = (
  lucid: LucidEvolution,
  params: CommitParams
): Promise<TxSignBuilder> => {
  return makeReturn(commitProgram(lucid, params)).unsafeRun();
};

const commitProgram = (
  lucid: LucidEvolution,
  params: CommitParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
