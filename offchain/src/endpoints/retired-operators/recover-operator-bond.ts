import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RecoverOperatorParams = {};

/**
 * Recovers the operator bond.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RecoverOperatorParams} params - The parameters for recovering the operator.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const recoverOperator = (
  lucid: LucidEvolution,
  params: RecoverOperatorParams
): Promise<TxSignBuilder> => {
  return makeReturn(recoverOperatorProgram(lucid, params)).unsafeRun();
};

/**
 * The program to recover the operator bond.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RecoverOperatorParams} params - The parameters for recovering the operator.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to recover the operator bond.
 */
const recoverOperatorProgram = (
  lucid: LucidEvolution,
  params: RecoverOperatorParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
