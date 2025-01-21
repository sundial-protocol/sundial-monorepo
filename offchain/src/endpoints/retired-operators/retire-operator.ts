import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RetireOperatorParams = {};

/**
 * Retires an operator by creating and signing a transaction.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for transaction creation.
 * @param {RetireOperatorParams} params - The parameters required to retire the operator.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder instance.
 */
export const retireOperator = (
  lucid: LucidEvolution,
  params: RetireOperatorParams
): Promise<TxSignBuilder> => {
  return makeReturn(retireOperatorProgram(lucid, params)).unsafeRun();
};

/**
 * Program to retire an operator.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for transaction creation.
 * @param {RetireOperatorParams} params - The parameters required to retire the operator.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - An Effect that yields a TxSignBuilder instance.
 */
const retireOperatorProgram = (
  lucid: LucidEvolution,
  params: RetireOperatorParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
