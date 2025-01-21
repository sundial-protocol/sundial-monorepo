import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type TransactionOrderParams = {};

/**
 * Creates a transaction order using the provided LucidEvolution instance and parameters.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for creating the transaction.
 * @param {TransactionOrderParams} params - The parameters for the transaction order.
 * @returns {Promise<TxSignBuilder>} A promise that resolves to a TxSignBuilder instance.
 */
export const transactionOrder = (
  lucid: LucidEvolution,
  params: TransactionOrderParams
): Promise<TxSignBuilder> => {
  return makeReturn(transactionOrderProgram(lucid, params)).unsafeRun();
};

/**
 * The program that generates a transaction order.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for creating the transaction.
 * @param {TransactionOrderParams} params - The parameters for the transaction order.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} An Effect that yields a TxSignBuilder instance.
 */
const transactionOrderProgram = (
  lucid: LucidEvolution,
  params: TransactionOrderParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
