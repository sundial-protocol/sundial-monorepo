import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RetireParams = {};

/**
 * Retires an operator by creating and signing a transaction.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for transaction creation.
 * @param {RetireParams} params - The parameters for retiring the operator.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder instance.
 */
export const retire = (
  lucid: LucidEvolution,
  params: RetireParams
): Promise<TxSignBuilder> => {
  return makeReturn(retireProgram(lucid, params)).unsafeRun();
};

/**
 * The program that handles the logic for retiring an operator.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for transaction creation.
 * @param {RetireParams} params - The parameters for retiring the operator.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - An Effect that yields a TxSignBuilder instance.
 */
const retireProgram = (
  lucid: LucidEvolution,
  params: RetireParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
