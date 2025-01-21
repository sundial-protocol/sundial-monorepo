import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveDeplicateParams = {};

/**
 * Removes duplicate entries.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveDeplicateParams} params - The parameters for removing duplicates.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const removeDeplicate = (
  lucid: LucidEvolution,
  params: RemoveDeplicateParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeDeplicateProgram(lucid, params)).unsafeRun();
};

/**
 * Program to remove duplicate entries.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveDeplicateParams} params - The parameters for removing duplicates.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - An effect that resolves to a TxSignBuilder or a TransactionError.
 */
const removeDeplicateProgram = (
  lucid: LucidEvolution,
  params: RemoveDeplicateParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
