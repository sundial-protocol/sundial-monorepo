import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveCategoryParams = {};

/**
 * Removes a fraud category.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveCategoryParams} params - The parameters for removing the fraud category.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const removeCategory = (
  lucid: LucidEvolution,
  params: RemoveCategoryParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeCategoryProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to remove a fraud category.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveCategoryParams} params - The parameters for removing the fraud category.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to remove the fraud category.
 */
const removeCategoryProgram = (
  lucid: LucidEvolution,
  params: RemoveCategoryParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
