import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type NewCategoryParams = {};

/**
 * Creates a new fraud category.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {NewCategoryParams} params - The parameters for the new fraud category.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const newCategory = (
  lucid: LucidEvolution,
  params: NewCategoryParams
): Promise<TxSignBuilder> => {
  return makeReturn(newCategoryProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to create a new fraud category.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {NewCategoryParams} params - The parameters for the new fraud category.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to create the new fraud category.
 */
const newCategoryProgram = (
  lucid: LucidEvolution,
  params: NewCategoryParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
