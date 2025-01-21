import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveParams = {};

/**
 * Removes a fraudulent block header.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveParams} params - The remove parameters.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const remove = (
  lucid: LucidEvolution,
  params: RemoveParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to remove a fraudulent block header.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {RemoveParams} params - The remove parameters.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to remove the fraudulent block header.
 */
const removeProgram = (
  lucid: LucidEvolution,
  params: RemoveParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
