import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type MergeParams = {};

/**
 * Merges the state to a confirmed state.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {MergeParams} params - The merge parameters.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const merge = (
  lucid: LucidEvolution,
  params: MergeParams
): Promise<TxSignBuilder> => {
  return makeReturn(mergeProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to merge the state to a confirmed state.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {MergeParams} params - The merge parameters.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to merge the state to a confirmed state.
 */
const mergeProgram = (
  lucid: LucidEvolution,
  params: MergeParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
