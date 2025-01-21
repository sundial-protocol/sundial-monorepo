import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RewindParams = {};

/**
 * Rewinds the state using the provided LucidEvolution instance and parameters.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for the transaction.
 * @param {RewindParams} params - The parameters for the rewind operation.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const rewind = (
  lucid: LucidEvolution,
  params: RewindParams
): Promise<TxSignBuilder> => {
  return makeReturn(rewindProgram(lucid, params)).unsafeRun();
};

/**
 * The program that performs the rewind operation.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for the transaction.
 * @param {RewindParams} params - The parameters for the rewind operation.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect that represents the transaction.
 */
const rewindProgram = (
  lucid: LucidEvolution,
  params: RewindParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
