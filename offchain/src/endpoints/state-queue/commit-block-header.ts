import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type CommitParams = {};

/**
 * Commits a block header.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {CommitParams} params - The commit parameters.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const commit = (
  lucid: LucidEvolution,
  params: CommitParams
): Promise<TxSignBuilder> => {
  return makeReturn(commitProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to commit a block header.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {CommitParams} params - The commit parameters.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to commit the block header.
 */
const commitProgram = (
  lucid: LucidEvolution,
  params: CommitParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
