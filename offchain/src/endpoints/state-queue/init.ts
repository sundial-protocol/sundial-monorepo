import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type InitParams = {};

/**
 * Initializes a transaction signing builder.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {InitParams} params - The initialization parameters.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const init = (
  lucid: LucidEvolution,
  params: InitParams
): Promise<TxSignBuilder> => {
  return makeReturn(initProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to initialize a transaction signing builder.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {InitParams} params - The initialization parameters.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to initialize the transaction signing builder.
 */
const initProgram = (
  lucid: LucidEvolution,
  params: InitParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
