import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type DeinitParams = {};

/**
 * Deinitializes a transaction signing builder.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {DeinitParams} params - The deinitialization parameters.
 * @returns {Promise<TxSignBuilder>} - A promise that resolves to a TxSignBuilder.
 */
export const deinit = (
  lucid: LucidEvolution,
  params: DeinitParams
): Promise<TxSignBuilder> => {
  return makeReturn(deinitProgram(lucid, params)).unsafeRun();
};

/**
 * Creates an effect to deinitialize a transaction signing builder.
 * 
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {DeinitParams} params - The deinitialization parameters.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} - The effect to deinitialize the transaction signing builder.
 */
const deinitProgram = (
  lucid: LucidEvolution,
  params: DeinitParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
