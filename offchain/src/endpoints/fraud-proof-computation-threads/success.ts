import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type SuccessParams = {};

/**
 * Executes the success program and returns a TxSignBuilder.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the success program.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const success = (
  lucid: LucidEvolution,
  params: SuccessParams
): Promise<TxSignBuilder> => {
  return makeReturn(successProgram(lucid, params)).unsafeRun();
};

/**
 * Defines the success program effect.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the success program.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const successProgram = (
  lucid: LucidEvolution,
  params: SuccessParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
