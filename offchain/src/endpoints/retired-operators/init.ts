import { makeReturn } from "../../core.js";
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
 * @param lucid - An instance of LucidEvolution.
 * @param params - Initialization parameters.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const init = (
  lucid: LucidEvolution,
  params: InitParams
): Promise<TxSignBuilder> => {
  return makeReturn(initProgram(lucid, params)).unsafeRun();
};

/**
 * Program to initialize a transaction.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Initialization parameters.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const initProgram = (
  lucid: LucidEvolution,
  params: InitParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
