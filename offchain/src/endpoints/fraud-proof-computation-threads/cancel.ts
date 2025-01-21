import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type CancelParams = {};

/**
 * Cancels a transaction using the provided LucidEvolution instance and parameters.
 * 
 * @param lucid - The LucidEvolution instance to use for the transaction.
 * @param params - The parameters for the cancellation.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const cancel = (
  lucid: LucidEvolution,
  params: CancelParams
): Promise<TxSignBuilder> => {
  return makeReturn(cancelProgram(lucid, params)).unsafeRun();
};

/**
 * The program that performs the cancellation of a transaction.
 * 
 * @param lucid - The LucidEvolution instance to use for the transaction.
 * @param params - The parameters for the cancellation.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const cancelProgram = (
  lucid: LucidEvolution,
  params: CancelParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
