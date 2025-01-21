import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type TransactionRequestParams = {};

/**
 * Initiates a transaction request.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the transaction request.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const transactionRequest = (
  lucid: LucidEvolution,
  params: TransactionRequestParams
): Promise<TxSignBuilder> => {
  return makeReturn(transactionRequestProgram(lucid, params)).unsafeRun();
};

/**
 * Program to handle the transaction request.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the transaction request.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const transactionRequestProgram = (
  lucid: LucidEvolution,
  params: TransactionRequestParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
