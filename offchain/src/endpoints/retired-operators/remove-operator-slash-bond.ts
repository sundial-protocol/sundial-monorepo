import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveOperatorParams = {};

/**
 * Removes an operator by creating and signing a transaction.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for removing the operator.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const removeOperator = (
  lucid: LucidEvolution,
  params: RemoveOperatorParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeOperatorProgram(lucid, params)).unsafeRun();
};

/**
 * Program to remove an operator.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for removing the operator.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const removeOperatorProgram = (
  lucid: LucidEvolution,
  params: RemoveOperatorParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
