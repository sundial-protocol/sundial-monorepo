import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type WithdrawOrderParams = {};

/**
 * Initiates a withdrawal order.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the withdrawal order.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const withdrawOrder = (
  lucid: LucidEvolution,
  params: WithdrawOrderParams
): Promise<TxSignBuilder> => {
  return makeReturn(withdrawOrderProgram(lucid, params)).unsafeRun();
};

/**
 * Program to handle the withdrawal order logic.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the withdrawal order.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const withdrawOrderProgram = (
  lucid: LucidEvolution,
  params: WithdrawOrderParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
