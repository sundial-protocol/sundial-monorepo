import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type DepositParams = {};

/**
 * Initiates a deposit transaction.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the deposit.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const deposit = (
  lucid: LucidEvolution,
  params: DepositParams
): Promise<TxSignBuilder> => {
  return makeReturn(depositProgram(lucid, params)).unsafeRun();
};

/**
 * Defines the deposit program logic.
 *
 * @param lucid - An instance of LucidEvolution.
 * @param params - Parameters for the deposit.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const depositProgram = (
  lucid: LucidEvolution,
  params: DepositParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
