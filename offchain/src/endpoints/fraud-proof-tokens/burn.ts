import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type BurnParams = {};

/**
 * Burns tokens using the LucidEvolution instance.
 *
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for the burn operation.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const burn = (
  lucid: LucidEvolution,
  params: BurnParams
): Promise<TxSignBuilder> => {
  return makeReturn(burnProgram(lucid, params)).unsafeRun();
};

/**
 * The program that performs the burn operation.
 *
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for the burn operation.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const burnProgram = (
  lucid: LucidEvolution,
  params: BurnParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
