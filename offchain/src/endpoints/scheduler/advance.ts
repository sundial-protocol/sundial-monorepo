import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type AdvanceParams = {};

/**
 * Advances the state of the LucidEvolution instance.
 *
 * @param lucid - The LucidEvolution instance to advance.
 * @param params - The parameters for advancing the state.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const advance = (
  lucid: LucidEvolution,
  params: AdvanceParams
): Promise<TxSignBuilder> => {
  return makeReturn(advanceProgram(lucid, params)).unsafeRun();
};

/**
 * The program that defines the advancement logic for the LucidEvolution instance.
 *
 * @param lucid - The LucidEvolution instance to advance.
 * @param params - The parameters for advancing the state.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const advanceProgram = (
  lucid: LucidEvolution,
  params: AdvanceParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
