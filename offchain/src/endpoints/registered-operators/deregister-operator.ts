import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type DeregisterParams = {};

/**
 * Deregisters an operator.
 *
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for deregistration.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const deregister = (
  lucid: LucidEvolution,
  params: DeregisterParams
): Promise<TxSignBuilder> => {
  return makeReturn(deregisterProgram(lucid, params)).unsafeRun();
};

/**
 * The deregistration program.
 *
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for deregistration.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const deregisterProgram = (
  lucid: LucidEvolution,
  params: DeregisterParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
