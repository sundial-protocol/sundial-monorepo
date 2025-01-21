import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type ActivateParams = {};

/**
 * Activates an operator using the provided LucidEvolution instance and parameters.
 * 
 * @param lucid - The LucidEvolution instance to use for the activation.
 * @param params - The parameters required for activation.
 * @returns A promise that resolves to a TxSignBuilder instance.
 */
export const activate = (
  lucid: LucidEvolution,
  params: ActivateParams
): Promise<TxSignBuilder> => {
  return makeReturn(activateProgram(lucid, params)).unsafeRun();
};

/**
 * The program that performs the activation of an operator.
 * 
 * @param lucid - The LucidEvolution instance to use for the activation.
 * @param params - The parameters required for activation.
 * @returns An Effect that yields a TxSignBuilder instance or a TransactionError.
 */
const activateProgram = (
  lucid: LucidEvolution,
  params: ActivateParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
