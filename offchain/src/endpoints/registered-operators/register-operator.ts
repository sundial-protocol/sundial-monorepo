import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RegisterParams = {};

/**
 * Registers an operator using the provided LucidEvolution instance and parameters.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for the registration.
 * @param {RegisterParams} params - The parameters for the registration.
 * @returns {Promise<TxSignBuilder>} A promise that resolves to a TxSignBuilder instance.
 */
export const register = (
  lucid: LucidEvolution,
  params: RegisterParams
): Promise<TxSignBuilder> => {
  return makeReturn(registerProgram(lucid, params)).unsafeRun();
};

/**
 * The program that handles the registration logic.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for the registration.
 * @param {RegisterParams} params - The parameters for the registration.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} An Effect that resolves to a TxSignBuilder instance.
 */
const registerProgram = (
  lucid: LucidEvolution,
  params: RegisterParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
