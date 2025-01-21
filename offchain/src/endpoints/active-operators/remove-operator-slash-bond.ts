import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveParams = {};

/**
 * Removes an operator's slash bond.
 * 
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for the removal.
 * @returns A promise that resolves to a TxSignBuilder.
 */
export const remove = (
  lucid: LucidEvolution,
  params: RemoveParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeProgram(lucid, params)).unsafeRun();
};

/**
 * The program to remove an operator's slash bond.
 * 
 * @param lucid - The LucidEvolution instance.
 * @param params - The parameters for the removal.
 * @returns An Effect that yields a TxSignBuilder or a TransactionError.
 */
const removeProgram = (
  lucid: LucidEvolution,
  params: RemoveParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
