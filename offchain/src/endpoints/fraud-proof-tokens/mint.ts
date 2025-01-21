import { makeReturn } from "../../core.js";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type MintParams = {};

/**
 * Mints a new token using the provided LucidEvolution instance and parameters.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for minting.
 * @param {MintParams} params - The parameters for the minting process.
 * @returns {Promise<TxSignBuilder>} A promise that resolves to a TxSignBuilder instance.
 */
export const mint = (
  lucid: LucidEvolution,
  params: MintParams
): Promise<TxSignBuilder> => {
  return makeReturn(mintProgram(lucid, params)).unsafeRun();
};

/**
 * The minting program that generates a new transaction.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance to use for minting.
 * @param {MintParams} params - The parameters for the minting process.
 * @returns {Effect.Effect<TxSignBuilder, TransactionError, never>} An Effect that yields a TxSignBuilder instance.
 */
const mintProgram = (
  lucid: LucidEvolution,
  params: MintParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
