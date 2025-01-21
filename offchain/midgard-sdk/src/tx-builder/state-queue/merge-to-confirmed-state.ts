import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type MergeParams = {};

/**
 * Merge
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const mergeTxBuilder = (
  lucid: LucidEvolution,
  params: MergeParams
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
