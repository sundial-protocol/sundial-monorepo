import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type CommitParams = {};

/**
 * Commit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const commitTxBuilder = (
  lucid: LucidEvolution,
  params: CommitParams
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
