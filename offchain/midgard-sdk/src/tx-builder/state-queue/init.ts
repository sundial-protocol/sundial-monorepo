import { LucidEvolution, TxBuilder, Assets, PolicyId, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type InitParams = {
  policyId : PolicyId
};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const initTxBuilder = (
  lucid: LucidEvolution,
  params: InitParams
): Effect.Effect <TxBuilder, Error> => {
  const tx = lucid.newTx();
  const assets : Assets ={
    [toUnit(params.policyId, "Init")]: 1n,
   }

  tx.mintAssets(assets)
  return Effect.succeed(tx)
};
