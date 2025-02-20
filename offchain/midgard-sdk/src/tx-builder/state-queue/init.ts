import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { LucidEvolution, TxBuilder, Assets, PolicyId, toUnit, Address, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type InitParams = {
    policyId : PolicyId
  , address : Address
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
  const assets : Assets = {
    [toUnit(params.policyId, "Init")]: 1n,
   }

  const confirmedState: ConfirmedState =  {
      headerHash: ""
    , prevHeaderHash: ""
    , utxoRoot: ""
    , startTime : 0n
    , endTime : 0n
    , protocolVersion: 0n
  }
  const datum : NodeDatum = {
    key: { Key: { key: "" } },
    next: { Key: { key: "" } },
    data: Data.Object(confirmedState)
  }

  tx.mintAssets(assets)
  tx.pay.ToAddressWithData(params.address, datum, assets)
  return Effect.succeed(tx)
};
