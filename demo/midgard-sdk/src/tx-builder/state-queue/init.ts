import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { LucidEvolution, TxBuilder, PolicyId, Address, Assets, toUnit, Data, OutputDatum, CBORHex } from "@lucid-evolution/lucid";
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
      headerHash: "00".repeat(28)
    , prevHeaderHash: "00".repeat(28)
    , utxoRoot: "00".repeat(32)
    , startTime : BigInt(Date.now())
    , endTime : BigInt(Date.now())
    , protocolVersion: 0n
  }
  const datum : NodeDatum = {
      key: "Empty"
    , next: "Empty"
    , data: Data.castTo(confirmedState, ConfirmedState)
  }

  const outputDatum : OutputDatum = {
      kind: "inline"
    , value: Data.to(datum, NodeDatum)
  }

  tx.mintAssets(assets)
  tx.pay.ToAddressWithData(params.address, outputDatum, assets)
  return Effect.succeed(tx)
};

