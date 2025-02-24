import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { InitParams } from "@/types/state-queue.js";
import {
  LucidEvolution,
  TxBuilder,
  Assets,
  toUnit,
  Data,
  OutputDatum,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const initTxBuilder = (
  lucid: LucidEvolution,
  { policyId, address, stateQueueMintingScript }: InitParams,
): Effect.Effect<TxBuilder, Error> => {
  const assets: Assets = {
    [toUnit(policyId, "Node")]: 1n,
  };

  const confirmedState: ConfirmedState = {
    headerHash: "00".repeat(28),
    prevHeaderHash: "00".repeat(28),
    utxoRoot: "00".repeat(32),
    startTime: BigInt(Date.now()),
    endTime: BigInt(Date.now()),
    protocolVersion: 0n,
  };
  const datum: NodeDatum = {
    key: "Empty",
    next: "Empty",
    data: Data.castTo(confirmedState, ConfirmedState),
  };

  const outputDatum: OutputDatum = {
    kind: "inline",
    value: Data.to(datum, NodeDatum),
  };

  const tx = lucid
    .newTx()
    .mintAssets(assets)
    .pay.ToAddressWithData(address, outputDatum, assets)
    .attach.Script(stateQueueMintingScript);
  return Effect.succeed(tx);
};
