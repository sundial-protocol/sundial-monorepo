import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { Data, UTxO } from "@lucid-evolution/lucid";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Either } from "effect";
import { Datum } from "@/types/contracts/state-queue.js";
import {getNodeDatumFromUTxO} from "./linked-list.js";

export const getLinkFromBlockUTxO = (
  blockUTxO: UTxO
): Either.Either<NodeKey, string> => {
  const nodeDatum = getNodeDatumFromUTxO(blockUTxO);
  return Either.map(nodeDatum, (nd) => nd.next)
};

export const getConfirmedStateFromBlockUTxO = (
  blockUTxO: UTxO
): Either.Either<{ data: ConfirmedState; link: NodeKey }, string> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
      if (nodeDatum.key === "Empty") {
        const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
        return Either.right({ data: confirmedState, link: nodeDatum.next });
      } else {
        return Either.left("Given UTxO is not root");
      }
    } catch {
      return Either.left("Could not coerce to a node datum");
    }
  } else {
    return Either.left("No datum found");
  }
};
