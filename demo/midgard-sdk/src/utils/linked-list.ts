import { Effect } from "effect";
import { NodeDatum } from "../tx-builder/linked-list.js";
import { Data, UTxO } from "@lucid-evolution/lucid";
import { StateQueueError } from "./common.js";

export const getNodeDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<NodeDatum, StateQueueError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, NodeDatum);
      return Effect.succeed(nodeDatum);
    } catch {
      return Effect.fail(new StateQueueError({message: "Could not coerce to a node datum"}));
    }
  } else {
    return Effect.fail(new StateQueueError({message: "No datum found"}));
  }
};
