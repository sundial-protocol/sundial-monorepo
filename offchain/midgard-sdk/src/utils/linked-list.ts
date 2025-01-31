import { Either } from "effect";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { Data, UTxO } from "@lucid-evolution/lucid";

export const getNodeDatumFromUTxO = (
  nodeUTxO: UTxO
): Either.Either<NodeDatum, string> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, NodeDatum);
      return Either.right(nodeDatum);
    } catch {
      return Either.left("Could not coerce to a node datum");
    }
  } else {
    return Either.left("No datum found");
  }
};
