import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { ConfirmedState, Header } from "@/types/contracts/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Datum } from "@/types/contracts/state-queue.js";

/**
 * Given a block UTxO, this function confirmes the node is root (i.e. no keys
 * in its datum), and attempts to coerce its underlying data into a
 * `ConfirmedState`.
 */
export const getConfirmedStateFromUTxO = (
  blockUTxO: UTxO
): Effect.Effect<{ data: ConfirmedState; link: NodeKey }, Error> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
      if (nodeDatum.key === "Empty") {
        const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
        return Effect.succeed({ data: confirmedState, link: nodeDatum.next });
      } else {
        return Effect.fail(new Error("Given UTxO is not root"));
      }
    } catch {
      return Effect.fail(new Error("Could not coerce to a node datum"));
    }
  } else {
    return Effect.fail(new Error("No datum found"));
  }
};

export const hashHeader = (header: Header): Effect.Effect<string, Error> =>
  hashHexWithBlake2b224(Data.to(header, Header));
