import { Data, UTxO } from "@lucid-evolution/lucid";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Effect } from "effect";
import { getNodeDatumFromUTxO } from "./linked-list.js";
import { Header } from "@/types/contracts/ledger-state.js";

export const getLinkFromBlockUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<NodeKey, Error> => {
  const nodeDatum = getNodeDatumFromUTxO(blockUTxO);
  return Effect.map(nodeDatum, (nd) => nd.next);
};

export const getHeaderFromBlockUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<Header, Error> =>
  Effect.gen(function* () {
    const nodeDatum = yield* getNodeDatumFromUTxO(blockUTxO);
    const headerProgram: Effect.Effect<Header, Error> = Effect.try({
      try: () => Data.castFrom(nodeDatum.data, Header),
      catch: (e) => new Error(`Failed coercing latest block's datum: ${e}`),
    });
    const header: Header = yield* headerProgram;
    return header;
  });
