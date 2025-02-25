import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Header } from "../tx-builder/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";
import { getNodeDatumFromUTxO } from "./linked-list.js";

export const getHeaderFromBlockUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<Header, Error> =>
  Effect.gen(function* () {
    const nodeDatum = yield* getNodeDatumFromUTxO(blockUTxO);
    const header = yield* Effect.try({
      try: () => Data.castFrom(nodeDatum.data, Header),
      catch: (e) => new Error(`Failed coercing latest block's datum: ${e}`),
    });
    return header;
  });

export const hashHeader = (header: Header): Effect.Effect<string, Error> =>
  hashHexWithBlake2b224(Data.to(header, Header));
