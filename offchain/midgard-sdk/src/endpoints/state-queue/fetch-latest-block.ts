import { Effect, Option, Either } from "effect";
import {
  Address,
  Data,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import { getSingleAssetApartFromAda } from "@/utils/helpers.js";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Datum } from "@/types/contracts/state-queue.js";

export type Config = {
  stateQueueAddress: Address;
  stateQueuePolicyID: PolicyId;
};

const getLinkFromBlockUTxO = (
  blockUTxO: UTxO
): Either.Either<NodeKey, string> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
      return Either.right(nodeDatum.next);
    } catch {
      return Either.left("Could not coerce to a node datum");
    }
  } else {
    return Either.left("No datum found");
  }
};

export const fetchLatestBlock = (
  lucid: LucidEvolution,
  config: Config
): Effect.Effect<UTxO, string> =>
  Effect.gen(function* () {
    const allBlocks = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.stateQueueAddress),
      catch: (_) => "Failed to fetch state queue UTxOs",
    });
    const blockNFTsEithers: Either.Either<[UTxO, NodeKey], string>[] =
      allBlocks.map((u: UTxO) => {
        const nfts = getSingleAssetApartFromAda(u.assets);
        return Either.andThen(
          nfts,
          ([sym, _tn, qty]): Either.Either<[UTxO, NodeKey], string> => {
            if (sym === config.stateQueuePolicyID && qty === 1n) {
              const nodeKey = getLinkFromBlockUTxO(u);
              return Either.map(nodeKey, (nk): [UTxO, NodeKey] => [u, nk]);
            } else {
              return Either.left("Unauthentic block");
            }
          }
        );
      });
    const authenticBlockUTxOs = yield* Effect.filterMap(
      blockNFTsEithers,
      Option.some
    );
    const filtered = authenticBlockUTxOs.filter(([_u, nk]) => nk === "Empty");
    if (filtered.length === 1) {
      return filtered[0][0];
    } else {
      return yield* Effect.fail("Latest block not found");
    }
  });
