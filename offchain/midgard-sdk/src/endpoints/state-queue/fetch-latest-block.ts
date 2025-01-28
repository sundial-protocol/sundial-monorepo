import { Effect, Option, Either } from "effect";
import {
  Address,
  Data,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import { utxosAtByNFTPolicyId } from "@/utils/helpers.js";
import { NodeKey } from "@/types/contracts/linked-list/index.js";
import { Datum } from "@/types/contracts/state-queue.js";
import { makeReturn } from "@/core.js";

export type Config = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
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

export const fetchLatestCommitedBlockProgram = (
  lucid: LucidEvolution,
  config: Config
): Effect.Effect<UTxO, string> =>
  Effect.gen(function* () {
    const allBlocks = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId
    );
    const filtered = allBlocks.filter((u: UTxO) => {
      const eithNodeKey = getLinkFromBlockUTxO(u);
      return Either.isRight(eithNodeKey) && eithNodeKey.right === "Empty";
    });
    if (filtered.length === 1) {
      return filtered[0];
    } else {
      return yield* Effect.fail("Latest block not found");
    }
  });

/**
 * Attempts fetching the commited block at the very end of the state queue
 * linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which links to no other nodes.
 */
export const fetchLatestCommitedBlock = (
  lucid: LucidEvolution,
  config: Config
) => makeReturn(fetchLatestCommitedBlockProgram(lucid, config)).unsafeRun();
