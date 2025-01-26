import { Effect, Option, Either } from "effect";
import {
  Address,
  Data,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import { utxosAtByNFTPolicyId } from "@/utils/helpers.js";
import { Datum } from "@/types/contracts/state-queue.js";
import { ConfirmedState } from "@/types/contracts/ledger-state.js";
import { makeReturn } from "@/core.js";

export type Config = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

const getConfirmedStateFromBlockUTxO = (
  blockUTxO: UTxO
): Either.Either<ConfirmedState, string> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
      if (nodeDatum.key === "Empty") {
        const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
        return Either.right(confirmedState);
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

export const fetchConfirmedStateProgram = (
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
      const eithConfirmedState = getConfirmedStateFromBlockUTxO(u);
      if (Either.isRight(eithConfirmedState)) {
        return Option.some(eithConfirmedState.right);
      } else {
        return Option.none();
      }
    });
    if (filtered.length === 1) {
      return filtered[0];
    } else {
      return yield* Effect.fail("Confirmed state not found");
    }
  });

export const fetchConfirmedState = (lucid: LucidEvolution, config: Config) =>
  makeReturn(fetchConfirmedStateProgram(lucid, config)).unsafeRun();
