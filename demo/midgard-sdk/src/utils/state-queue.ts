import { ConfirmedState, Header } from "../tx-builder/ledger-state.js";
import {
  Data,
  LucidEvolution,
  UTxO,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { NodeKey } from "../tx-builder/linked-list.js";
import { Effect } from "effect";
import { StateQueue } from "../tx-builder/index.js";
import { getNodeDatumFromUTxO } from "./linked-list.js";
import { MerkleRoot, POSIXTime } from "../tx-builder/common.js";
import { Datum } from "@/tx-builder/state-queue/types.js";

export type StateQueueUTxO = {
  utxo: UTxO;
  datum: StateQueue.Datum;
};

export const utxoToStateQueueUTxO = (
  utxo: UTxO,
): Effect.Effect<StateQueueUTxO, Error> => Effect.gen(function* () {
  const datum = yield* getNodeDatumFromUTxO(utxo);
  return { utxo, datum };
});

export const utxosToStateQueueUTxOs = (
  utxos: UTxO[],
): Effect.Effect<StateQueueUTxO[], Error> => {
  const effects = utxos.map(utxoToStateQueueUTxO);
  return Effect.allSuccesses(effects);
};

/**
 * Given a StateQueueUTxO, this function confirmes the node is root (i.e. no
 * keys in its datum), and attempts to coerce its underlying data into a
 * `ConfirmedState`.
 */
export const getConfirmedStateFromStateQueueUTxO = (
  utxo: StateQueueUTxO
): Effect.Effect<
  { utxo: StateQueueUTxO; data: ConfirmedState; link: NodeKey },
  Error
> => {
  try {
    const nodeDatum = utxo.datum;
    if (nodeDatum.key === "Empty") {
      const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
      return Effect.succeed({
        utxo,
        data: confirmedState,
        link: nodeDatum.next,
      });
    } else {
      return Effect.fail(new Error("Given UTxO is not root"));
    }
  } catch {
    return Effect.fail(new Error("Could not coerce to a node datum"));
  }
};

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlock - UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeader = (
  lucid: LucidEvolution,
  latestBlock: UTxO,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  endTime: POSIXTime
): Effect.Effect<{ nodeDatum: Datum; header: Header }, Error> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) => new Error(`Failed to find the wallet: ${e}`),
    });

    const pubKeyHash = paymentCredentialOf(walletAddress).hash;

    const stateQueueUTxO = yield* utxoToStateQueueUTxO(latestBlock);
    const { data: confirmedState } = yield* getConfirmedStateFromStateQueueUTxO(stateQueueUTxO);
    return {
      nodeDatum: {
        ...stateQueueUTxO.datum,
        next: { Key: { key: confirmedState.headerHash } },
      },
      header: {
        prevUtxosRoot: confirmedState.utxoRoot,
        utxosRoot: newUTxOsRoot,
        transactionsRoot,
        depositsRoot: "00".repeat(32),
        withdrawalsRoot: "00".repeat(32),
        startTime: confirmedState.endTime,
        endTime,
        prevHeaderHash: confirmedState.headerHash,
        operatorVkey: pubKeyHash,
        protocolVersion: confirmedState.protocolVersion,
      },
    };
  });

export const findLinkStateQueueUTxO = (
  link: NodeKey,
  utxos: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO, Error> => {
  if (link === "Empty") {
    return Effect.fail(new Error("Given link is \"Empty\""));
  } else {
    const foundLink = utxos.find(
      (u: StateQueueUTxO) => (u.datum.key !== "Empty" && u.datum.key.Key.key === link.Key.key));
    if (foundLink) {
      return Effect.succeed(foundLink);
    } else {
      return Effect.fail(new Error("Link not found"));
    }
  }
};

export const sortStateQueueUTxOs = (
  stateQueueUTxOs: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO[], Error> => Effect.gen(function* () {
  const filteredForConfirmedState = yield* Effect.allSuccesses(
    stateQueueUTxOs.map(getConfirmedStateFromStateQueueUTxO),
  );
  if (filteredForConfirmedState.length === 1) {
    const { utxo: confirmedStateUTxO, link: linkToOldestBlock } = filteredForConfirmedState[0];
    const sorted: StateQueueUTxO[] = [confirmedStateUTxO];
    let link = linkToOldestBlock;
    while (link !== "Empty") {
      const linkUTxO = yield* findLinkStateQueueUTxO(
        link,
        stateQueueUTxOs,
      );
      sorted.push(linkUTxO);
      link = linkUTxO.datum.next;
    }
    return sorted;
  } else {
    yield* Effect.fail(new Error("Confirmed state not found among state queue UTxOs."));
    return [];
  }
});
