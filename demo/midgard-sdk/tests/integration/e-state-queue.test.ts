import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import {
  StateQueueDatum,
  fetchLatestCommittedBlockProgram,
  getConfirmedStateFromStateQueueDatum,
  incompleteCommitBlockHeaderTxProgram,
  incompleteInitStateQueueTxProgram,
  incompleteStateQueueMergeTxProgram,
  sortStateQueueUTxOs,
  updateLatestBlocksDatumAndGetTheNewHeaderProgram,
  utxoToStateQueueUTxO,
} from "@sdk/state-queue.ts";
import { ConfirmedState, Header } from "@sdk/ledger-state.ts";
import { makeFakeLucid } from "./harness/fake-lucid.ts";
import {
  FIXTURE_ADDRESS_SCRIPT_A,
  FIXTURE_CONFIRMED_STATE,
  FIXTURE_HEADER,
  FIXTURE_MERKLE_ROOT_A,
  FIXTURE_MERKLE_ROOT_B,
  FIXTURE_NONCE_UTXO,
  FIXTURE_POLICY_ID_A,
  FIXTURE_VALIDATOR,
} from "./harness/fixtures.ts";

const makeStateQueueAssets = (assetName: string) => ({
  lovelace: 2_000_000n,
  [toUnit(FIXTURE_POLICY_ID_A, assetName)]: 1n,
});

const makeStateQueueUtxo = (args: {
  txHash: string;
  outputIndex: number;
  datum: any;
  assetName: string;
}) => ({
  txHash: args.txHash,
  outputIndex: args.outputIndex,
  address: FIXTURE_ADDRESS_SCRIPT_A,
  assets: makeStateQueueAssets(args.assetName),
  datum: Data.to(args.datum, StateQueueDatum),
});

const makeConfirmedStateData = () =>
  Data.castTo(FIXTURE_CONFIRMED_STATE, ConfirmedState);

const makeHeaderData = () => Data.castTo(FIXTURE_HEADER, Header);

describe("SDK state-queue integration", () => {
  it.effect("init state-queue tx emits root confirmed-state datum", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid({ walletUtxos: [FIXTURE_NONCE_UTXO] });
      const tx = yield* incompleteInitStateQueueTxProgram(lucid as any, {
        validator: FIXTURE_VALIDATOR as any,
        genesisTime: 123n,
      });

      const datumCbor = (tx as any).__calls.payToAddressWithData[0][1].value;
      const decoded = Data.from(datumCbor, StateQueueDatum);
      const confirmed = yield* getConfirmedStateFromStateQueueDatum(decoded);

      expect(decoded.key).toBe("Empty");
      expect(decoded.next).toBe("Empty");
      expect(confirmed.data.startTime).toBe(123n);
      expect(confirmed.data.headerHash).toHaveLength(56);
    }),
  );

  it.effect("state-queue UTxO conversion decodes root node", () =>
    Effect.gen(function* () {
      const root = makeStateQueueUtxo({
        txHash: "aa".repeat(32),
        outputIndex: 0,
        datum: {
          key: "Empty",
          next: "Empty",
          data: makeConfirmedStateData(),
        },
        assetName: "Node",
      });

      const converted = yield* utxoToStateQueueUTxO(root as any, FIXTURE_POLICY_ID_A);
      expect(converted.utxo.txHash).toBe("aa".repeat(32));
      expect(converted.datum.key).toBe("Empty");
      expect(converted.assetName).toBe("Node");
    }),
  );

  it.effect("sorting returns head-to-tail deterministic order", () =>
    Effect.gen(function* () {
      const headKey = "11".repeat(28);
      const tailKey = "22".repeat(28);
      const head = yield* utxoToStateQueueUTxO(
        makeStateQueueUtxo({
          txHash: "bb".repeat(32),
          outputIndex: 0,
          datum: {
            key: "Empty",
            next: { Key: { key: headKey } },
            data: makeConfirmedStateData(),
          },
          assetName: "Node",
        }) as any,
        FIXTURE_POLICY_ID_A,
      );
      const tail = yield* utxoToStateQueueUTxO(
        makeStateQueueUtxo({
          txHash: "cc".repeat(32),
          outputIndex: 1,
          datum: {
            key: { Key: { key: headKey } },
            next: { Key: { key: tailKey } },
            data: makeHeaderData(),
          },
          assetName: `Node${headKey}`,
        }) as any,
        FIXTURE_POLICY_ID_A,
      );
      const end = yield* utxoToStateQueueUTxO(
        makeStateQueueUtxo({
          txHash: "dd".repeat(32),
          outputIndex: 2,
          datum: {
            key: { Key: { key: tailKey } },
            next: "Empty",
            data: makeHeaderData(),
          },
          assetName: `Node${tailKey}`,
        }) as any,
        FIXTURE_POLICY_ID_A,
      );

      const sorted = yield* sortStateQueueUTxOs([tail, end, head]);

      expect(sorted).toHaveLength(3);
      expect(sorted[0].datum.key).toBe("Empty");
      expect(sorted[1].datum.key).toEqual({ Key: { key: headKey } });
      expect(sorted[2].datum.key).toEqual({ Key: { key: tailKey } });
    }),
  );

  it.effect("fetchLatestCommittedBlock selects the unique tail node", () =>
    Effect.gen(function* () {
      const nonTailKey = "33".repeat(28);
      const nonTail = makeStateQueueUtxo({
        txHash: "ee".repeat(32),
        outputIndex: 0,
        datum: {
          key: { Key: { key: nonTailKey } },
          next: { Key: { key: "44".repeat(28) } },
          data: makeHeaderData(),
        },
        assetName: `Node${nonTailKey}`,
      });
      const tail = makeStateQueueUtxo({
        txHash: "ff".repeat(32),
        outputIndex: 1,
        datum: {
          key: { Key: { key: "44".repeat(28) } },
          next: "Empty",
          data: makeHeaderData(),
        },
        assetName: `Node${"44".repeat(28)}`,
      });

      const { lucid } = makeFakeLucid({
        utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [nonTail, tail] },
      });

      const latest = yield* fetchLatestCommittedBlockProgram(lucid as any, {
        stateQueueAddress: FIXTURE_ADDRESS_SCRIPT_A,
        stateQueuePolicyId: FIXTURE_POLICY_ID_A,
      });

      expect(latest.utxo.txHash).toBe("ff".repeat(32));
      expect(latest.datum.next).toBe("Empty");
    }),
  );

  it.effect("commit-block-header tx emits updated and new queue nodes", () =>
    Effect.gen(function* () {
      const anchorDatum = {
        key: "Empty",
        next: "Empty",
        data: makeConfirmedStateData(),
      };
      const anchorUtxoRaw = makeStateQueueUtxo({
        txHash: "10".repeat(32),
        outputIndex: 0,
        datum: anchorDatum,
        assetName: "Node",
      });
      const anchorUtxo = yield* utxoToStateQueueUTxO(
        anchorUtxoRaw as any,
        FIXTURE_POLICY_ID_A,
      );

      const updatedAnchorDatum = {
        ...anchorDatum,
        next: { Key: { key: "55".repeat(28) } },
      };

      const { lucid } = makeFakeLucid();
      const tx = yield* incompleteCommitBlockHeaderTxProgram(
        lucid as any,
        {
          stateQueueAddress: FIXTURE_ADDRESS_SCRIPT_A,
          stateQueuePolicyId: FIXTURE_POLICY_ID_A,
        },
        {
          anchorUTxO: anchorUtxo,
          updatedAnchorDatum: updatedAnchorDatum as any,
          newHeader: FIXTURE_HEADER,
          stateQueueSpendingScript: FIXTURE_VALIDATOR.spendingScript,
          policyId: FIXTURE_POLICY_ID_A,
          stateQueueMintingScript: FIXTURE_VALIDATOR.mintingScript,
        },
      );

      const calls = (tx as any).__calls.payToContract;
      const newNodeDatum = Data.from(calls[0][1].value, StateQueueDatum);
      const updatedNodeDatum = Data.from(calls[1][1].value, StateQueueDatum);

      expect(calls).toHaveLength(2);
      expect(newNodeDatum.next).toBe("Empty");
      expect(updatedNodeDatum.next).toEqual({ Key: { key: "55".repeat(28) } });
    }),
  );

  it.effect("header update helper produces new header and link for root anchor", () =>
    Effect.gen(function* () {
      const { lucid } = makeFakeLucid();
      const latestRoot = {
        key: "Empty",
        next: "Empty",
        data: makeConfirmedStateData(),
      };

      const result = yield* updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid as any,
        latestRoot as any,
        FIXTURE_MERKLE_ROOT_A,
        FIXTURE_MERKLE_ROOT_B,
        FIXTURE_MERKLE_ROOT_A,
        FIXTURE_MERKLE_ROOT_B,
        2_000_000n,
      );

      expect(result.nodeDatum.next).not.toBe("Empty");
      expect(result.header.utxosRoot).toBe(FIXTURE_MERKLE_ROOT_A);
      expect(result.header.endTime).toBe(2_000_000n);
    }),
  );

  it.effect("merge-to-confirmed-state tx burns oldest block NFT and rewrites root", () =>
    Effect.gen(function* () {
      const confirmed = yield* utxoToStateQueueUTxO(
        makeStateQueueUtxo({
          txHash: "21".repeat(32),
          outputIndex: 0,
          datum: {
            key: "Empty",
            next: { Key: { key: "66".repeat(28) } },
            data: makeConfirmedStateData(),
          },
          assetName: "Node",
        }) as any,
        FIXTURE_POLICY_ID_A,
      );
      const firstBlock = yield* utxoToStateQueueUTxO(
        makeStateQueueUtxo({
          txHash: "22".repeat(32),
          outputIndex: 1,
          datum: {
            key: { Key: { key: "66".repeat(28) } },
            next: "Empty",
            data: makeHeaderData(),
          },
          assetName: `Node${"66".repeat(28)}`,
        }) as any,
        FIXTURE_POLICY_ID_A,
      );
      const { lucid } = makeFakeLucid();

      const tx = yield* incompleteStateQueueMergeTxProgram(
        lucid as any,
        {
          stateQueueAddress: FIXTURE_ADDRESS_SCRIPT_A,
          stateQueuePolicyId: FIXTURE_POLICY_ID_A,
        },
        {
          confirmedUTxO: confirmed,
          firstBlockUTxO: firstBlock,
          stateQueueSpendingScript: FIXTURE_VALIDATOR.spendingScript,
          stateQueueMintingScript: FIXTURE_VALIDATOR.mintingScript,
        },
      );

      const calls = (tx as any).__calls;
      expect(calls.collectFrom).toHaveLength(1);
      expect(calls.mintAssets).toHaveLength(1);
      expect(calls.payToContract).toHaveLength(1);
      expect(calls.mintAssets[0][0][toUnit(FIXTURE_POLICY_ID_A, firstBlock.assetName)]).toBe(-1n);
    }),
  );

  it.effect("non-root datum is rejected as confirmed-state node", () =>
    Effect.gen(function* () {
      const nonRoot = {
        key: { Key: { key: "77".repeat(28) } },
        next: "Empty",
        data: makeHeaderData(),
      };
      const result = yield* Effect.either(
        getConfirmedStateFromStateQueueDatum(nonRoot as any),
      );
      expect(Either.isLeft(result)).toBe(true);
    }),
  );
});
