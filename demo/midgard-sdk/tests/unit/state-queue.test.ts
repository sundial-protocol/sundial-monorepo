import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { Data, toUnit } from "@lucid-evolution/lucid";

import * as sdk from "../../src/index.ts";
import { ConfirmedState, Header } from "../../src/index.ts";
import {
  addressScriptA,
  confirmedStateFixture,
  headerFixture,
  makeBuilderSpy,
  makeLucidMock,
  makeUtxo,
  merkleRootA,
  merkleRootB,
  policyIdA,
  posixT1,
  posixT2,
  posixT3,
  txHashA,
  txHashB,
  validatorA,
} from "./helpers.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit state queue programs", () => {
  it("State queue helpers and transaction programs work on happy paths", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const stateQueueKey = "ab".repeat(28);
    const stateQueueTailKey = "cd".repeat(28);
    const confirmedNodeDatum = {
      key: "Empty",
      next: { Key: { key: stateQueueKey } },
      data: Data.castTo(confirmedStateFixture, ConfirmedState),
    };
    const blockNodeDatum = {
      key: { Key: { key: stateQueueKey } },
      next: { Key: { key: stateQueueTailKey } },
      data: Data.castTo(headerFixture, Header),
    };
    const tailNodeDatum = {
      key: { Key: { key: stateQueueTailKey } },
      next: "Empty",
      data: Data.castTo({ ...headerFixture, endTime: posixT3 }, Header),
    };
    const nodeAssetName = `${sdk.NODE_ASSET_NAME}${stateQueueKey}`;
    const tailAssetName = `${sdk.NODE_ASSET_NAME}${stateQueueTailKey}`;
    const confirmedUtxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: Data.to(confirmedNodeDatum, sdk.StateQueueDatum),
      unit: toUnit(policyIdA, `${sdk.NODE_ASSET_NAME}${"00".repeat(28)}`),
    });
    const blockUtxo = makeUtxo({
      txHash: txHashB,
      outputIndex: 1,
      datum: Data.to(blockNodeDatum, sdk.StateQueueDatum),
      unit: toUnit(policyIdA, nodeAssetName),
    });
    const tailUtxo = makeUtxo({
      txHash: "33".repeat(32),
      outputIndex: 2,
      datum: Data.to(tailNodeDatum, sdk.StateQueueDatum),
      unit: toUnit(policyIdA, tailAssetName),
    });

    const confirmedStateQueue = await Effect.runPromise(
      sdk.utxoToStateQueueUTxO(confirmedUtxo as any, policyIdA),
    );
    const blockStateQueue = await Effect.runPromise(
      sdk.utxoToStateQueueUTxO(blockUtxo as any, policyIdA),
    );
    const tailStateQueue = await Effect.runPromise(
      sdk.utxoToStateQueueUTxO(tailUtxo as any, policyIdA),
    );

    const sorted = await Effect.runPromise(
      sdk.sortStateQueueUTxOs([
        tailStateQueue as any,
        blockStateQueue as any,
        confirmedStateQueue as any,
      ]),
    );
    expect(sorted).toHaveLength(3);
    expect(
      await Effect.runPromise(
        sdk.headerHashFromStateQueueUTxO(confirmedStateQueue as any),
      ),
    ).toBe(confirmedStateFixture.headerHash);
    expect(
      await Effect.runPromise(
        sdk.headerHashFromStateQueueUTxO(blockStateQueue as any),
      ),
    ).toBe(stateQueueKey);

    lucid.utxosAt.mockResolvedValue([confirmedUtxo, blockUtxo, tailUtxo]);
    const fetchConfig = {
      stateQueueAddress: addressScriptA,
      stateQueuePolicyId: policyIdA,
    };
    const fetchedSorted = await Effect.runPromise(
      sdk.fetchSortedStateQueueUTxOsProgram(lucid as any, fetchConfig),
    );
    const confirmedAndLink = await Effect.runPromise(
      sdk.fetchConfirmedStateAndItsLinkProgram(lucid as any, fetchConfig),
    );
    const latest = await Effect.runPromise(
      sdk.fetchLatestCommittedBlockProgram(lucid as any, fetchConfig),
    );

    expect(fetchedSorted).toHaveLength(3);
    expect(confirmedAndLink.confirmed.assetName).toContain(sdk.NODE_ASSET_NAME);
    expect(latest.datum.next).toBe("Empty");

    const nowSpy = vi.spyOn(Date, "now").mockReturnValue(1_000_000);
    const updated = await Effect.runPromise(
      sdk.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid as any,
        confirmedNodeDatum as any,
        merkleRootB,
        merkleRootA,
        merkleRootB,
        merkleRootA,
        posixT2,
      ),
    );
    expect(updated.nodeDatum.next).not.toBe("Empty");
    expect(nowSpy).toHaveBeenCalled();

    const commitTx = await Effect.runPromise(
      sdk.incompleteCommitBlockHeaderTxProgram(lucid as any, fetchConfig, {
        anchorUTxO: confirmedStateQueue as any,
        updatedAnchorDatum: updated.nodeDatum as any,
        newHeader: updated.header as any,
        stateQueueSpendingScript: validatorA.spendingScript as any,
        policyId: policyIdA,
        stateQueueMintingScript: validatorA.mintingScript as any,
      }),
    );
    const unsignedCommit = await Effect.runPromise(
      sdk.unsignedCommitBlockHeaderTxProgram(
        lucid as any,
        fetchConfig,
        {
          anchorUTxO: confirmedStateQueue as any,
          updatedAnchorDatum: updated.nodeDatum as any,
          newHeader: updated.header as any,
          stateQueueSpendingScript: validatorA.spendingScript as any,
          policyId: policyIdA,
          stateQueueMintingScript: validatorA.mintingScript as any,
        },
        {} as any,
      ),
    );

    expect(commitTx).toBe(builder);
    expect(unsignedCommit).toEqual({ signed: true });
  });

  it("State queue initialization and merge wrappers produce completed transactions", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const confirmedNodeDatum = {
      key: "Empty",
      next: { Key: { key: "ab".repeat(28) } },
      data: Data.castTo(confirmedStateFixture, ConfirmedState),
    };
    const blockNodeDatum = {
      key: { Key: { key: "ab".repeat(28) } },
      next: "Empty",
      data: Data.castTo(headerFixture, Header),
    };
    const confirmedUtxo = {
      utxo: makeUtxo({
        txHash: txHashA,
        outputIndex: 0,
        datum: Data.to(confirmedNodeDatum, sdk.StateQueueDatum),
        unit: toUnit(policyIdA, `${sdk.NODE_ASSET_NAME}${"00".repeat(28)}`),
      }),
      datum: confirmedNodeDatum,
      assetName: `${sdk.NODE_ASSET_NAME}${"00".repeat(28)}`,
    };
    const firstBlockUtxo = {
      utxo: makeUtxo({
        txHash: txHashB,
        outputIndex: 1,
        datum: Data.to(blockNodeDatum, sdk.StateQueueDatum),
        unit: toUnit(policyIdA, `${sdk.NODE_ASSET_NAME}${"ab".repeat(28)}`),
      }),
      datum: blockNodeDatum,
      assetName: `${sdk.NODE_ASSET_NAME}${"ab".repeat(28)}`,
    };

    const initTx = await Effect.runPromise(
      sdk.incompleteInitStateQueueTxProgram(lucid as any, {
        validator: validatorA as any,
        genesisTime: posixT1,
      }),
    );
    const unsignedInit = await Effect.runPromise(
      sdk.unsignedInitStateQueueTxProgram(lucid as any, {
        validator: validatorA as any,
        genesisTime: posixT1,
      }),
    );
    const merged = await Effect.runPromise(
      sdk.incompleteStateQueueMergeTxProgram(
        lucid as any,
        {
          stateQueueAddress: addressScriptA,
          stateQueuePolicyId: policyIdA,
        },
        {
          confirmedUTxO: confirmedUtxo as any,
          firstBlockUTxO: firstBlockUtxo as any,
          stateQueueSpendingScript: validatorA.spendingScript as any,
          stateQueueMintingScript: validatorA.mintingScript as any,
        },
      ),
    );
    const mergedComplete = await Effect.runPromise(
      sdk.mergeToConfirmedStateProgram(
        lucid as any,
        {
          stateQueueAddress: addressScriptA,
          stateQueuePolicyId: policyIdA,
        },
        {
          confirmedUTxO: confirmedUtxo as any,
          firstBlockUTxO: firstBlockUtxo as any,
          stateQueueSpendingScript: validatorA.spendingScript as any,
          stateQueueMintingScript: validatorA.mintingScript as any,
        },
      ),
    );

    expect(initTx).toBe(builder);
    expect(unsignedInit).toEqual({ signed: true });
    expect(merged).toBe(builder);
    expect(mergedComplete).toEqual({ signed: true });
  });
});
