import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { Data, toUnit } from "@lucid-evolution/lucid";

import * as sdk from "../../src/index.ts";
import { ConfirmedState, Header } from "../../src/index.ts";
import {
  addressScriptA,
  captureLogs,
  confirmedStateFixture,
  headerFixture,
  type BuilderSpy,
  makeBlockStateQueueNode,
  makeBuilderSpy,
  makeConfirmedStateQueueNode,
  makeLucidMock,
  makeStateQueueUtxo,
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

const stateQueueConfig = {
  stateQueueAddress: addressScriptA,
  stateQueuePolicyId: policyIdA,
};

const blockKey = "ab".repeat(28);
const tailKey = "cd".repeat(28);
const tailTxHash = "33".repeat(32);

const makeThreeNodeQueue = () => {
  const confirmedNode = makeConfirmedStateQueueNode({
    next: { Key: { key: blockKey } },
  });
  const blockNode = makeBlockStateQueueNode({
    key: blockKey,
    next: { Key: { key: tailKey } },
  });
  const tailNode = makeBlockStateQueueNode({
    key: tailKey,
    header: { ...headerFixture, endTime: posixT3 },
  });
  const confirmed = makeStateQueueUtxo({
    txHash: txHashA,
    outputIndex: 0,
    datum: confirmedNode,
  });
  const block = makeStateQueueUtxo({
    txHash: txHashB,
    outputIndex: 1,
    datum: blockNode,
  });
  const tail = makeStateQueueUtxo({
    txHash: tailTxHash,
    outputIndex: 2,
    datum: tailNode,
  });

  return { confirmedNode, blockNode, tailNode, confirmed, block, tail };
};

const makeCommitHeaderScenario = async () => {
  const builder = makeBuilderSpy();
  const lucid = makeLucidMock(builder);
  const { confirmed } = makeThreeNodeQueue();
  const updated = await Effect.runPromise(
    sdk.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      confirmed.datum,
      merkleRootB,
      merkleRootA,
      merkleRootB,
      merkleRootA,
      posixT2,
    ),
  );
  const newHeaderHash = await Effect.runPromise(
    sdk.hashBlockHeader(updated.header),
  );

  return { builder, lucid, confirmed, updated, newHeaderHash };
};

const recordCommitBuilderSemantics = (builder: BuilderSpy) => ({
  consumedUtxos: builder.collectFrom.mock.calls.map(([utxos]) => utxos),
  producedContracts: builder.pay.ToContract.mock.calls.map(
    ([address, datum, assets]) => ({ address, datum, assets }),
  ),
  minted: builder.mintAssets.mock.calls.map(([assets, redeemer]) => ({
    assets,
    redeemer,
  })),
  attachedScripts: builder.attach.Script.mock.calls.map(([script]) => script),
});

describe("SDK unit state queue programs", () => {
  it("fetchConfirmedStateAndItsLinkByUnitProgram resolves confirmed and link UTxOs via unit lookups", async () => {
    const lucid = makeLucidMock();
    const { confirmed, block } = makeThreeNodeQueue();
    const rootUnit = toUnit(policyIdA, sdk.NODE_ASSET_NAME);
    const firstBlockUnit = toUnit(
      policyIdA,
      `${sdk.NODE_ASSET_NAME}${blockKey}`,
    );
    const lookupByAddressAndUnit = new Map([
      [`${addressScriptA}:${rootUnit}`, [confirmed.utxo]],
      [`${addressScriptA}:${firstBlockUnit}`, [block.utxo]],
    ]);

    lucid.utxosAt.mockImplementation(async () => {
      throw new Error("unexpected broad state-queue scan");
    });
    lucid.utxosAtWithUnit.mockImplementation(
      async (address: string, unit: string) => {
        const key = `${address}:${unit}`;
        const result = lookupByAddressAndUnit.get(key);
        if (!result) {
          throw new Error(`Unexpected utxosAtWithUnit lookup: ${key}`);
        }
        return result;
      },
    );

    const result = await Effect.runPromise(
      sdk.fetchConfirmedStateAndItsLinkByUnitProgram(lucid, stateQueueConfig),
    );

    expect(lucid.utxosAtWithUnit).toHaveBeenNthCalledWith(
      1,
      addressScriptA,
      rootUnit,
    );
    expect(lucid.utxosAtWithUnit).toHaveBeenNthCalledWith(
      2,
      addressScriptA,
      firstBlockUnit,
    );
    expect(lucid.utxosAt).not.toHaveBeenCalled();
    expect(result.confirmed.utxo.txHash).toBe(txHashA);
    expect(result.link?.utxo.txHash).toBe(txHashB);
  });

  it("sortStateQueueUTxOs orders confirmed, block, and tail nodes", async () => {
    const { confirmed, block, tail } = makeThreeNodeQueue();

    const sorted = await Effect.runPromise(
      sdk.sortStateQueueUTxOs([tail, block, confirmed]),
    );

    expect(sorted.map(({ utxo }) => utxo.txHash)).toEqual([
      txHashA,
      txHashB,
      tailTxHash,
    ]);
    expect(
      await Effect.runPromise(sdk.headerHashFromStateQueueUTxO(confirmed)),
    ).toBe(confirmedStateFixture.headerHash);
    expect(
      await Effect.runPromise(sdk.headerHashFromStateQueueUTxO(block)),
    ).toBe(blockKey);
  });

  it("utxosToStateQueueUTxOs logs dropped UTxOs instead of silently discarding them", async () => {
    const { confirmed } = makeThreeNodeQueue();
    const invalidUtxo = makeUtxo({
      txHash: tailTxHash,
      outputIndex: 2,
      unit: toUnit(policyIdA, "deadbeef"),
    });

    const { entries, layer } = captureLogs();

    const result = await Effect.runPromise(
      sdk
        .utxosToStateQueueUTxOs([confirmed.utxo, invalidUtxo], policyIdA)
        .pipe(Effect.provide(layer)),
    );

    expect(result.map(({ utxo }) => utxo.txHash)).toEqual([txHashA]);

    const warning = entries.find(
      (entry) =>
        typeof entry[0] === "string" &&
        entry[0].includes("dropped 1 invalid UTxO"),
    );
    expect(warning).toBeDefined();
    const details = warning?.[1] as Array<{ utxo: string; tag: string }>;
    expect(details).toHaveLength(1);
    expect(details[0].utxo).toBe(`${tailTxHash}#2`);
  });

  it("fetchSortedStateQueueUTxOsProgram parses and sorts fetched queue UTxOs", async () => {
    const lucid = makeLucidMock();
    const { confirmed, block, tail } = makeThreeNodeQueue();
    lucid.utxosAt.mockResolvedValue([tail.utxo, block.utxo, confirmed.utxo]);

    const fetchedSorted = await Effect.runPromise(
      sdk.fetchSortedStateQueueUTxOsProgram(lucid, stateQueueConfig),
    );

    expect(lucid.utxosAt).toHaveBeenCalledWith(addressScriptA);
    expect(fetchedSorted.map(({ utxo }) => utxo.txHash)).toEqual([
      txHashA,
      txHashB,
      tailTxHash,
    ]);
  });

  it("fetchConfirmedStateAndItsLinkProgram returns the root node and its first block", async () => {
    const lucid = makeLucidMock();
    const { confirmed, block, tail } = makeThreeNodeQueue();
    lucid.utxosAt.mockResolvedValue([tail.utxo, block.utxo, confirmed.utxo]);

    const result = await Effect.runPromise(
      sdk.fetchConfirmedStateAndItsLinkProgram(lucid, stateQueueConfig),
    );

    expect(result.confirmed.utxo.txHash).toBe(txHashA);
    expect(result.link?.utxo.txHash).toBe(txHashB);
  });

  it("fetchLatestCommittedBlockProgram returns the tail node", async () => {
    const lucid = makeLucidMock();
    const { confirmed, block, tail } = makeThreeNodeQueue();
    lucid.utxosAt.mockResolvedValue([block.utxo, tail.utxo, confirmed.utxo]);

    const latest = await Effect.runPromise(
      sdk.fetchLatestCommittedBlockProgram(lucid, stateQueueConfig),
    );

    expect(latest.utxo.txHash).toBe(tailTxHash);
    expect(latest.datum.next).toBe("Empty");
  });

  it("updateLatestBlocksDatumAndGetTheNewHeaderProgram links the previous tail to the new header", async () => {
    const lucid = makeLucidMock();
    const latestDatum = makeConfirmedStateQueueNode();

    const updated = await Effect.runPromise(
      sdk.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid,
        latestDatum,
        merkleRootB,
        merkleRootA,
        merkleRootB,
        merkleRootA,
        posixT2,
      ),
    );
    const newHeaderHash = await Effect.runPromise(
      sdk.hashBlockHeader(updated.header),
    );

    expect(updated.header).toMatchObject({
      prevUtxosRoot: confirmedStateFixture.utxoRoot,
      utxosRoot: merkleRootB,
      transactionsRoot: merkleRootA,
      depositsRoot: merkleRootB,
      withdrawalsRoot: merkleRootA,
      startTime: confirmedStateFixture.endTime,
      endTime: posixT2,
      prevHeaderHash: confirmedStateFixture.headerHash,
      protocolVersion: confirmedStateFixture.protocolVersion,
    });
    expect(updated.nodeDatum.next).toEqual({ Key: { key: newHeaderHash } });
  });

  it("incompleteCommitBlockHeaderTxProgram collects the anchor, mints the new node NFT, and pays updated datums", async () => {
    const { builder, lucid, confirmed, updated, newHeaderHash } =
      await makeCommitHeaderScenario();
    const mintedAssets = {
      [toUnit(policyIdA, `${sdk.NODE_ASSET_NAME}${newHeaderHash}`)]: 1n,
    };
    const newNodeDatum = {
      key: updated.nodeDatum.next,
      next: "Empty",
      data: Data.castTo(updated.header, Header),
    };

    await Effect.runPromise(
      sdk.incompleteCommitBlockHeaderTxProgram(lucid, stateQueueConfig, {
        anchorUTxO: confirmed,
        updatedAnchorDatum: updated.nodeDatum,
        newHeader: updated.header,
        stateQueueSpendingScript: validatorA.spendingScript,
        policyId: policyIdA,
        stateQueueMintingScript: validatorA.mintingScript,
      }),
    );
    const calls = recordCommitBuilderSemantics(builder);

    expect(lucid.newTx).toHaveBeenCalledOnce();
    expect(calls.consumedUtxos).toContainEqual([confirmed.utxo]);
    expect(calls.producedContracts).toContainEqual({
      address: addressScriptA,
      datum: {
        kind: "inline",
        value: Data.to(newNodeDatum, sdk.StateQueueDatum),
      },
      assets: mintedAssets,
    });
    expect(calls.producedContracts).toContainEqual({
      address: addressScriptA,
      datum: {
        kind: "inline",
        value: Data.to(updated.nodeDatum, sdk.StateQueueDatum),
      },
      assets: confirmed.utxo.assets,
    });
    expect(calls.minted).toContainEqual({
      assets: mintedAssets,
      redeemer: Data.void(),
    });
    expect(calls.attachedScripts).toEqual(
      expect.arrayContaining([
        validatorA.spendingScript,
        validatorA.mintingScript,
      ]),
    );
  });

  it("unsignedCommitBlockHeaderTxProgram completes the commit transaction without local UPLC evaluation", async () => {
    const { builder, lucid, confirmed, updated } =
      await makeCommitHeaderScenario();

    const unsignedCommit = await Effect.runPromise(
      sdk.unsignedCommitBlockHeaderTxProgram(
        lucid,
        stateQueueConfig,
        {
          anchorUTxO: confirmed,
          updatedAnchorDatum: updated.nodeDatum,
          newHeader: updated.header,
          stateQueueSpendingScript: validatorA.spendingScript,
          policyId: policyIdA,
          stateQueueMintingScript: validatorA.mintingScript,
        },
        {},
      ),
    );

    expect(builder.complete).toHaveBeenCalledWith({ localUPLCEval: false });
    expect(unsignedCommit).toBe(builder.completedTx);
  });

  it("incompleteInitStateQueueTxProgram mints the root state queue node", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const rootAssets = {
      [toUnit(policyIdA, sdk.NODE_ASSET_NAME)]: 1n,
    };
    const rootDatum = {
      key: "Empty",
      next: "Empty",
      data: Data.castTo(
        {
          headerHash: sdk.GENESIS_HEADER_HASH,
          prevHeaderHash: sdk.GENESIS_HEADER_HASH,
          utxoRoot: sdk.GENESIS_UTXO_ROOT,
          startTime: posixT1,
          endTime: posixT1,
          protocolVersion: sdk.GENESIS_PROTOCOL_VERSION,
        },
        ConfirmedState,
      ),
    };

    await Effect.runPromise(
      sdk.incompleteInitStateQueueTxProgram(lucid, {
        validator: validatorA,
        genesisTime: posixT1,
      }),
    );

    expect(builder.mintAssets).toHaveBeenCalledWith(
      rootAssets,
      Data.to("Init", sdk.StateQueueRedeemer),
    );
    expect(builder.pay.ToAddressWithData).toHaveBeenCalledWith(
      validatorA.spendingScriptAddress,
      { kind: "inline", value: Data.to(rootDatum, sdk.NodeDatum) },
      rootAssets,
    );
    expect(builder.attach.Script).toHaveBeenCalledWith(
      validatorA.mintingScript,
    );
  });

  it("unsignedInitStateQueueTxProgram completes the initialization transaction", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);

    const unsignedInit = await Effect.runPromise(
      sdk.unsignedInitStateQueueTxProgram(lucid, {
        validator: validatorA,
        genesisTime: posixT1,
      }),
    );

    expect(builder.complete).toHaveBeenCalledWith({ localUPLCEval: false });
    expect(unsignedInit).toBe(builder.completedTx);
  });

  it("incompleteStateQueueMergeTxProgram burns the first block NFT and updates the confirmed node", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const { confirmed, block } = makeThreeNodeQueue();
    const blockHeaderHash = await Effect.runPromise(
      sdk.hashBlockHeader(headerFixture),
    );
    const newConfirmedNodeDatum = {
      ...confirmed.datum,
      data: Data.castTo(
        {
          ...confirmedStateFixture,
          headerHash: blockHeaderHash,
          prevHeaderHash: confirmedStateFixture.headerHash,
          utxoRoot: headerFixture.utxosRoot,
          startTime: confirmedStateFixture.endTime,
          endTime: headerFixture.endTime,
        },
        ConfirmedState,
      ),
      next: block.datum.next,
    };
    const assetsToBurn = {
      [toUnit(policyIdA, block.assetName)]: -1n,
    };

    await Effect.runPromise(
      sdk.incompleteStateQueueMergeTxProgram(lucid, stateQueueConfig, {
        confirmedUTxO: confirmed,
        firstBlockUTxO: block,
        stateQueueSpendingScript: validatorA.spendingScript,
        stateQueueMintingScript: validatorA.mintingScript,
      }),
    );

    expect(builder.collectFrom).toHaveBeenCalledWith(
      [confirmed.utxo, block.utxo],
      Data.to("MergeToConfirmedState", sdk.StateQueueRedeemer),
    );
    expect(builder.pay.ToContract).toHaveBeenCalledWith(
      addressScriptA,
      { kind: "inline", value: Data.to(newConfirmedNodeDatum, sdk.NodeDatum) },
      confirmed.utxo.assets,
    );
    expect(builder.mintAssets).toHaveBeenCalledWith(assetsToBurn, Data.void());
    expect(builder.attach.Script).toHaveBeenNthCalledWith(
      1,
      validatorA.spendingScript,
    );
    expect(builder.attach.Script).toHaveBeenNthCalledWith(
      2,
      validatorA.mintingScript,
    );
  });

  it("mergeToConfirmedStateProgram completes the merge transaction through completeProgram", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const { confirmed, block } = makeThreeNodeQueue();

    const mergedComplete = await Effect.runPromise(
      sdk.mergeToConfirmedStateProgram(lucid, stateQueueConfig, {
        confirmedUTxO: confirmed,
        firstBlockUTxO: block,
        stateQueueSpendingScript: validatorA.spendingScript,
        stateQueueMintingScript: validatorA.mintingScript,
      }),
    );

    expect(builder.completeProgram).toHaveBeenCalledOnce();
    expect(mergedComplete).toBe(builder.completedTx);
  });
});
