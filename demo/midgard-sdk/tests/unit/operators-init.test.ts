import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { Data } from "@lucid-evolution/lucid";

import * as sdk from "../../src/index.ts";
import {
  ConfirmedState,
  Header,
  RegisteredOperatorMintRedeemer,
  getNodeDatumFromUTxO,
  incompleteInitLinkedListTxProgram,
} from "../../src/index.ts";
import {
  addressDataKeyA,
  addressKeyA,
  addressScriptA,
  depositDatumFixture,
  hexA,
  makeBuilderSpy,
  makeLucidMock,
  makeMidgardValidatorsFixture,
  makeUtxo,
  merkleRootA,
  nodeDatumFixture,
  policyIdA,
  posixT1,
  posixT2,
  posixT3,
  pubKeyHashA,
  scriptHashA,
  txHashA,
  txHashB,
  validNodeDatumCbor,
  validatorA,
  withdrawalBodyFixture,
  withdrawalInfoFixture,
} from "./helpers.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit operator and initialization programs", () => {
  it("Linked-list node datum decodes from UTxO", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validNodeDatumCbor,
    });
    const datum = await Effect.runPromise(getNodeDatumFromUTxO(utxo as any));
    expect(datum.key).toEqual(nodeDatumFixture.key);
    expect(datum.next).toEqual(nodeDatumFixture.next);
    expect(datum.data).toBe(hexA);
  });

  it("Linked-list init transaction mints root node", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const redeemer = Data.to("Init", RegisteredOperatorMintRedeemer);

    const tx = await Effect.runPromise(
      incompleteInitLinkedListTxProgram(lucid as any, {
        validator: validatorA as any,
        data: "00",
        redeemer,
      }),
    );

    expect(tx).toBe(builder);
    expect(builder.mintAssets).toHaveBeenCalledTimes(1);
    expect(builder.pay.ToAddressWithData).toHaveBeenCalledTimes(1);
    expect(builder.attach.Script).toHaveBeenCalledWith(
      validatorA.mintingScript,
    );
  });

  it("No-op SDK transaction programs return a new tx builder", () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const calls = [
      sdk.incompleteActiveOperatorDeinitTxProgram(lucid as any, {}),
      sdk.incompleteActiveOperatorActivateTxProgram(lucid as any, {}),
      sdk.incompleteActiveOperatorRetireTxProgram(lucid as any, {}),
      sdk.incompleteActiveOperatorListStateTransitionTxProgram(
        lucid as any,
        {},
      ),
      sdk.incompleteActiveOperatorRemoveSlashBondTxProgram(lucid as any, {}),
      sdk.incompleteActiveOperatorUpdateCommitmentTimeTxProgram(
        lucid as any,
        {},
      ),
      sdk.incompleteRegisteredOperatorDeinitTxProgram(lucid as any, {}),
      sdk.incompleteRegisteredOperatorRegisterTxProgram(lucid as any, {}),
      sdk.incompleteRegisteredOperatorDeregisterTxProgram(lucid as any, {}),
      sdk.incompleteRegisteredOperatorActivateTxProgram(lucid as any, {}),
      sdk.incompleteRegisteredOperatorRemoveDuplicateSlashBondTxProgram(
        lucid as any,
        {},
      ),
      sdk.incompleteRetiredOperatorDeinitTxProgram(lucid as any, {}),
      sdk.incompleteRetiredOperatorRetireTxProgram(lucid as any, {}),
      sdk.incompleteRetiredOperatorRemoveOperatorTxProgram(lucid as any, {}),
      sdk.incompleteRetiredOperatorRecoverSlashBondTxProgram(lucid as any, {}),
      sdk.incompleteSchedulerDeinitTxProgram(lucid as any, {}),
      sdk.incompleteSchedulerAdvanceTxProgram(lucid as any, {}),
      sdk.incompleteSchedulerRewindTxProgram(lucid as any, {}),
      sdk.incompleteFraudProofDeinitTxProgram(lucid as any, {}),
      sdk.incompleteFraudProofNewCategoryTxProgram(lucid as any, {}),
      sdk.incompleteFraudProofRemoveCategoryTxProgram(lucid as any, {}),
      sdk.incompleteDeinitStateQueueTxProgram(lucid as any, {}),
      sdk.incompleteRemoveBlockStateQueueTxProgram(lucid as any, {}),
    ];

    for (const tx of calls) {
      expect(tx).toBe(builder);
    }
    expect(lucid.newTx).toHaveBeenCalledTimes(calls.length);
  });

  it("Init programs for scheduler and fraud-proof catalogue use mint/build flow", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);

    const schedulerTx = sdk.incompleteSchedulerInitTxProgram(lucid as any, {
      validator: validatorA as any,
    });
    const catalogueTx = await Effect.runPromise(
      sdk.incompleteFraudProofCatalogueInitTxProgram(lucid as any, {
        validator: validatorA as any,
        mptRootHash: merkleRootA,
      }),
    );

    expect(schedulerTx).toBe(builder);
    expect(catalogueTx).toBe(builder);
    expect(builder.mintAssets).toHaveBeenCalledTimes(2);
    expect(builder.pay.ToAddress).toHaveBeenCalledTimes(1);
    expect(builder.pay.ToAddressWithData).toHaveBeenCalledTimes(1);
  });

  it("Operator fetch programs authenticate and return matching UTxOs", async () => {
    const lucid = makeLucidMock();
    const activeDatum = Data.to(
      {
        key: pubKeyHashA,
        link: null,
        bondUnlockTime: posixT2,
      },
      sdk.ActiveOperatorDatum,
    );
    lucid.utxosAt.mockResolvedValueOnce([
      makeUtxo({ txHash: txHashA, outputIndex: 0, datum: activeDatum }),
    ]);

    const active = await Effect.runPromise(
      sdk.fetchActiveOperatorUTxOs(
        {
          activeOperatorAddress: addressScriptA,
          operator: pubKeyHashA,
          activeOperatorPolicyId: policyIdA,
        },
        lucid as any,
      ),
    );
    expect(active).toHaveLength(1);
    expect(active[0].datum.key).toBe(pubKeyHashA);

    const retiredDatum = Data.to(
      {
        key: pubKeyHashA,
        link: scriptHashA,
        bondUnlockTime: posixT3,
      },
      sdk.RetiredOperatorDatum,
    );
    lucid.utxosAt.mockResolvedValueOnce([
      makeUtxo({ txHash: txHashB, outputIndex: 1, datum: retiredDatum }),
    ]);

    const retired = await Effect.runPromise(
      sdk.fetchRetiredOperatorUTxOs(
        {
          retiredOperatorAddress: addressScriptA,
          operator: pubKeyHashA,
          retiredOperatorPolicyId: policyIdA,
        },
        lucid as any,
      ),
    );
    expect(retired).toHaveLength(1);
    expect(retired[0].datum.link).toBe(scriptHashA);
  });

  it("Hub oracle and scheduler fetch programs return a single authentic UTxO", async () => {
    const lucid = makeLucidMock();
    const validators = makeMidgardValidatorsFixture();
    const hubDatum = await Effect.runPromise(
      sdk.makeHubOracleDatum(validators as any),
    );
    const hubDatumCbor = Data.to(hubDatum, sdk.HubOracleDatum);
    const schedulerDatumCbor = Data.to(
      { operator: pubKeyHashA, startTime: posixT1 },
      sdk.SchedulerDatum,
    );

    lucid.utxosAt.mockImplementation(async (address: string) => {
      if (address === addressScriptA) {
        return [
          makeUtxo({ txHash: txHashA, outputIndex: 0, datum: hubDatumCbor }),
        ];
      }
      return [
        makeUtxo({
          txHash: txHashB,
          outputIndex: 1,
          datum: schedulerDatumCbor,
        }),
      ];
    });

    const hub = await Effect.runPromise(
      sdk.fetchHubOracleUTxOProgram(lucid as any, {
        hubOracleAddress: addressScriptA,
        hubOraclePolicyId: policyIdA,
      }),
    );
    const scheduler = await Effect.runPromise(
      sdk.fetchSchedulerUTxOProgram(lucid as any, {
        schedulerAddress: "addr_test1qzzz",
        schedulerPolicyId: policyIdA,
      }),
    );

    expect(hub.assetName).toBeDefined();
    expect(scheduler.datum.operator).toBe(pubKeyHashA);
  });

  it("Initialization program composes per-module initialization tx fragments", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const validators = makeMidgardValidatorsFixture();
    const nonceUTxO = makeUtxo({ txHash: txHashA, outputIndex: 0 });
    lucid.wallet.mockReturnValue({
      getUtxos: vi.fn(async () => [nonceUTxO]),
      address: vi.fn(async () => addressKeyA),
    });

    const nowSpy = vi.spyOn(Date, "now").mockReturnValue(1_000_000);
    const initTx = await Effect.runPromise(
      sdk.incompleteInitializationTxProgram(lucid as any, {
        midgardValidators: validators as any,
        fraudProofCatalogueMerkleRoot: merkleRootA,
      }),
    );
    const signed = await Effect.runPromise(
      sdk.unsignedInitializationTxProgram(lucid as any, {
        midgardValidators: validators as any,
        fraudProofCatalogueMerkleRoot: merkleRootA,
      }),
    );

    expect(initTx).toBe(builder);
    expect(signed).toEqual({ signed: true });
    expect(builder.collectFrom).toHaveBeenCalled();
    expect(builder.validTo).toHaveBeenCalled();
    expect(builder.compose).toHaveBeenCalled();
    expect(nowSpy).toHaveBeenCalled();
  });

  it("User-event transaction programs and wrappers complete successfully", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const nonceUTxO = makeUtxo({ txHash: txHashA, outputIndex: 0 });
    const cardanoTx = { to_cbor_hex: () => hexA };
    const nowSpy = vi.spyOn(Date, "now").mockReturnValue(1_000_000);

    const depositTx = await Effect.runPromise(
      sdk.incompleteDepositTxProgram(lucid as any, {
        depositScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        depositAmount: 5_000_000n,
        depositInfo: depositDatumFixture.event.info,
      }),
    );
    const txOrderTx = await Effect.runPromise(
      sdk.incompleteTxOrderTxProgram(lucid as any, {
        txOrderScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        cardanoTx: cardanoTx as any,
        refundAddress: addressDataKeyA as any,
      }),
    );
    const withdrawalTx = await Effect.runPromise(
      sdk.incompleteWithdrawalTxProgram(lucid as any, {
        withdrawalScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        withdrawalBody: withdrawalBodyFixture as any,
        withdrawalSignature: withdrawalInfoFixture.signature as any,
        refundAddress: addressDataKeyA as any,
      }),
    );

    const unsignedDeposit = await Effect.runPromise(
      sdk.unsignedDepositTxProgram(lucid as any, {
        depositScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        depositAmount: 5_000_000n,
        depositInfo: depositDatumFixture.event.info,
      }),
    );
    const unsignedTxOrder = await Effect.runPromise(
      sdk.unsignedTxOrderTxProgram(lucid as any, {
        txOrderScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        cardanoTx: cardanoTx as any,
        refundAddress: addressDataKeyA as any,
      }),
    );
    const unsignedWithdrawal = await Effect.runPromise(
      sdk.unsignedWithdrawalTxProgram(lucid as any, {
        withdrawalScriptAddress: addressScriptA,
        mintingPolicy: validatorA.mintingScript as any,
        policyId: policyIdA,
        nonceUTxO: nonceUTxO as any,
        withdrawalBody: withdrawalBodyFixture as any,
        withdrawalSignature: withdrawalInfoFixture.signature as any,
        refundAddress: addressDataKeyA as any,
      }),
    );

    expect(depositTx).toBe(builder);
    expect(txOrderTx).toBe(builder);
    expect(withdrawalTx).toBe(builder);
    expect(unsignedDeposit).toEqual({ signed: true });
    expect(unsignedTxOrder).toEqual({ signed: true });
    expect(unsignedWithdrawal).toEqual({ signed: true });
    expect(nowSpy).toHaveBeenCalled();
  });
});
