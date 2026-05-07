import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import { toUnit } from "@lucid-evolution/lucid";

import {
  createSlashedOperatorMintRedeemerCBOR,
  fetchUserEventRefUTxO,
  getOperatorNFT,
  incompleteResolveSettlementProgram,
  unsignedResolveSettlementTxProgram,
} from "../../src/settlement.ts";
import {
  addressScriptA,
  addressKeyA,
  assetNameA,
  makeBuilderSpy,
  makeLucidMock,
  makeUtxo,
  merkleRootA,
  merkleRootB,
  policyIdA,
  posixT1,
  posixT2,
  pubKeyHashA,
  scriptHashA,
  txHashA,
  txHashB,
  validDepositDatumCbor,
  validTxOrderDatumCbor,
  validWithdrawalDatumCbor,
  validatorA,
} from "./helpers.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit settlement programs", () => {
  it("Settlement helpers and resolve transaction programs succeed on happy flow", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const settlementUTxO = {
      utxo: makeUtxo({ txHash: txHashA, outputIndex: 0 }),
      datum: {
        depositsRoot: merkleRootA,
        withdrawalsRoot: merkleRootB,
        transactionsRoot: merkleRootA,
        resolutionClaim: {
          resolutionTime: BigInt(Date.now() + 10 * 60_000),
          operator: pubKeyHashA,
        },
      },
      assetName: assetNameA,
    };

    const resolved = await Effect.runPromise(
      incompleteResolveSettlementProgram(lucid as any, {
        settlementAddress: addressScriptA,
        resolutionClaimOperator: pubKeyHashA,
        settlementId: merkleRootA,
        changeAddress: addressKeyA,
        settlementPolicyId: policyIdA,
        settlementMintingPolicy: validatorA.mintingScript as any,
        settlementUTxO: settlementUTxO as any,
      }),
    );
    const unsignedResolved = await Effect.runPromise(
      unsignedResolveSettlementTxProgram(lucid as any, {
        settlementAddress: addressScriptA,
        resolutionClaimOperator: pubKeyHashA,
        settlementId: merkleRootA,
        changeAddress: addressKeyA,
        settlementPolicyId: policyIdA,
        settlementMintingPolicy: validatorA.mintingScript as any,
        settlementUTxO: settlementUTxO as any,
      }),
    );

    const activeRedeemer = await Effect.runPromise(
      createSlashedOperatorMintRedeemerCBOR(
        {
          utxo: makeUtxo({ txHash: txHashA, outputIndex: 0 }),
          datum: { key: pubKeyHashA, link: null, bondUnlockTime: posixT1 },
          assetName: assetNameA,
          isActive: true,
        } as any,
        pubKeyHashA,
      ),
    );
    const retiredRedeemer = await Effect.runPromise(
      createSlashedOperatorMintRedeemerCBOR(
        {
          utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
          datum: {
            key: pubKeyHashA,
            link: scriptHashA,
            bondUnlockTime: posixT2,
          },
          assetName: assetNameA,
          isActive: false,
        } as any,
        pubKeyHashA,
      ),
    );
    const activeNft = await Effect.runPromise(
      getOperatorNFT(
        {
          utxo: makeUtxo({ txHash: txHashA, outputIndex: 0 }),
          datum: { key: pubKeyHashA, link: null, bondUnlockTime: posixT1 },
          assetName: assetNameA,
          isActive: true,
        } as any,
        policyIdA,
        "bb".repeat(28),
      ),
    );
    const retiredNft = await Effect.runPromise(
      getOperatorNFT(
        {
          utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
          datum: {
            key: pubKeyHashA,
            link: scriptHashA,
            bondUnlockTime: posixT2,
          },
          assetName: assetNameA,
          isActive: false,
        } as any,
        policyIdA,
        "bb".repeat(28),
      ),
    );

    expect(resolved).toBe(builder);
    expect(unsignedResolved).toEqual({ signed: true });
    expect(activeRedeemer.length).toBeGreaterThan(2);
    expect(retiredRedeemer.length).toBeGreaterThan(2);
    expect(activeNft).toBe(toUnit(policyIdA, assetNameA));
    expect(retiredNft).toBe(toUnit("bb".repeat(28), assetNameA));
  });

  it("Settlement fetchUserEventRefUTxO handles deposit, tx-order, and withdrawal", async () => {
    const lucid = makeLucidMock();

    lucid.utxosAt.mockResolvedValueOnce([
      makeUtxo({
        txHash: txHashA,
        outputIndex: 0,
        datum: validDepositDatumCbor,
      }),
    ]);
    const deposit = await Effect.runPromise(
      fetchUserEventRefUTxO("Deposit", addressScriptA, policyIdA, lucid as any),
    );

    lucid.utxosAt.mockResolvedValueOnce([
      makeUtxo({
        txHash: txHashB,
        outputIndex: 1,
        datum: validTxOrderDatumCbor,
      }),
    ]);
    const txOrder = await Effect.runPromise(
      fetchUserEventRefUTxO(
        { TxOrder: { validityOverride: "TxIsValid" } } as any,
        addressScriptA,
        policyIdA,
        lucid as any,
      ),
    );

    lucid.utxosAt.mockResolvedValueOnce([
      makeUtxo({
        txHash: "44".repeat(32),
        outputIndex: 2,
        datum: validWithdrawalDatumCbor,
      }),
    ]);
    const withdrawal = await Effect.runPromise(
      fetchUserEventRefUTxO(
        { Withdrawal: { validityOverride: "WithdrawalIsValid" } } as any,
        addressScriptA,
        policyIdA,
        lucid as any,
      ),
    );

    expect(deposit.utxo.txHash).toBe(txHashA);
    expect(txOrder.utxo.txHash).toBe(txHashB);
    expect(withdrawal.utxo.txHash).toBe("44".repeat(32));
  });
});
