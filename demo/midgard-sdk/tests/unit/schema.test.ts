import { describe, it, expect } from "vitest";
import { Data } from "@lucid-evolution/lucid";

import {
  ActiveOperatorMintRedeemer,
  ConfirmedState,
  DepositDatum,
  FraudProofCatalogueDatum,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  Header,
  OutputReference,
  RegisteredOperatorDatum,
  RetiredOperatorDatum,
  TxOrderDatum,
  Value,
  WithdrawalOrderDatum,
} from "../../src/index.ts";
import { SettlementDatum } from "../../src/settlement.ts";
import { UserEventMintRedeemer } from "../../src/user-events/internals.ts";
import {
  addressDataKeyA,
  assetNameA,
  depositDatumFixture,
  hexA,
  merkleRootA,
  merkleRootB,
  outRefA,
  policyIdA,
  posixT0,
  posixT1,
  posixT2,
  pubKeyHashA,
  scriptHashA,
  txHashA,
  txOrderDatumFixture,
  valueFixture,
  withdrawalDatumFixture,
} from "./helpers.ts";

describe("SDK unit schema coverage", () => {
  it("Output reference schema round trips", () => {
    const cbor = Data.to(outRefA, OutputReference);
    const decoded = Data.from(cbor, OutputReference) as typeof outRefA;
    expect(decoded.txHash.hash).toBe(outRefA.txHash.hash);
    expect(decoded.outputIndex).toBe(outRefA.outputIndex);
  });

  it("Value schema round trips ADA map", () => {
    const cbor = Data.to(valueFixture, Value);
    const decoded = Data.from(cbor, Value) as typeof valueFixture;
    expect(decoded.inner.get(policyIdA)?.get(assetNameA)).toBe(42n);
  });

  it("Header schema round trips", () => {
    const header = {
      prevUtxosRoot: merkleRootA,
      utxosRoot: merkleRootB,
      transactionsRoot: merkleRootA,
      depositsRoot: merkleRootB,
      withdrawalsRoot: merkleRootA,
      startTime: posixT0,
      endTime: posixT1,
      prevHeaderHash: "77".repeat(28),
      operatorVkey: pubKeyHashA,
      protocolVersion: 1n,
    };
    const cbor = Data.to(header, Header);
    const decoded = Data.from(cbor, Header) as typeof header;
    expect(decoded.prevUtxosRoot).toBe(merkleRootA);
    expect(decoded.operatorVkey).toBe(pubKeyHashA);
  });

  it("Confirmed state schema round trips", () => {
    const confirmed = {
      headerHash: "88".repeat(28),
      prevHeaderHash: "99".repeat(28),
      utxoRoot: merkleRootA,
      startTime: posixT0,
      endTime: posixT1,
      protocolVersion: 1n,
    };
    const cbor = Data.to(confirmed, ConfirmedState);
    const decoded = Data.from(cbor, ConfirmedState) as typeof confirmed;
    expect(decoded.headerHash).toBe(confirmed.headerHash);
    expect(decoded.utxoRoot).toBe(merkleRootA);
  });

  it("Deposit datum schema round trips", () => {
    const cbor = Data.to(depositDatumFixture, DepositDatum);
    const decoded = Data.from(cbor, DepositDatum) as typeof depositDatumFixture;
    expect(decoded.event.id.txHash.hash).toBe(txHashA);
    expect(decoded.inclusionTime).toBe(posixT1);
  });

  it("Tx-order datum schema round trips", () => {
    const cbor = Data.to(txOrderDatumFixture, TxOrderDatum);
    const decoded = Data.from(cbor, TxOrderDatum) as typeof txOrderDatumFixture;
    expect(decoded.event.tx).toBe(hexA);
    expect(decoded.refundAddress.paymentCredential).toEqual(
      addressDataKeyA.paymentCredential,
    );
  });

  it("Withdrawal datum schema round trips", () => {
    const cbor = Data.to(withdrawalDatumFixture, WithdrawalOrderDatum);
    const decoded = Data.from(
      cbor,
      WithdrawalOrderDatum,
    ) as typeof withdrawalDatumFixture;
    expect(decoded.event.info.validity).toBe("WithdrawalIsValid");
    expect(decoded.inclusionTime).toBe(posixT1);
  });

  it("User-event mint redeemer authenticates event", () => {
    const redeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const cbor = Data.to(redeemer, UserEventMintRedeemer);
    const decoded = Data.from(cbor, UserEventMintRedeemer) as typeof redeemer;
    expect(decoded.AuthenticateEvent.nonceInputIndex).toBe(0n);
    expect(decoded.AuthenticateEvent.hubRefInputIndex).toBe(0n);
  });

  it("User-event mint redeemer burns event NFT", () => {
    const redeemer = {
      BurnEventNFT: {
        nonceAssetName: assetNameA,
        witnessUnregistrationRedeemerIndex: 0n,
      },
    };
    const cbor = Data.to(redeemer, UserEventMintRedeemer);
    const decoded = Data.from(cbor, UserEventMintRedeemer) as typeof redeemer;
    expect(decoded.BurnEventNFT.nonceAssetName).toBe(assetNameA);
    expect(decoded.BurnEventNFT.witnessUnregistrationRedeemerIndex).toBe(0n);
  });

  it("Active operator redeemers round trip simple Init", () => {
    const cbor = Data.to("Init", ActiveOperatorMintRedeemer);
    const decoded = Data.from(cbor, ActiveOperatorMintRedeemer);
    expect(decoded).toBe("Init");
  });

  it("Registered operator datum round trips", () => {
    const datum = { registrationTime: posixT1 };
    const cbor = Data.to(datum, RegisteredOperatorDatum);
    const decoded = Data.from(cbor, RegisteredOperatorDatum) as typeof datum;
    expect(decoded.registrationTime).toBe(posixT1);
  });

  it("Retired operator datum round trips", () => {
    const datum = {
      key: pubKeyHashA,
      link: scriptHashA,
      bondUnlockTime: posixT2,
    };
    const cbor = Data.to(datum, RetiredOperatorDatum);
    const decoded = Data.from(cbor, RetiredOperatorDatum) as typeof datum;
    expect(decoded.key).toBe(pubKeyHashA);
    expect(decoded.link).toBe(scriptHashA);
    expect(decoded.bondUnlockTime).toBe(posixT2);
  });

  it("Settlement datum round trips without claim", () => {
    const datum = {
      depositsRoot: merkleRootA,
      withdrawalsRoot: merkleRootB,
      transactionsRoot: merkleRootA,
      resolutionClaim: null,
    };
    const cbor = Data.to(datum, SettlementDatum);
    const decoded = Data.from(cbor, SettlementDatum) as typeof datum;
    expect(decoded.depositsRoot).toBe(merkleRootA);
    expect(decoded.resolutionClaim).toBeNull();
  });

  it("Settlement datum round trips with claim", () => {
    const datum = {
      depositsRoot: merkleRootA,
      withdrawalsRoot: merkleRootB,
      transactionsRoot: merkleRootA,
      resolutionClaim: {
        resolutionTime: posixT2,
        operator: pubKeyHashA,
      },
    };
    const cbor = Data.to(datum, SettlementDatum);
    const decoded = Data.from(cbor, SettlementDatum) as typeof datum;
    expect(decoded.resolutionClaim?.resolutionTime).toBe(posixT2);
    expect(decoded.resolutionClaim?.operator).toBe(pubKeyHashA);
  });

  it("Fraud-proof catalogue datum round trips", () => {
    const cbor = Data.to(merkleRootA, FraudProofCatalogueDatum);
    const decoded = Data.from(cbor, FraudProofCatalogueDatum);
    expect(decoded).toBe(merkleRootA);
  });

  it("Fraud-proof computation step datum round trips", () => {
    const datum = { fraudProver: pubKeyHashA, data: hexA };
    const cbor = Data.to(datum, FraudProofComputationThreadStepDatum);
    const decoded = Data.from(
      cbor,
      FraudProofComputationThreadStepDatum,
    ) as typeof datum;
    expect(decoded.fraudProver).toBe(pubKeyHashA);
    expect(decoded.data).toBe(hexA);
  });

  it("Fraud-proof token datum round trips", () => {
    const datum = { fraudProver: pubKeyHashA };
    const cbor = Data.to(datum, FraudProofTokenDatum);
    const decoded = Data.from(cbor, FraudProofTokenDatum) as typeof datum;
    expect(decoded.fraudProver).toBe(pubKeyHashA);
  });
});
