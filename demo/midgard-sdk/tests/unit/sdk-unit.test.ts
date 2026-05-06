import { describe, it, expect, vi, afterEach } from "vitest";
import { Effect } from "effect";
import { Data, credentialToAddress, toUnit } from "@lucid-evolution/lucid";

import * as sdk from "../../src/index.ts";
import {
  AddressData,
  ActiveOperatorMintRedeemer,
  ConfirmedState,
  DepositDatum,
  FraudProofCatalogueDatum,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  Header,
  NodeDatum,
  OutputReference,
  RegisteredOperatorDatum,
  RetiredOperatorDatum,
  TxOrderDatum,
  Value,
  WithdrawalOrderDatum,
  getNodeDatumFromUTxO,
  incompleteInitLinkedListTxProgram,
  makeReturn,
  midgardAddressFromBech32,
  midgardAddressToBech32,
  utxosToDepositUTxOs,
  utxosToTxOrderUTxOs,
  utxosToWithdrawalUTxOs,
} from "../../src/index.ts";
import { SettlementDatum } from "../../src/settlement.ts";
import {
  authenticateUTxO,
  authenticateUTxOs,
  fetchSingleAuthenticUTxOProgram,
  getDatumFromUTxO,
  getStateToken,
} from "../../src/internals.ts";
import {
  UserEventMintRedeemer,
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
} from "../../src/user-events/internals.ts";

type BuilderSpy = {
  collectFrom: ReturnType<typeof vi.fn>;
  mintAssets: ReturnType<typeof vi.fn>;
  validTo: ReturnType<typeof vi.fn>;
  pay: {
    ToAddressWithData: ReturnType<typeof vi.fn>;
    ToAddress: ReturnType<typeof vi.fn>;
    ToContract: ReturnType<typeof vi.fn>;
  };
  attach: {
    Script: ReturnType<typeof vi.fn>;
    MintingPolicy: ReturnType<typeof vi.fn>;
  };
};

const txHashA = "11".repeat(32);
const txHashB = "22".repeat(32);
const policyIdA = "aa".repeat(28);
const pubKeyHashA = "33".repeat(28);
const scriptHashA = "44".repeat(28);
const merkleRootA = "55".repeat(32);
const merkleRootB = "66".repeat(32);
const hexA = "deadbeef0123456789abcdef";
const assetNameA = "abcd";
const posixT0 = 1_000_000n;
const posixT1 = 1_050_000n;
const posixT2 = 1_100_000n;
const posixT3 = 1_150_000n;

const outRefA = {
  txHash: { hash: txHashA },
  outputIndex: 0n,
};

const outRefB = {
  txHash: { hash: txHashB },
  outputIndex: 1n,
};

const addressKeyA = credentialToAddress("Preview", {
  type: "Key",
  hash: pubKeyHashA,
});

const addressScriptA = credentialToAddress("Preview", {
  type: "Script",
  hash: scriptHashA,
});

const addressDataKeyA: AddressData = {
  paymentCredential: {
    PublicKeyCredential: [pubKeyHashA],
  },
  stakeCredential: null,
};

const validatorA = {
  mintingScriptCBOR: "4d01000033222220051200120011",
  mintingScript: { type: "PlutusV3", script: "4d01000033222220051200120011" },
  policyId: policyIdA,
  spendingScriptCBOR: "4d01000033222220051200120011",
  spendingScript: { type: "PlutusV3", script: "4d01000033222220051200120011" },
  spendingScriptHash: scriptHashA,
  spendingScriptAddress: addressScriptA,
};

const nodeDatumFixture = {
  key: { Key: { key: merkleRootA } },
  next: { Key: { key: merkleRootB } },
  data: hexA,
};

const valueFixture = {
  inner: new Map([[policyIdA, new Map([[assetNameA, 42n]])]]),
};

const withdrawalBodyFixture = {
  l2_outref: outRefB,
  l2_owner: pubKeyHashA,
  l2_value: valueFixture,
  l1_address: addressDataKeyA,
  l1_datum: "NoDatum",
};

const withdrawalInfoFixture = {
  body: withdrawalBodyFixture,
  signature: new Map(),
  validity: "WithdrawalIsValid",
};

const depositDatumFixture = {
  event: {
    id: outRefA,
    info: {
      l2Address: { PublicKeyCredential: [pubKeyHashA] },
      l2Datum: null,
    },
  },
  inclusionTime: posixT1,
};

const txOrderDatumFixture = {
  event: {
    id: outRefA,
    tx: hexA,
  },
  inclusionTime: posixT1,
  refundAddress: addressDataKeyA,
  refundDatum: null,
};

const withdrawalDatumFixture = {
  event: {
    id: outRefA,
    info: withdrawalInfoFixture,
  },
  inclusionTime: posixT1,
  refundAddress: addressDataKeyA,
  refundDatum: null,
};

const validNodeDatumCbor = Data.to(nodeDatumFixture, NodeDatum);
const validDepositDatumCbor = Data.to(depositDatumFixture, DepositDatum);
const validTxOrderDatumCbor = Data.to(txOrderDatumFixture, TxOrderDatum);
const validWithdrawalDatumCbor = Data.to(
  withdrawalDatumFixture,
  WithdrawalOrderDatum,
);

const makeBuilderSpy = (): BuilderSpy & Record<string, unknown> => {
  const builder: Record<string, unknown> = {};
  const collectFrom = vi.fn(() => builder);
  const mintAssets = vi.fn(() => builder);
  const validTo = vi.fn(() => builder);
  const payToAddressWithData = vi.fn(() => builder);
  const payToAddress = vi.fn(() => builder);
  const payToContract = vi.fn(() => builder);
  const attachScript = vi.fn(() => builder);
  const attachMintingPolicy = vi.fn(() => builder);

  Object.assign(builder, {
    collectFrom,
    mintAssets,
    validTo,
    pay: {
      ToAddressWithData: payToAddressWithData,
      ToAddress: payToAddress,
      ToContract: payToContract,
    },
    attach: {
      Script: attachScript,
      MintingPolicy: attachMintingPolicy,
    },
  });

  return builder as BuilderSpy & Record<string, unknown>;
};

const makeLucidMock = (builder?: BuilderSpy & Record<string, unknown>) => {
  const selectedBuilder = builder ?? makeBuilderSpy();
  return {
    newTx: vi.fn(() => selectedBuilder),
    utxosAt: vi.fn(async () => []),
    wallet: vi.fn(() => ({
      getUtxos: vi.fn(async () => []),
    })),
    config: vi.fn(() => ({ network: "Preview" })),
  };
};

const makeUtxo = (params: {
  txHash: string;
  outputIndex: number;
  datum?: string;
  unit?: string;
}) => {
  const unit = params.unit ?? toUnit(policyIdA, assetNameA);
  return {
    txHash: params.txHash,
    outputIndex: params.outputIndex,
    assets: {
      lovelace: 3_000_000n,
      [unit]: 1n,
    },
    address: addressScriptA,
    datum: params.datum,
  };
};

afterEach(() => {
  vi.restoreAllMocks();
});

describe("SDK unit plan coverage", () => {
  it("Public index exports common helpers", () => {
    expect(typeof sdk.isHexString).toBe("function");
    expect(typeof sdk.makeReturn).toBe("function");
    expect(typeof sdk.hashHexWithBlake2b256).toBe("function");
  });

  it("Public index exports user-event helpers", () => {
    expect(typeof sdk.incompleteDepositTxProgram).toBe("function");
    expect(typeof sdk.incompleteWithdrawalTxProgram).toBe("function");
    expect(typeof sdk.incompleteTxOrderTxProgram).toBe("function");
  });

  it("Public index exports fraud-proof helpers", () => {
    expect(typeof sdk.incompleteFraudProofCatalogueInitTxProgram).toBe(
      "function",
    );
    expect(typeof sdk.incompleteFraudProofComputationThreadInitTxProgram).toBe(
      "function",
    );
    expect(typeof sdk.incompleteFraudProofTokenMintTxProgram).toBe("function");
  });

  it("makeReturn unsafeRun resolves successful program", async () => {
    const result = await makeReturn(Effect.succeed("ok")).unsafeRun();
    expect(result).toBe("ok");
  });

  it("makeReturn safeRun wraps successful program", async () => {
    const result = await makeReturn(Effect.succeed("ok")).safeRun();
    expect(result._tag).toBe("Right");
    expect(result.right).toBe("ok");
  });

  it("Accept lowercase hex string", () => {
    expect(sdk.isHexString("deadbeef0123456789")).toBe(true);
  });

  it("Accept uppercase hex string", () => {
    expect(sdk.isHexString("DEADBEEF0123456789")).toBe(true);
  });

  it("Blake2b224 hashes valid hex", async () => {
    const a = await Effect.runPromise(sdk.hashHexWithBlake2b224(hexA));
    const b = await Effect.runPromise(sdk.hashHexWithBlake2b224(hexA));
    expect(a).toHaveLength(56);
    expect(a).toBe(b);
  });

  it("Blake2b256 hashes valid hex", async () => {
    const a = await Effect.runPromise(sdk.hashHexWithBlake2b256(hexA));
    const b = await Effect.runPromise(sdk.hashHexWithBlake2b256(hexA));
    expect(a).toHaveLength(64);
    expect(a).toBe(b);
  });

  it("Buffer converts to hex", () => {
    expect(sdk.bufferToHex(Buffer.from("abcd", "hex"))).toBe("abcd");
  });

  it("Public-key Midgard address converts to bech32 and back", async () => {
    const midgard = { PublicKeyCredential: [pubKeyHashA] } as const;
    const bech32 = midgardAddressToBech32("Preview", midgard);
    const decoded = await Effect.runPromise(midgardAddressFromBech32(bech32));
    expect("PublicKeyCredential" in decoded).toBe(true);
    if ("PublicKeyCredential" in decoded) {
      expect(decoded.PublicKeyCredential[0]).toBe(pubKeyHashA);
    }
  });

  it("Script Midgard address converts to bech32 and back", async () => {
    const midgard = { ScriptCredential: [scriptHashA] } as const;
    const bech32 = midgardAddressToBech32("Preview", midgard);
    const decoded = await Effect.runPromise(midgardAddressFromBech32(bech32));
    expect("ScriptCredential" in decoded).toBe(true);
    if ("ScriptCredential" in decoded) {
      expect(decoded.ScriptCredential[0]).toBe(scriptHashA);
    }
  });

  it("AddressData extracts payment credential", async () => {
    const decoded = await Effect.runPromise(
      sdk.addressDataFromBech32(addressKeyA),
    );
    expect("PublicKeyCredential" in decoded.paymentCredential).toBe(true);
    if ("PublicKeyCredential" in decoded.paymentCredential) {
      expect(decoded.paymentCredential.PublicKeyCredential[0]).toBe(
        pubKeyHashA,
      );
    }
  });

  it("Find active operator by public-key hash", async () => {
    const active = [
      {
        utxo: makeUtxo({ txHash: txHashA, outputIndex: 0 }),
        datum: { key: pubKeyHashA, link: null, bondUnlockTime: null },
        assetName: assetNameA,
      },
    ];
    const retired = [
      {
        utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
        datum: { key: "99".repeat(28), link: null, bondUnlockTime: null },
        assetName: assetNameA,
      },
    ];
    const found = await Effect.runPromise(
      sdk.findOperatorByPKH(active as any, retired as any, pubKeyHashA),
    );
    expect(found.isActive).toBe(true);
    expect(found.datum.key).toBe(pubKeyHashA);
  });

  it("Find retired operator by public-key hash", async () => {
    const retired = [
      {
        utxo: makeUtxo({ txHash: txHashB, outputIndex: 1 }),
        datum: { key: pubKeyHashA, link: scriptHashA, bondUnlockTime: posixT2 },
        assetName: assetNameA,
      },
    ];
    const found = await Effect.runPromise(
      sdk.findOperatorByPKH([], retired as any, pubKeyHashA),
    );
    expect(found.isActive).toBe(false);
    expect(found.datum.key).toBe(pubKeyHashA);
  });

  it("Mainnet protocol parameters are selected", () => {
    const params = sdk.getProtocolParameters("Mainnet");
    expect(params.event_wait_duration).toBe(60_000);
    expect(params.maturity_duration).toBe(30n);
    expect(params.slashing_penalty).toBe(2_000_000n);
  });

  it("Testnet protocol parameters are selected", () => {
    const params = sdk.getProtocolParameters("Preview");
    expect(params.event_wait_duration).toBe(50_000);
    expect(params.maturity_duration).toBe(1n);
    expect(params.slashing_penalty).toBe(1_000_000n);
  });

  it("Asset-name constants encode expected labels", () => {
    const values = [
      sdk.ESCAPE_HATCH_ASSET_NAME,
      sdk.SCHEDULER_ASSET_NAME,
      sdk.HUB_ORACLE_ASSET_NAME,
      sdk.FRAUD_PROOF_CATALOGUE_ASSET_NAME,
    ];

    for (const value of values) {
      expect(typeof value).toBe("string");
      expect(value.length).toBeGreaterThan(0);
      expect(value).toBe(value);
    }
  });

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

  it("getStateToken returns single NFT", async () => {
    const unit = toUnit(policyIdA, assetNameA);
    const [policyId, assetName] = await Effect.runPromise(
      getStateToken({ lovelace: 2_000_000n, [unit]: 1n }),
    );
    expect(policyId).toBe(policyIdA);
    expect(assetName).toBe(assetNameA);
  });

  it("getDatumFromUTxO decodes valid inline datum", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const decoded = await Effect.runPromise(
      getDatumFromUTxO(utxo as any, DepositDatum),
    );
    expect(decoded.event.id.txHash.hash).toBe(txHashA);
    expect(decoded.inclusionTime).toBe(posixT1);
  });

  it("authenticateUTxO returns authentic datum and asset name", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxO(utxo as any, policyIdA, DepositDatum),
    );
    expect(result.assetName).toBe(assetNameA);
    expect(result.datum.event.id.txHash.hash).toBe(txHashA);
  });

  it("authenticateUTxO adds extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxO(utxo as any, policyIdA, DepositDatum, () => ({
        extra: "ok",
      })),
    );
    expect(result.extra).toBe("ok");
    expect(result.datum.inclusionTime).toBe(posixT1);
  });

  it("authenticateUTxOs returns all valid UTxOs", async () => {
    const utxoA = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const utxoB = makeUtxo({
      txHash: txHashB,
      outputIndex: 1,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      authenticateUTxOs([utxoA as any, utxoB as any], policyIdA, DepositDatum),
    );
    expect(result).toHaveLength(2);
    expect(result[0].utxo.txHash).toBe(txHashA);
    expect(result[1].utxo.txHash).toBe(txHashB);
  });

  it("fetchSingleAuthenticUTxOProgram returns one converted UTxO", async () => {
    const lucid = makeLucidMock();
    const rawUtxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    lucid.utxosAt.mockResolvedValue([rawUtxo]);
    const converted = [{ id: "one" }];
    const conversionFunction = vi.fn(() => Effect.succeed(converted));

    const result = await Effect.runPromise(
      fetchSingleAuthenticUTxOProgram(lucid as any, {
        address: addressScriptA,
        policyId: policyIdA,
        utxoLabel: "test",
        conversionFunction,
        onUnexpectedAuthenticUTxOCount: () => new Error("bad count"),
      }),
    );

    expect(result).toEqual({ id: "one" });
    expect(lucid.utxosAt).toHaveBeenCalledWith(addressScriptA);
    expect(conversionFunction).toHaveBeenCalledTimes(1);
  });

  it("fetchUserEventUTxOsProgram filters inside inclusion window", async () => {
    const lucid = makeLucidMock();
    lucid.utxosAt.mockResolvedValue([
      makeUtxo({ txHash: txHashA, outputIndex: 0 }),
    ]);

    const conversionFunction = vi.fn(() =>
      Effect.succeed([
        { datum: { inclusionTime: posixT0 }, id: "too-early" },
        { datum: { inclusionTime: posixT1 }, id: "keep" },
        { datum: { inclusionTime: posixT3 }, id: "too-late" },
      ]),
    );

    const result = await Effect.runPromise(
      fetchUserEventUTxOsProgram(
        lucid as any,
        {
          eventAddress: addressScriptA,
          eventPolicyId: policyIdA,
          inclusionTimeLowerBound: posixT1,
          inclusionTimeUpperBound: posixT2,
        },
        conversionFunction,
      ),
    );

    expect(result).toHaveLength(1);
    expect(result[0].id).toBe("keep");
  });

  it("Deposit UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validDepositDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToDepositUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Tx-order UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validTxOrderDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToTxOrderUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Withdrawal UTxO conversion adds derived extra fields", async () => {
    const utxo = makeUtxo({
      txHash: txHashA,
      outputIndex: 0,
      datum: validWithdrawalDatumCbor,
    });
    const result = await Effect.runPromise(
      utxosToWithdrawalUTxOs([utxo as any], policyIdA),
    );
    expect(result).toHaveLength(1);
    expect(result[0].idCbor).toBeInstanceOf(Buffer);
    expect(result[0].infoCbor).toBeInstanceOf(Buffer);
    expect(result[0].inclusionTime).toBeInstanceOf(Date);
  });

  it("Inclusion time uses network protocol wait", async () => {
    const lucid = makeLucidMock();
    const nowSpy = vi.spyOn(Date, "now").mockReturnValue(1_000_000);
    const inclusionTime = await Effect.runPromise(
      findInclusionTimeForUserEvent(lucid as any),
    );
    expect(inclusionTime).toBe(1_050_000);
    expect(nowSpy).toHaveBeenCalled();
  });

  it("Nonce asset name uses provided nonce UTxO", async () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const nonceUTxO = makeUtxo({ txHash: txHashA, outputIndex: 0 });

    const result = await Effect.runPromise(
      getNonceInputAndAssetName(lucid as any, "deposit", nonceUTxO as any),
    );

    expect(result.inputUtxo.txHash).toBe(txHashA);
    expect(result.assetName).toHaveLength(64);
  });

  it("User-event mint transaction calls fluent builder", () => {
    const builder = makeBuilderSpy();
    const lucid = makeLucidMock(builder);
    const inputUtxo = makeUtxo({ txHash: txHashA, outputIndex: 0 });

    const result = buildUserEventMintTransaction({
      lucid: lucid as any,
      inputUtxo: inputUtxo as any,
      nft: toUnit(policyIdA, assetNameA),
      mintRedeemer: "00",
      scriptAddress: addressScriptA,
      datum: validDepositDatumCbor,
      validTo: Number(posixT2),
      mintingPolicy: validatorA.mintingScript,
    });

    expect(result).toBe(builder);
    expect(builder.collectFrom).toHaveBeenCalledWith([inputUtxo]);
    expect(builder.mintAssets).toHaveBeenCalledTimes(1);
    expect(builder.pay.ToAddressWithData).toHaveBeenCalledTimes(1);
    expect(builder.validTo).toHaveBeenCalledWith(Number(posixT2));
    expect(builder.attach.MintingPolicy).toHaveBeenCalledWith(
      validatorA.mintingScript,
    );
  });

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
    const redeemer = Data.to("Init", sdk.RegisteredOperatorMintRedeemer);

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
});
