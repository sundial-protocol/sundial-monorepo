import { Effect } from "effect";
import { Data, credentialToAddress, toUnit } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  AddressData,
  ConfirmedState,
  DepositDatum,
  Header,
  NodeDatum,
  OutputReference,
  TxOrderDatum,
  Value,
  WithdrawalOrderDatum,
} from "../../src/index.ts";

export type BuilderSpy = {
  collectFrom: ReturnType<typeof vi.fn>;
  readFrom: ReturnType<typeof vi.fn>;
  mintAssets: ReturnType<typeof vi.fn>;
  validTo: ReturnType<typeof vi.fn>;
  validFrom: ReturnType<typeof vi.fn>;
  addSignerKey: ReturnType<typeof vi.fn>;
  setMinFee: ReturnType<typeof vi.fn>;
  compose: ReturnType<typeof vi.fn>;
  complete: ReturnType<typeof vi.fn>;
  completeProgram: ReturnType<typeof vi.fn>;
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

export const txHashA = "11".repeat(32);
export const txHashB = "22".repeat(32);
export const policyIdA = "aa".repeat(28);
export const pubKeyHashA = "33".repeat(28);
export const scriptHashA = "44".repeat(28);
export const merkleRootA = "55".repeat(32);
export const merkleRootB = "66".repeat(32);
export const hexA = "deadbeef0123456789abcdef";
export const assetNameA = "abcd";
export const posixT0 = 1_000_000n;
export const posixT1 = 1_050_000n;
export const posixT2 = 1_100_000n;
export const posixT3 = 1_150_000n;

export const outRefA = {
  txHash: { hash: txHashA },
  outputIndex: 0n,
};

export const outRefB = {
  txHash: { hash: txHashB },
  outputIndex: 1n,
};

export const addressKeyA = credentialToAddress("Preview", {
  type: "Key",
  hash: pubKeyHashA,
});

export const addressScriptA = credentialToAddress("Preview", {
  type: "Script",
  hash: scriptHashA,
});

export const addressDataKeyA: AddressData = {
  paymentCredential: {
    PublicKeyCredential: [pubKeyHashA],
  },
  stakeCredential: null,
};

export const validatorA = {
  mintingScriptCBOR: "4d01000033222220051200120011",
  mintingScript: { type: "PlutusV3", script: "4d01000033222220051200120011" },
  policyId: policyIdA,
  spendingScriptCBOR: "4d01000033222220051200120011",
  spendingScript: { type: "PlutusV3", script: "4d01000033222220051200120011" },
  spendingScriptHash: scriptHashA,
  spendingScriptAddress: addressScriptA,
};

export const nodeDatumFixture = {
  key: { Key: { key: merkleRootA } },
  next: { Key: { key: merkleRootB } },
  data: hexA,
};

export const valueFixture = {
  inner: new Map([[policyIdA, new Map([[assetNameA, 42n]])]]),
};

export const withdrawalBodyFixture = {
  l2_outref: outRefB,
  l2_owner: pubKeyHashA,
  l2_value: valueFixture,
  l1_address: addressDataKeyA,
  l1_datum: "NoDatum",
};

export const withdrawalInfoFixture = {
  body: withdrawalBodyFixture,
  signature: new Map(),
  validity: "WithdrawalIsValid",
};

export const depositDatumFixture = {
  event: {
    id: outRefA,
    info: {
      l2Address: { PublicKeyCredential: [pubKeyHashA] },
      l2Datum: null,
    },
  },
  inclusionTime: posixT1,
};

export const txOrderDatumFixture = {
  event: {
    id: outRefA,
    tx: hexA,
  },
  inclusionTime: posixT1,
  refundAddress: addressDataKeyA,
  refundDatum: null,
};

export const withdrawalDatumFixture = {
  event: {
    id: outRefA,
    info: withdrawalInfoFixture,
  },
  inclusionTime: posixT1,
  refundAddress: addressDataKeyA,
  refundDatum: null,
};

export const validNodeDatumCbor = Data.to(nodeDatumFixture, NodeDatum);
export const validDepositDatumCbor = Data.to(depositDatumFixture, DepositDatum);
export const validTxOrderDatumCbor = Data.to(txOrderDatumFixture, TxOrderDatum);
export const validWithdrawalDatumCbor = Data.to(
  withdrawalDatumFixture,
  WithdrawalOrderDatum,
);

export const confirmedStateFixture = {
  headerHash: "88".repeat(28),
  prevHeaderHash: "99".repeat(28),
  utxoRoot: merkleRootA,
  startTime: posixT0,
  endTime: posixT1,
  protocolVersion: 1n,
};

export const headerFixture = {
  prevUtxosRoot: merkleRootA,
  utxosRoot: merkleRootB,
  transactionsRoot: merkleRootA,
  depositsRoot: merkleRootB,
  withdrawalsRoot: merkleRootA,
  startTime: posixT1,
  endTime: posixT2,
  prevHeaderHash: "aa".repeat(28),
  operatorVkey: pubKeyHashA,
  protocolVersion: 1n,
};

export const makeMidgardValidatorsFixture = () => ({
  hubOracle: { ...validatorA },
  stateQueue: { ...validatorA },
  scheduler: { ...validatorA },
  registeredOperators: { ...validatorA },
  activeOperators: { ...validatorA },
  retiredOperators: { ...validatorA },
  escapeHatch: { ...validatorA },
  fraudProofCatalogue: { ...validatorA },
  fraudProof: { ...validatorA },
  deposit: { ...validatorA },
  withdrawal: { ...validatorA },
  txOrder: { ...validatorA },
  settlement: { ...validatorA },
  payout: { ...validatorA },
  reserve: {
    spendingScriptCBOR: validatorA.spendingScriptCBOR,
    spendingScript: validatorA.spendingScript,
    spendingScriptHash: validatorA.spendingScriptHash,
    spendingScriptAddress: validatorA.spendingScriptAddress,
    withdrawalScriptCBOR: validatorA.spendingScriptCBOR,
    withdrawalScript: validatorA.spendingScript,
    withdrawalScriptHash: validatorA.spendingScriptHash,
  },
  fraudProofs: {
    doubleSpend: {
      spendingScriptCBOR: validatorA.spendingScriptCBOR,
      spendingScript: validatorA.spendingScript,
      spendingScriptHash: validatorA.spendingScriptHash,
      spendingScriptAddress: validatorA.spendingScriptAddress,
    },
    nonExistentInput: {
      spendingScriptCBOR: validatorA.spendingScriptCBOR,
      spendingScript: validatorA.spendingScript,
      spendingScriptHash: validatorA.spendingScriptHash,
      spendingScriptAddress: validatorA.spendingScriptAddress,
    },
    nonExistentInputNoIndex: {
      spendingScriptCBOR: validatorA.spendingScriptCBOR,
      spendingScript: validatorA.spendingScript,
      spendingScriptHash: validatorA.spendingScriptHash,
      spendingScriptAddress: validatorA.spendingScriptAddress,
    },
    invalidRange: {
      spendingScriptCBOR: validatorA.spendingScriptCBOR,
      spendingScript: validatorA.spendingScript,
      spendingScriptHash: validatorA.spendingScriptHash,
      spendingScriptAddress: validatorA.spendingScriptAddress,
    },
  },
});

export const makeBuilderSpy = (): BuilderSpy & Record<string, unknown> => {
  const builder: Record<string, unknown> = {};
  const collectFrom = vi.fn(() => builder);
  const readFrom = vi.fn(() => builder);
  const mintAssets = vi.fn(() => builder);
  const validTo = vi.fn(() => builder);
  const validFrom = vi.fn(() => builder);
  const addSignerKey = vi.fn(() => builder);
  const setMinFee = vi.fn(() => builder);
  const compose = vi.fn(() => builder);
  const complete = vi.fn(async () => ({ signed: true }));
  const completeProgram = vi.fn(() => Effect.succeed({ signed: true }));
  const payToAddressWithData = vi.fn(() => builder);
  const payToAddress = vi.fn(() => builder);
  const payToContract = vi.fn(() => builder);
  const attachScript = vi.fn(() => builder);
  const attachMintingPolicy = vi.fn(() => builder);

  Object.assign(builder, {
    collectFrom,
    readFrom,
    mintAssets,
    validTo,
    validFrom,
    addSignerKey,
    setMinFee,
    compose,
    complete,
    completeProgram,
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

export const makeLucidMock = (
  builder?: BuilderSpy & Record<string, unknown>,
) => {
  const selectedBuilder = builder ?? makeBuilderSpy();
  return {
    newTx: vi.fn(() => selectedBuilder),
    utxosAt: vi.fn(async () => []),
    wallet: vi.fn(() => ({
      getUtxos: vi.fn(async () => []),
      address: vi.fn(async () => addressKeyA),
    })),
    config: vi.fn(() => ({ network: "Preview" })),
  };
};

export const makeUtxo = (params: {
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

export const outputReferenceSchema = OutputReference;
export const valueSchema = Value;
export const headerSchema = Header;
export const confirmedStateSchema = ConfirmedState;
