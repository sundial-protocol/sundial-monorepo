import { Effect, Logger } from "effect";
import {
  Data,
  credentialToAddress,
  toUnit,
  type Assets,
  type Delegation,
  type LucidEvolution,
  type Script,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  type Wallet,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  AddressData,
  AuthenticatedValidator,
  ConfirmedState,
  DepositDatum,
  Header,
  NodeDatum,
  NODE_ASSET_NAME,
  OutputReference,
  StateQueueDatum,
  StateQueueUTxO,
  TxOrderDatum,
  Value,
  WithdrawalOrderDatum,
} from "../../src/index.ts";

type CompletedTxFixture = Readonly<{ signed: true }>;

export type BuilderSpy = TxBuilder & {
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
  completedTx: CompletedTxFixture;
};

export type LucidMock = LucidEvolution & {
  newTx: ReturnType<typeof vi.fn>;
  utxosAt: ReturnType<typeof vi.fn>;
  utxosAtWithUnit: ReturnType<typeof vi.fn>;
  wallet: ReturnType<typeof vi.fn>;
  config: ReturnType<typeof vi.fn>;
};

type WalletFixture = Pick<
  Wallet,
  | "address"
  | "rewardAddress"
  | "getUtxos"
  | "getUtxosCore"
  | "getDelegation"
  | "signTx"
  | "signMessage"
  | "submitTx"
> & {
  overrideUTxOs: ReturnType<typeof vi.fn>;
};

const unexpectedCallError = (method: string): Error =>
  new Error(`unexpected call: ${method}`);

const makeUnexpectedMethodSpy = (method: string): ReturnType<typeof vi.fn> =>
  vi.fn(() => {
    throw unexpectedCallError(method);
  });

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

const plutusV3Script: Script = {
  type: "PlutusV3",
  script: "4d01000033222220051200120011",
};

export const validatorA: AuthenticatedValidator = {
  mintingScriptCBOR: "4d01000033222220051200120011",
  mintingScript: plutusV3Script,
  policyId: policyIdA,
  spendingScriptCBOR: "4d01000033222220051200120011",
  spendingScript: plutusV3Script,
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

const makeTxSignBuilderSpy = (): CompletedTxFixture => {
  return { signed: true };
};

export const makeBuilderSpy = (): BuilderSpy => {
  const completedTx = makeTxSignBuilderSpy();
  const builder = {
    collectFrom: vi.fn(() => builder),
    readFrom: vi.fn(() => builder),
    mintAssets: vi.fn(() => builder),
    validTo: vi.fn(() => builder),
    validFrom: vi.fn(() => builder),
    addSignerKey: vi.fn(() => builder),
    setMinFee: vi.fn(() => builder),
    compose: vi.fn(() => builder),
    complete: vi.fn(async () => completedTx),
    completeProgram: vi.fn(() => Effect.succeed(completedTx)),
    pay: {
      ToAddressWithData: vi.fn(() => builder),
      ToAddress: vi.fn(() => builder),
      ToContract: vi.fn(() => builder),
    },
    attach: {
      Script: vi.fn(() => builder),
      MintingPolicy: vi.fn(() => builder),
    },
    addSigner: makeUnexpectedMethodSpy("TxBuilder.addSigner"),
    registerStake: makeUnexpectedMethodSpy("TxBuilder.registerStake"),
    deRegisterStake: makeUnexpectedMethodSpy("TxBuilder.deRegisterStake"),
    withdraw: makeUnexpectedMethodSpy("TxBuilder.withdraw"),
    register: {
      Stake: makeUnexpectedMethodSpy("TxBuilder.register.Stake"),
      DRep: makeUnexpectedMethodSpy("TxBuilder.register.DRep"),
    },
    deregister: {
      Stake: makeUnexpectedMethodSpy("TxBuilder.deregister.Stake"),
      DRep: makeUnexpectedMethodSpy("TxBuilder.deregister.DRep"),
    },
    delegateTo: makeUnexpectedMethodSpy("TxBuilder.delegateTo"),
    delegate: {
      ToPool: makeUnexpectedMethodSpy("TxBuilder.delegate.ToPool"),
      VoteToDRep: makeUnexpectedMethodSpy("TxBuilder.delegate.VoteToDRep"),
      VoteToPoolAndDRep: makeUnexpectedMethodSpy(
        "TxBuilder.delegate.VoteToPoolAndDRep",
      ),
    },
    registerAndDelegate: {
      ToPool: makeUnexpectedMethodSpy("TxBuilder.registerAndDelegate.ToPool"),
      ToDRep: makeUnexpectedMethodSpy("TxBuilder.registerAndDelegate.ToDRep"),
      ToPoolAndDRep: makeUnexpectedMethodSpy(
        "TxBuilder.registerAndDelegate.ToPoolAndDRep",
      ),
    },
    updateDRep: makeUnexpectedMethodSpy("TxBuilder.updateDRep"),
    authCommitteeHot: makeUnexpectedMethodSpy("TxBuilder.authCommitteeHot"),
    resignCommitteeHot: makeUnexpectedMethodSpy("TxBuilder.resignCommitteeHot"),
    attachMetadata: makeUnexpectedMethodSpy("TxBuilder.attachMetadata"),
    completeSafe: makeUnexpectedMethodSpy("TxBuilder.completeSafe"),
    chainProgram: makeUnexpectedMethodSpy("TxBuilder.chainProgram"),
    chain: makeUnexpectedMethodSpy("TxBuilder.chain"),
    chainSafe: makeUnexpectedMethodSpy("TxBuilder.chainSafe"),
    config: makeUnexpectedMethodSpy("TxBuilder.config"),
    rawConfig: makeUnexpectedMethodSpy("TxBuilder.rawConfig"),
    lucidConfig: makeUnexpectedMethodSpy("TxBuilder.lucidConfig"),
    getPrograms: makeUnexpectedMethodSpy("TxBuilder.getPrograms"),
    completedTx,
  } satisfies BuilderSpy;

  return builder;
};

export const makeLucidMock = (builder?: BuilderSpy): LucidMock => {
  const selectedBuilder = builder ?? makeBuilderSpy();
  const wallet = {
    overrideUTxOs: vi.fn(),
    address: vi.fn(async () => addressKeyA),
    rewardAddress: vi.fn(async () => null),
    getUtxos: vi.fn(async () => []),
    getUtxosCore: vi.fn(async () => []),
    getDelegation: vi.fn(
      async (): Delegation => ({ poolId: null, rewards: 0n }),
    ),
    signTx: vi.fn(async () => ({})),
    signMessage: vi.fn(async () => ({ signature: hexA, key: pubKeyHashA })),
    submitTx: vi.fn(async () => txHashA),
  } satisfies WalletFixture;

  const lucid = {
    newTx: vi.fn(() => selectedBuilder),
    utxosAt: vi.fn(async () => []),
    utxosAtWithUnit: vi.fn(async () => []),
    wallet: vi.fn(() => wallet),
    config: vi.fn(() => ({
      network: "Preview",
      protocolParameters: { coinsPerUtxoByte: 4_310n },
    })),
    overrideUTxOs: vi.fn(),
    switchProvider: vi.fn(async () => undefined),
    fromTx: vi.fn(() => selectedBuilder.completedTx),
    selectWallet: {
      fromSeed: vi.fn(),
      fromPrivateKey: vi.fn(),
      fromAPI: vi.fn(),
      fromAddress: vi.fn(),
    },
    currentSlot: vi.fn(() => 0),
    unixTimeToSlot: vi.fn(() => 0),
    utxoByUnit: vi.fn(async () =>
      makeUtxo({ txHash: txHashA, outputIndex: 0 }),
    ),
    utxosByOutRef: vi.fn(async () => []),
    delegationAt: vi.fn(
      async (): Delegation => ({ poolId: null, rewards: 0n }),
    ),
    awaitTx: vi.fn(async () => true),
    datumOf: vi.fn(async () => undefined),
    metadataOf: vi.fn(async () => undefined),
  } satisfies LucidMock;

  return lucid;
};

export const makeUtxo = (params: {
  txHash: string;
  outputIndex: number;
  datum?: string;
  unit?: string;
}): UTxO => {
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

export const makeConfirmedStateQueueNode = (
  params: {
    next?: StateQueueDatum["next"];
    confirmedState?: ConfirmedState;
  } = {},
): StateQueueDatum => ({
  key: "Empty",
  next: params.next ?? "Empty",
  data: Data.castTo(
    params.confirmedState ?? confirmedStateFixture,
    ConfirmedState,
  ),
});

export const makeBlockStateQueueNode = (params: {
  key: string;
  next?: StateQueueDatum["next"];
  header?: Header;
}): StateQueueDatum => ({
  key: { Key: { key: params.key } },
  next: params.next ?? "Empty",
  data: Data.castTo(params.header ?? headerFixture, Header),
});

export const makeStateQueueUtxo = (params: {
  txHash: string;
  outputIndex: number;
  datum: StateQueueDatum;
  assetName?: string;
  policyId?: string;
}): StateQueueUTxO => {
  const assetName =
    params.assetName ??
    (params.datum.key === "Empty"
      ? NODE_ASSET_NAME
      : `${NODE_ASSET_NAME}${params.datum.key.Key.key}`);
  const policyId = params.policyId ?? policyIdA;
  const utxo = makeUtxo({
    txHash: params.txHash,
    outputIndex: params.outputIndex,
    datum: Data.to(params.datum, NodeDatum),
    unit: toUnit(policyId, assetName),
  });

  return {
    utxo,
    datum: params.datum,
    assetName,
  };
};

/**
 * Captures messages emitted via `Effect.log*` instead of letting them go to
 * the default console logger, so tests can assert on diagnostic logging.
 */
export const captureLogs = () => {
  const entries: unknown[][] = [];
  const testLogger = Logger.make(({ message }) => {
    entries.push(Array.isArray(message) ? message : [message]);
  });
  return {
    entries,
    layer: Logger.replace(Logger.defaultLogger, testLogger),
  };
};

export const outputReferenceSchema = OutputReference;
export const valueSchema = Value;
export const headerSchema = Header;
export const confirmedStateSchema = ConfirmedState;
