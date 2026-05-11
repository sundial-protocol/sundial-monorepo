import { credentialToAddress } from "@lucid-evolution/lucid";

// Fake Lucid boundary for SDK integration tests (fast integration mode).
//
// This harness returns deterministic fixture UTxOs and tx submission behavior.
// No live Cardano network calls are made.

export type FakeLucidOptions = {
  /** UTxOs returned by `utxosAt`, keyed by address string. */
  utxosAt?: Record<string, unknown[]>;
  /** UTxOs returned by `wallet().getUtxos()`. */
  walletUtxos?: unknown[];
  /** Network returned by `config().network`. */
  network?: string;
};

export type SubmitRecorder = {
  /** CBOR hex strings of all submitted transactions. */
  submitted: string[];
  /** Deterministic txid returned for each submission. */
  lastTxId: string;
};

type BuilderCalls = {
  collectFrom: unknown[][];
  mintAssets: unknown[][];
  validTo: unknown[][];
  payToAddressWithData: unknown[][];
  payToAddress: unknown[][];
  payToContract: unknown[][];
  attachScript: unknown[][];
  attachMintingPolicy: unknown[][];
  compose: unknown[][];
  complete: unknown[][];
};

type FakeBuilder = {
  collectFrom: (...args: unknown[]) => FakeBuilder;
  mintAssets: (...args: unknown[]) => FakeBuilder;
  validTo: (...args: unknown[]) => FakeBuilder;
  pay: {
    ToAddressWithData: (...args: unknown[]) => FakeBuilder;
    ToAddress: (...args: unknown[]) => FakeBuilder;
    ToContract: (...args: unknown[]) => FakeBuilder;
  };
  attach: {
    Script: (...args: unknown[]) => FakeBuilder;
    MintingPolicy: (...args: unknown[]) => FakeBuilder;
  };
  compose: (...args: unknown[]) => FakeBuilder;
  complete: (...args: unknown[]) => Promise<FakeBuilder>;
  completeProgram: () => Promise<FakeBuilder>;
  __calls: BuilderCalls;
};

export type FakeLucid = {
  utxosAt: (address: string) => Promise<unknown[]>;
  wallet: () => {
    getUtxos: () => Promise<unknown[]>;
    signTx: (tx: unknown) => Promise<unknown>;
    address: () => Promise<string>;
    submitTx: (cbor: string) => Promise<string>;
  };
  config: () => { network: string };
  newTx: () => FakeBuilder;
};

export type FakeLucidResult = {
  /** The fake Lucid object. */
  lucid: FakeLucid;
  /** Records submitted CBOR payloads for test assertions. */
  submitRecorder: SubmitRecorder;
};

/**
 * Builds a deterministic fake Lucid boundary for SDK integration tests.
 */
export const makeFakeLucid = (opts: FakeLucidOptions = {}): FakeLucidResult => {
  const submitRecorder: SubmitRecorder = {
    submitted: [],
    lastTxId: "deadbeef".repeat(8),
  };

  const makeBuilderSpy = (): FakeBuilder => {
    const calls: BuilderCalls = {
      collectFrom: [] as unknown[][],
      mintAssets: [] as unknown[][],
      validTo: [] as unknown[][],
      payToAddressWithData: [] as unknown[][],
      payToAddress: [] as unknown[][],
      payToContract: [] as unknown[][],
      attachScript: [] as unknown[][],
      attachMintingPolicy: [] as unknown[][],
      compose: [] as unknown[][],
      complete: [] as unknown[][],
    };
    const tx = {} as FakeBuilder;
    tx.collectFrom = (...args: unknown[]) => {
      calls.collectFrom.push(args);
      return tx;
    };
    tx.mintAssets = (...args: unknown[]) => {
      calls.mintAssets.push(args);
      return tx;
    };
    tx.validTo = (...args: unknown[]) => {
      calls.validTo.push(args);
      return tx;
    };
    tx.pay = {
      ToAddressWithData: (...args: unknown[]) => {
        calls.payToAddressWithData.push(args);
        return tx;
      },
      ToAddress: (...args: unknown[]) => {
        calls.payToAddress.push(args);
        return tx;
      },
      ToContract: (...args: unknown[]) => {
        calls.payToContract.push(args);
        return tx;
      },
    };
    tx.attach = {
      Script: (...args: unknown[]) => {
        calls.attachScript.push(args);
        return tx;
      },
      MintingPolicy: (...args: unknown[]) => {
        calls.attachMintingPolicy.push(args);
        return tx;
      },
    };
    tx.compose = (...args: unknown[]) => {
      calls.compose.push(args);
      return tx;
    };
    tx.complete = async (...args: unknown[]) => {
      calls.complete.push(args);
      return tx;
    };
    tx.completeProgram = async () => tx;
    tx.__calls = calls;
    return tx;
  };

  const lucid: FakeLucid = {
    utxosAt: async (address: string) => (opts.utxosAt ?? {})[address] ?? [],
    wallet: () => ({
      getUtxos: async () => opts.walletUtxos ?? [],
      signTx: async (tx: unknown) => tx,
      address: async () =>
        credentialToAddress("Preview", {
          type: "Key",
          hash: "dd".repeat(28),
        }),
      submitTx: async (cbor: string) => {
        submitRecorder.submitted.push(cbor);
        return submitRecorder.lastTxId;
      },
    }),
    config: () => ({ network: opts.network ?? "Preview" }),
    newTx: makeBuilderSpy,
  };

  return { lucid, submitRecorder };
};
