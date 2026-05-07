// Fake Lucid boundary for SDK integration tests (fast SDK integration mode).
//
// Provides a deterministic LucidEvolution-compatible object that returns
// fixture UTxOs, network, protocol parameters, and a submit recorder.
// No live Cardano network calls are made.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   const { lucid, submitRecorder } = makeFakeLucid({
//     utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [depositUtxo] },
//     walletUtxos: [FIXTURE_NONCE_UTXO],
//   });
//
//   // In an Effect layer:
//   // TODO (implementation): import Lucid service from @node/services/lucid.ts
//   // const fakeLucidLayer = Layer.succeed(LucidService, { api: lucid });
//
// ─── HTTP boundary mode ───────────────────────────────────────────────────────
//
// For tests that need to assert real HTTP calls to a Cardano provider, use
// withFakeCardanoHttp from ./fake-cardano-http.ts instead.  Point the node
// Lucid config at 127.0.0.1:<randomPort> and assert the observed request.
//
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation): import LucidEvolution from @lucid-evolution/lucid and
// replace the `unknown` type annotations below with the correct interface types.
//
// TODO (implementation): wire into the Effect layer system:
//   import { Lucid } from "@node/services/lucid.ts";
//   import { Layer } from "effect";
//   const fakeLucidLayer = Layer.succeed(Lucid, { api: lucid });

export type FakeLucidOptions = {
  /** UTxOs returned by utxosAt, keyed by address string. */
  utxosAt?: Record<string, unknown[]>;
  /** UTxOs returned by wallet().getUtxos(). */
  walletUtxos?: unknown[];
  /** Network returned by config().network. */
  network?: string;
};

export type SubmitRecorder = {
  /** CBOR hex strings of all submitted transactions. */
  submitted: string[];
  /** Deterministic txid returned for each submission. */
  lastTxId: string;
};

export type FakeLucidResult = {
  /** The fake Lucid object — pass as `lucid.api` when building a layer. */
  lucid: unknown;
  /** Records submitted CBOR payloads for test assertions. */
  submitRecorder: SubmitRecorder;
};

/**
 * Builds a deterministic fake Lucid boundary for SDK integration tests.
 *
 * TODO (implementation):
 *   1. Import LucidEvolution type from @lucid-evolution/lucid.
 *   2. Replace `unknown` return type with `LucidEvolution`.
 *   3. Implement utxosAt to return opts.utxosAt[address] ?? [].
 *   4. Implement wallet().getUtxos() returning opts.walletUtxos ?? [].
 *   5. Implement wallet().signTx(tx) returning tx (identity — no real signing).
 *   6. Implement wallet().submitTx(cbor) recording CBOR and returning a
 *      deterministic txid ("deadbeef".repeat(8)).
 *   7. Implement config() returning { network: opts.network ?? "Preview" }.
 *   8. Implement newTx() returning a builder spy that chains all calls via
 *      method chaining (similar to makeBuilderSpy in sdk-unit.test.ts).
 */
export const makeFakeLucid = (opts: FakeLucidOptions = {}): FakeLucidResult => {
  const submitRecorder: SubmitRecorder = {
    submitted: [],
    lastTxId: "deadbeef".repeat(8),
  };

  // TODO (implementation): replace this stub with the full fake implementation
  // described in the JSDoc above.
  const lucid: unknown = {
    utxosAt: async (address: string) => (opts.utxosAt ?? {})[address] ?? [],
    wallet: () => ({
      getUtxos: async () => opts.walletUtxos ?? [],
      signTx: async (tx: unknown) => tx,
      submitTx: async (cbor: string) => {
        submitRecorder.submitted.push(cbor);
        return submitRecorder.lastTxId;
      },
    }),
    config: () => ({ network: opts.network ?? "Preview" }),
    // TODO (implementation): return a real builder spy with method chaining
    newTx: () => ({
      collectFrom: () => ({}) as unknown,
      mintAssets: () => ({}) as unknown,
      validTo: () => ({}) as unknown,
      pay: {
        ToAddressWithData: () => ({}) as unknown,
        ToAddress: () => ({}) as unknown,
        ToContract: () => ({}) as unknown,
      },
      attach: {
        Script: () => ({}) as unknown,
        MintingPolicy: () => ({}) as unknown,
      },
    }),
  };

  return { lucid, submitRecorder };
};
