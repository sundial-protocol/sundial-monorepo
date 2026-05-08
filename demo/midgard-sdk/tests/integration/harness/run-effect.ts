import { Context, Layer } from "effect";
import type {
  FakeLucid,
  FakeLucidOptions,
  SubmitRecorder,
} from "./fake-lucid.ts";
import type { TestNodeConfigOptions } from "./node-config-layer.ts";
import { makeFakeLucid } from "./fake-lucid.ts";
import { makeTestSqlLayer } from "./test-sql-layer.ts";
import { makeTestNodeConfigLayer } from "./node-config-layer.ts";

// Local tags used by the SDK integration harness runtime.
// They model the dependencies expected by tests without coupling this package
// to demo/midgard-node internals.
type HarnessGlobals = Record<string, never>;
type HarnessLucid = {
  api: FakeLucid;
  switchToOperatorsMainWallet: () => void;
  switchToOperatorsBlockCommitmentWallet: () => void;
  switchToOperatorsMergingWallet: () => void;
};

const HarnessGlobalsTag =
  Context.GenericTag<HarnessGlobals>("sdk.tests/Globals");
const HarnessLucidTag = Context.GenericTag<HarnessLucid>("sdk.tests/Lucid");

export type SdkIntegrationRuntimeOptions = {
  fakeLucid?: FakeLucidOptions;
  nodeConfig?: TestNodeConfigOptions;
};

export type SdkIntegrationRuntime = {
  /** Composed Effect layer for use with `Effect.provide(...)`. */
  layers: Layer.Layer<unknown>;
  /** The fake Lucid object (also embedded in the layer). */
  lucid: FakeLucid;
  /** Submit recorder for asserting submitted CBOR. */
  submitRecorder: SubmitRecorder;
};

/** Composes SQL + NodeConfig + Globals + fake Lucid harness layers. */
export const makeSdkIntegrationRuntime = (
  opts: SdkIntegrationRuntimeOptions = {},
): SdkIntegrationRuntime => {
  const { lucid, submitRecorder } = makeFakeLucid(opts.fakeLucid);
  const sqlLayer = makeTestSqlLayer();
  const nodeConfigLayer = makeTestNodeConfigLayer(opts.nodeConfig);
  const globalsLayer = Layer.succeed(HarnessGlobalsTag, {});
  const fakeLucidLayer = Layer.succeed(HarnessLucidTag, {
    api: lucid,
    switchToOperatorsMainWallet: () => undefined,
    switchToOperatorsBlockCommitmentWallet: () => undefined,
    switchToOperatorsMergingWallet: () => undefined,
  });

  return {
    layers: Layer.mergeAll(
      sqlLayer,
      nodeConfigLayer,
      globalsLayer,
      fakeLucidLayer,
    ),
    lucid,
    submitRecorder,
  };
};
