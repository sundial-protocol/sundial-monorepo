// Shared Effect runtime composer for SDK integration tests.
//
// makeSdkIntegrationRuntime composes all layers needed by the full SDK
// integration suite: PGlite SQL, NodeConfig, Globals, and fake Lucid.
// Individual tests that need only a subset can compose layers directly.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   const { layers, lucid, submitRecorder, cleanup } =
//     makeSdkIntegrationRuntime({ ledgerMptPath, mempoolMptPath });
//
//   yield* DBInitialization.program.pipe(Effect.provide(layers));
//   yield* someNodeAction.pipe(Effect.provide(layers));
//
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation):
//   import { makeTestSqlLayer } from "./pglite-sql-layer.ts";
//   import { makeTestNodeConfigLayer } from "./node-config-layer.ts";
//   import { makeFakeLucid } from "./fake-lucid.ts";
//   import { Globals } from "@node/services/globals.ts";
//   import { Lucid } from "@node/services/lucid.ts";
//   import { Layer } from "effect";
//
//   export const makeSdkIntegrationRuntime = (opts = {}) => {
//     const { lucid, submitRecorder } = makeFakeLucid(opts.fakeLucid);
//     const sqlLayer = makeTestSqlLayer();
//     const nodeConfigLayer = makeTestNodeConfigLayer(opts.nodeConfig);
//     const globalsLayer = Globals.Default;
//     const fakeLucidLayer = Layer.succeed(Lucid, { api: lucid });
//     const layers = Layer.mergeAll(
//       sqlLayer, nodeConfigLayer, globalsLayer, fakeLucidLayer
//     );
//     return { layers, lucid, submitRecorder };
//   };
//
// Each test file composes the subset of layers it needs.  Not every test needs
// the full runtime (e.g., pure SDK encoding tests need no SQL or Lucid layers).

import type { FakeLucidOptions, SubmitRecorder } from "./fake-lucid.ts";
import type { TestNodeConfigOptions } from "./node-config-layer.ts";

export type AnyLayer = any;

export type SdkIntegrationRuntimeOptions = {
  fakeLucid?: FakeLucidOptions;
  nodeConfig?: TestNodeConfigOptions;
};

export type SdkIntegrationRuntime = {
  /** Composed Effect layer for use with Effect.provide. */
  layers: AnyLayer;
  /** The fake Lucid object (also embedded in the layer). */
  lucid: unknown;
  /** Submit recorder for asserting submitted CBOR. */
  submitRecorder: SubmitRecorder;
};

/**
 * Composes the full integration layer stack: PGlite SQL + NodeConfig +
 * Globals + fake Lucid.
 *
 * TODO (implementation): implement as described in the module JSDoc above.
 * This is the primary entry point for most SDK integration tests.
 */
export const makeSdkIntegrationRuntime = (
  _opts: SdkIntegrationRuntimeOptions = {},
): SdkIntegrationRuntime => {
  // TODO (implementation): compose real layers and return them.
  throw new Error(
    "makeSdkIntegrationRuntime: not yet implemented — implement layer composition after installing PGlite and wiring NodeConfig",
  );
};
