// Shared Effect runtime composer for node integration tests.
//
// Composes PGlite SQL, NodeConfig, and Globals into a single layer stack
// suitable for most integration tests.
//
// Usage:
//   const { layers, cleanup } = makeNodeIntegrationRuntime();
//   yield* DBInitialization.program.pipe(Effect.provide(layers));

import { makeTestSqlLayer } from "./pglite-sql-layer.js";
import {
  makeTestNodeConfigLayer,
  type TestNodeConfigOptions,
} from "./node-config-layer.js";
import { Globals } from "@/services/globals.js";
import { Layer } from "effect";

export type NodeIntegrationRuntimeOptions = {
  nodeConfig?: TestNodeConfigOptions;
};

export const makeNodeIntegrationLayers = (
  opts: NodeIntegrationRuntimeOptions = {},
) => {
  const sqlLayer = makeTestSqlLayer();
  const nodeConfigLayer = makeTestNodeConfigLayer(opts.nodeConfig);
  const globalsLayer = Globals.Default;
  return Layer.mergeAll(sqlLayer, nodeConfigLayer, globalsLayer);
};
