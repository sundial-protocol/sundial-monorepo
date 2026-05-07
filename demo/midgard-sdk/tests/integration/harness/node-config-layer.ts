// NodeConfig Effect layer factory for SDK integration tests.
//
// Returns a Layer.succeed(NodeConfig, ...) with sensible test defaults and
// unique-per-test LevelDB paths so MPT state does not bleed between tests.
//
// ─── Usage ────────────────────────────────────────────────────────────────────
//
//   const nodeConfigLayer = makeTestNodeConfigLayer({
//     ledgerMptPath: tmpLedgerPath,
//     mempoolMptPath: tmpMempoolPath,
//   });
//   const layers = Layer.mergeAll(sqlLayer, nodeConfigLayer, ...);
//
// ─── Implementation notes ─────────────────────────────────────────────────────
//
// TODO (implementation):
//   import * as os from "node:os";
//   import * as path from "node:path";
//   import { randomUUID } from "node:crypto";
//   import { NodeConfig } from "@node/services/config.ts";
//   import { Layer } from "effect";
//
//   export type TestNodeConfigOptions = {
//     ledgerMptPath?: string;
//     mempoolMptPath?: string;
//   };
//
//   export const makeTestNodeConfigLayer = (opts: TestNodeConfigOptions = {}) => {
//     const ledger = opts.ledgerMptPath ??
//       path.join(os.tmpdir(), `sdk-int-ledger-${randomUUID()}`);
//     const mempool = opts.mempoolMptPath ??
//       path.join(os.tmpdir(), `sdk-int-mempool-${randomUUID()}`);
//     return Layer.succeed(NodeConfig, NodeConfig.of({
//       LEDGER_MPT_DB_PATH: ledger,
//       MEMPOOL_MPT_DB_PATH: mempool,
//       GENESIS_UTXOS: [],
//       // Fill remaining NodeConfig fields with test-safe defaults.
//       DATABASE_URL: "memory://", // overridden by PGlite layer
//       NETWORK: "Preview",
//       // ...add remaining required fields as NodeConfig interface evolves
//     }));
//   };
//
// Call makeTestNodeConfigLayer() once per test file in a beforeAll, or once
// per test when each test needs an isolated MPT path.

export type AnyLayer = any;

export type TestNodeConfigOptions = {
  /** Absolute path for the ledger MPT LevelDB.  Defaults to a unique temp path. */
  ledgerMptPath?: string;
  /** Absolute path for the mempool MPT LevelDB.  Defaults to a unique temp path. */
  mempoolMptPath?: string;
};

/**
 * Returns a NodeConfig Effect layer with test-safe defaults and unique MPT paths.
 *
 * TODO (implementation): implement as described in the module JSDoc above.
 * Import NodeConfig from @node/services/config.ts and use Layer.succeed.
 */
export const makeTestNodeConfigLayer = (
  _opts: TestNodeConfigOptions = {},
): AnyLayer => {
  // TODO (implementation): return Layer.succeed(NodeConfig, NodeConfig.of({ ... }))
  throw new Error(
    "makeTestNodeConfigLayer: not yet implemented — import NodeConfig from @node/services/config.ts",
  );
};
