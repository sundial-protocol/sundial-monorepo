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
import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { NodeConfig } from "@node/services/config.ts";
import { Layer } from "effect";

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
  opts: TestNodeConfigOptions = {},
): AnyLayer => {
  const ledgerPath =
    opts.ledgerMptPath ??
    path.join(os.tmpdir(), `sdk-int-ledger-${randomUUID()}`);
  const mempoolPath =
    opts.mempoolMptPath ??
    path.join(os.tmpdir(), `sdk-int-mempool-${randomUUID()}`);
  return Layer.succeed(
    NodeConfig,
    NodeConfig.of({
      L1_PROVIDER: "Kupmios",
      L1_BLOCKFROST_API_URL: "http://127.0.0.1:1",
      L1_BLOCKFROST_KEY: "test-key",
      L1_OGMIOS_KEY: "http://127.0.0.1:1",
      L1_KUPO_KEY: "http://127.0.0.1:1",
      L1_OPERATOR_SEED_PHRASE:
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT:
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX:
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      NETWORK: "Preview",
      PROTOCOL_PARAMETERS: {
        event_wait_duration: 50_000,
        maturity_duration: 1n,
        slashing_penalty: 1_000_000n,
      },
      PORT: 3000,
      WAIT_BETWEEN_BLOCK_COMMITMENTS: 1000,
      WAIT_BETWEEN_BLOCK_SUBMISSIONS: 1000,
      WAIT_BETWEEN_USER_EVENT_FETCHES: 1000,
      WAIT_BETWEEN_MERGE_TXS: 1000,
      PROM_METRICS_PORT: 9464,
      OLTP_EXPORTER_URL: "http://127.0.0.1:4318/v1/traces",
      POSTGRES_USER: "postgres",
      POSTGRES_PASSWORD: "postgres",
      POSTGRES_DB: "midgard",
      POSTGRES_HOST: "localhost",
      LEDGER_MPT_DB_PATH: ledgerPath,
      MEMPOOL_MPT_DB_PATH: mempoolPath,
      GENESIS_UTXOS: [],
    }),
  );
};
