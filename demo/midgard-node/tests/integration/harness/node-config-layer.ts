// NodeConfig Effect layer factory for node integration tests.
//
// Returns a Layer.succeed(NodeConfig, ...) with test-safe defaults and
// unique per-test LevelDB paths so MPT state does not bleed between tests.
//
// Usage:
//   const nodeConfigLayer = makeTestNodeConfigLayer();
//   const layers = Layer.mergeAll(sqlLayer, nodeConfigLayer, ...);

import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { NodeConfig } from "@/services/config.js";
import { Layer } from "effect";

export type TestNodeConfigOptions = {
  ledgerMptPath?: string;
  mempoolMptPath?: string;
  genesisUtxos?: NodeConfig["Type"]["GENESIS_UTXOS"];
};

export const makeTestNodeConfigLayer = (opts: TestNodeConfigOptions = {}) => {
  const ledgerPath =
    opts.ledgerMptPath ?? path.join(os.tmpdir(), `nit-ledger-${randomUUID()}`);
  const mempoolPath =
    opts.mempoolMptPath ??
    path.join(os.tmpdir(), `nit-mempool-${randomUUID()}`);
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
      GENESIS_UTXOS: opts.genesisUtxos ?? [],
    }),
  );
};
