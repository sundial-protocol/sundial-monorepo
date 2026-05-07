import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { Context, Layer } from "effect";

// Local NodeConfig-shaped test contract used by SDK integration harnesses.
// This keeps the SDK package decoupled from the midgard-node runtime internals.
export type TestNodeConfig = {
  L1_PROVIDER: string;
  L1_BLOCKFROST_API_URL: string;
  L1_BLOCKFROST_KEY: string;
  L1_OGMIOS_KEY: string;
  L1_KUPO_KEY: string;
  L1_OPERATOR_SEED_PHRASE: string;
  L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT: string;
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  NETWORK: "Preview";
  PROTOCOL_PARAMETERS: {
    event_wait_duration: number;
    maturity_duration: bigint;
    slashing_penalty: bigint;
  };
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENTS: number;
  WAIT_BETWEEN_BLOCK_SUBMISSIONS: number;
  WAIT_BETWEEN_USER_EVENT_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  LEDGER_MPT_DB_PATH: string;
  MEMPOOL_MPT_DB_PATH: string;
  GENESIS_UTXOS: readonly unknown[];
};

export const TestNodeConfigTag = Context.GenericTag<TestNodeConfig>(
  "sdk.tests/NodeConfig",
);

export type TestNodeConfigOptions = {
  /** Absolute path for the ledger MPT LevelDB. Defaults to a unique temp path. */
  ledgerMptPath?: string;
  /** Absolute path for the mempool MPT LevelDB. Defaults to a unique temp path. */
  mempoolMptPath?: string;
};

/** Returns a NodeConfig-shaped Effect layer with test-safe defaults. */
export const makeTestNodeConfigLayer = (
  opts: TestNodeConfigOptions = {},
): Layer.Layer<TestNodeConfig> => {
  const ledgerPath =
    opts.ledgerMptPath ??
    path.join(os.tmpdir(), `sdk-int-ledger-${randomUUID()}`);
  const mempoolPath =
    opts.mempoolMptPath ??
    path.join(os.tmpdir(), `sdk-int-mempool-${randomUUID()}`);

  const config: TestNodeConfig = {
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
  };

  return Layer.succeed(TestNodeConfigTag, config);
};
