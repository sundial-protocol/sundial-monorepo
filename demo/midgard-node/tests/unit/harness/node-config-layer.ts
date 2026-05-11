import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { NodeConfig } from "@/services/config.js";
import { Layer } from "effect";

export const makeTestNodeConfigLayer = () =>
  Layer.succeed(
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
      LEDGER_MPT_DB_PATH: path.join(os.tmpdir(), `unit-ledger-${randomUUID()}`),
      MEMPOOL_MPT_DB_PATH: path.join(
        os.tmpdir(),
        `unit-mempool-${randomUUID()}`,
      ),
      GENESIS_UTXOS: [],
    }),
  );
