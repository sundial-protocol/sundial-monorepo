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
      SUBMIT_SIGNED_TX_TIMEOUT_MS: 30_000,
      SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES: 1,
      WAIT_BETWEEN_USER_EVENT_FETCHES: 1000,
      WAIT_BETWEEN_MERGE_TXS: 1000,
      COMMITMENT_WORKER_TIMEOUT_MS: 30_000,
      COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG: 0,
      COMMITMENT_WINDOW_WARN_TX_REQUESTS: 50_000,
      COMMITMENT_WINDOW_WARN_TOTAL_EVENTS: 60_000,
      COMMITMENT_WINDOW_WARN_TOTAL_BYTES: 20_000_000,
      COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK: 1,
      COMMITMENT_MAX_WAIT_MS: 0,
      COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK: 2_000,
      TX_QUEUE_DRAIN_BATCH_SIZE: 100,
      TX_QUEUE_CONSUMER_WORKER_COUNT: 1,
      TX_QUEUE_PROCESSOR_INTERVAL_MS: 250,
      TX_QUEUE_CLAIM_IDLE_MS: 30_000,
      TX_QUEUE_CLAIM_BATCH_SIZE: 100,
      TX_QUEUE_MAX_DELIVERY_ATTEMPTS: 5,
      TX_QUEUE_DEAD_LETTER_STREAM: "midgard:tx-submissions:dead-letter",
      TX_PARSE_CONCURRENCY: 8,
      NODE_ROLE: "all",
      REDIS_URL: "redis://127.0.0.1:6379",
      REDIS_STREAM_KEY: "midgard:tx-submissions",
      REDIS_STREAM_CONSUMER_GROUP: "midgard-tx-processors",
      REDIS_STREAM_CONSUMER_NAME: "unit-test-consumer",
      REDIS_STREAM_BLOCK_MS: 1000,
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
      LUCID_INIT_MAX_RETRIES: 0,
      GENESIS_UTXOS: [],
      FAUCET_ENABLED: false,
      FAUCET_SEED_PHRASE: "",
      FAUCET_API_KEY: "",
      FAUCET_ADDRESS: "",
      FAUCET_AMOUNT_LOVELACE: 100_000_000n,
      FAUCET_COOLDOWN_SECONDS: 86_400,
      FAUCET_DAILY_IP_LIMIT: 5,
      FAUCET_MIN_BALANCE_LOVELACE: 100_000_000n,
    }),
  );
