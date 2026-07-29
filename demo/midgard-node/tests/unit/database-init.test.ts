import { expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { Layer } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

// createTable in addressHistory/blocks/blocksTxs is an Effect property, not a function
vi.mock("@/database/addressHistory.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    createTable: E.succeed(undefined),
    tableName: "address_history",
    Columns: {},
    Status: {},
    EventType: {},
  };
});
vi.mock("@/database/blocks.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    createTable: E.succeed(undefined),
    tableName: "blocks",
  };
});
vi.mock("@/database/blocksTxs.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    createTable: E.succeed(undefined),
    tableName: "blocks_txs",
    Columns: {},
    ColumnsIndices: {},
  };
});
vi.mock("@/database/confirmedLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "confirmed_ledger",
    insertMultiple: vi.fn(() => E.succeed(undefined)),
    retrieve: E.succeed([]),
    clearUTxOs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});
vi.mock("@/database/latestLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "latest_ledger",
    insertMultiple: vi.fn(() => E.succeed(undefined)),
    retrieveByOutRef: vi.fn(() => E.succeed(undefined)),
    retrieveEntries: vi.fn(() => E.succeed([])),
    retrieve: E.succeed([]),
    retrieveNoTimeStamps: E.succeed([]),
    clearUTxOs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});
vi.mock("@/database/mempoolLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "mempool_ledger",
    insert: vi.fn(() => E.succeed(undefined)),
    retrieve: E.succeed([]),
    retrieveByAddress: vi.fn(() => E.succeed([])),
    retrieveByOutRefs: vi.fn(() => E.succeed([])),
    retrieveByOutRef: vi.fn(() => E.succeed(undefined)),
    clearUTxOs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});
vi.mock("@/database/immutable.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "immutable",
    insertTx: vi.fn(() => E.succeed(undefined)),
    insertTxs: vi.fn(() => E.succeed(undefined)),
    retrieve: E.succeed([]),
    retrieveTxCborByHash: vi.fn(() => E.succeed(undefined)),
    retrieveTxCborsByHashes: vi.fn(() => E.succeed([])),
    clearTxs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});
vi.mock("@/database/mempool.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "mempool",
    ensureNormalizedColumns: E.succeed(undefined),
    insertMultiple: vi.fn(() => E.succeed(undefined)),
    retrieveTxCborByHash: vi.fn(() => E.succeed(undefined)),
    retrieveTxCborsByHashes: vi.fn(() => E.succeed([])),
    retrieve: E.succeed([]),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    retrieveTxCount: E.succeed(0n),
    clearTxs: vi.fn(() => E.succeed(undefined)),
    clear: E.succeed(undefined),
  };
});
vi.mock("@/database/deposits.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "deposits_utxos",
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    delEntries: vi.fn(() => E.succeed(undefined)),
  };
});
vi.mock("@/database/txOrders.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "transaction_order_utxos",
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
  };
});
vi.mock("@/database/withdrawals.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    tableName: "withdrawal_order_utxos",
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
  };
});
vi.mock("@/database/utils/ledger.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: {
      TX_ID: "tx_id",
      OUTREF: "outref",
      OUTPUT: "output",
      ADDRESS: "address",
      TIMESTAMPTZ: "time_stamp_tz",
    },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    retrieveByOutRef: vi.fn(() => E.succeed(undefined)),
    retrieveByOutRefs: vi.fn(() => E.succeed([])),
    retrieveEntriesWithAddress: vi.fn(() => E.succeed([])),
    delEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntriesNoTimeStamps: vi.fn(() => E.succeed([])),
  };
});
vi.mock("@/database/utils/tx.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: { TX_ID: "tx_id", TX: "tx", TIMESTAMPTZ: "time_stamp_tz" },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    retrieveValue: vi.fn(() => E.succeed(undefined)),
    retrieveValues: vi.fn(() => E.succeed([])),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    delMultiple: vi.fn(() => E.succeed(undefined)),
  };
});
vi.mock("@/database/utils/user-events.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: {
      ID: "event_id",
      INFO: "event_info",
      ASSET_NAME: "asset_name",
      L1_UTXO_CBOR: "l1_utxo_cbor",
      INCLUSION_TIME: "inclusion_time",
    },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    delEntries: vi.fn(() => E.succeed(undefined)),
  };
});

import * as DBInitialization from "@/database/init.js";
import { NodeConfig } from "@/services/config.js";

const sqlHarness = createMockSqlHarness();
const mockNodeConfigLayer = Layer.succeed(
  NodeConfig,
  NodeConfig.of({
    L1_PROVIDER: "Kupmios",
    L1_BLOCKFROST_API_URL: "",
    L1_BLOCKFROST_KEY: "",
    L1_OGMIOS_KEY: "",
    L1_KUPO_KEY: "",
    L1_OPERATOR_SEED_PHRASE: "",
    L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT: "",
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
    NETWORK: "Preview",
    PROTOCOL_PARAMETERS: SDK.getProtocolParameters("Preview"),
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
    REDIS_STREAM_CONSUMER_NAME: "database-init-test-consumer",
    REDIS_STREAM_BLOCK_MS: 1000,
    PROM_METRICS_PORT: 9464,
    OLTP_EXPORTER_URL: "http://localhost:4318/v1/traces",
    POSTGRES_USER: "postgres",
    POSTGRES_PASSWORD: "postgres",
    POSTGRES_DB: "midgard",
    POSTGRES_HOST: "localhost",
    LEDGER_MPT_DB_PATH: "midgard-ledger-mpt-db",
    MEMPOOL_MPT_DB_PATH: "midgard-mempool-mpt-db",
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

it.effect("Database initialization creates schema", () =>
  DBInitialization.program.pipe(
    Effect.map(() => expect(sqlHarness.getCallCount()).toBeGreaterThan(0)),
    Effect.provide(sqlHarness.layer),
    Effect.provide(mockNodeConfigLayer),
  ),
);
