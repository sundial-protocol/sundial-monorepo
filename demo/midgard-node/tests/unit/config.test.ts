import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { ConfigProvider, Effect, Layer } from "effect";

vi.mock("@lucid-evolution/lucid", () => ({
  walletFromSeed: vi.fn(() => ({ address: "addr_test1mock" })),
}));

import { NodeConfig, ConfigError } from "@/services/config.js";

const testConfigProvider = ConfigProvider.fromMap(
  new Map([
    ["L1_PROVIDER", "Kupmios"],
    ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
    ["L1_BLOCKFROST_KEY", "blockfrost-key"],
    ["L1_OGMIOS_KEY", "ogmios-key"],
    ["L1_KUPO_KEY", "kupo-key"],
    ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
    [
      "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
      "seed phrase block commitment",
    ],
    ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
    ["NETWORK", "Preview"],
    ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
    ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
    ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
  ]),
);

const configLayer = NodeConfig.layer.pipe(
  Layer.provide(Layer.setConfigProvider(testConfigProvider)),
);

describe("NodeConfig", () => {
  it.effect(
    "resolves with valid env vars",
    () =>
      Effect.gen(function* () {
        const config = yield* NodeConfig;
        expect(config.L1_PROVIDER).toBe("Kupmios");
        expect(config.NETWORK).toBe("Preview");
        expect(config.PORT).toBe(3000);
        expect(config.L1_BLOCKFROST_API_URL).toBe("http://localhost:1337");
      }).pipe(Effect.provide(configLayer)),
    { timeout: 10000 },
  );

  it.effect(
    "sets default values when optional vars are absent",
    () =>
      Effect.gen(function* () {
        const config = yield* NodeConfig;
        expect(config.PROM_METRICS_PORT).toBe(9464);
        expect(config.POSTGRES_USER).toBe("postgres");
        expect(config.POSTGRES_DB).toBe("midgard");
        expect(config.REDIS_URL).toBe("redis://redis:6379");
        expect(config.REDIS_STREAM_KEY).toBe("midgard:tx-submissions");
        expect(config.NODE_ROLE).toBe("all");
        expect(config.TX_QUEUE_DRAIN_BATCH_SIZE).toBe(100);
        expect(config.TX_QUEUE_PROCESSOR_INTERVAL_MS).toBe(250);
        expect(config.TX_PARSE_CONCURRENCY).toBe(8);
        expect(config.SUBMIT_SIGNED_TX_TIMEOUT_MS).toBe(30_000);
        expect(config.SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES).toBe(1);
        expect(config.COMMITMENT_WORKER_TIMEOUT_MS).toBe(300_000);
        expect(config.COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG).toBe(0);
        expect(config.COMMITMENT_WINDOW_WARN_TX_REQUESTS).toBe(50_000);
        expect(config.COMMITMENT_WINDOW_WARN_TOTAL_EVENTS).toBe(60_000);
        expect(config.COMMITMENT_WINDOW_WARN_TOTAL_BYTES).toBe(20_000_000);
        expect(config.COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK).toBe(1);
        expect(config.COMMITMENT_MAX_WAIT_MS).toBe(0);
        expect(config.COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK).toBe(2_000);
        expect(config.FAUCET_ENABLED).toBe(false);
        expect(config.FAUCET_SEED_PHRASE).toBe("");
        expect(config.FAUCET_API_KEY).toBe("");
        expect(config.FAUCET_ADDRESS).toBe("");
        expect(config.FAUCET_AMOUNT_LOVELACE).toBe(100_000_000n);
        expect(config.FAUCET_COOLDOWN_SECONDS).toBe(86_400);
        expect(config.FAUCET_DAILY_IP_LIMIT).toBe(5);
        expect(config.FAUCET_MIN_BALANCE_LOVELACE).toBe(100_000_000n);
        // The faucet wallet must not be funded while disabled.
        expect(
          config.GENESIS_UTXOS.some(
            (u) => u.assets.lovelace === 10_000_000_000_000n,
          ),
        ).toBe(false);
      }).pipe(Effect.provide(configLayer)),
    { timeout: 10000 },
  );

  it.effect(
    "funds a dedicated faucet genesis UTxO when FAUCET_ENABLED",
    () => {
      const faucetProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["FAUCET_ENABLED", "true"],
          ["FAUCET_SEED_PHRASE", "dedicated faucet seed phrase"],
          ["FAUCET_API_KEY", "faucet-api-key"],
          ["FAUCET_AMOUNT_LOVELACE", "100000000"],
          ["FAUCET_GENESIS_ALLOCATION_LOVELACE", "10000000000000"],
        ]),
      );
      const faucetLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(faucetProvider)),
      );
      return Effect.gen(function* () {
        const config = yield* NodeConfig;
        expect(config.FAUCET_ENABLED).toBe(true);
        expect(config.FAUCET_ADDRESS).toBe("addr_test1mock");
        const faucetUtxo = config.GENESIS_UTXOS.find(
          (u) => u.assets.lovelace === 10_000_000_000_000n,
        );
        expect(faucetUtxo).toBeDefined();
        expect(faucetUtxo?.address).toBe("addr_test1mock");
      }).pipe(Effect.provide(faucetLayer));
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when FAUCET_SEED_PHRASE reuses a genesis seed",
    () => {
      const reusedProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["FAUCET_ENABLED", "true"],
          ["FAUCET_SEED_PHRASE", "seed phrase a"],
          ["FAUCET_API_KEY", "faucet-api-key"],
        ]),
      );
      const reusedLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(reusedProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(reusedLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain("FAUCET_SEED_PHRASE");
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails closed when FAUCET_ENABLED in production without a seed",
    () => {
      const productionProvider = ConfigProvider.fromMap(
        new Map([
          ["NODE_ENV", "production"],
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["REDIS_URL", "redis://redis:6379"],
          ["POSTGRES_HOST", "postgres"],
          ["POSTGRES_PASSWORD", "postgres"],
          ["POSTGRES_DB", "midgard"],
          ["POSTGRES_USER", "postgres"],
          ["LEDGER_MPT_DB_PATH", "ledger"],
          ["MEMPOOL_MPT_DB_PATH", "mempool"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["FAUCET_ENABLED", "true"],
        ]),
      );
      const productionLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(productionProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(productionLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain("FAUCET_SEED_PHRASE");
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "resolves explicit high-throughput tx queue processor settings",
    () => {
      const highThroughputProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["TX_QUEUE_DRAIN_BATCH_SIZE", "500"],
          ["TX_QUEUE_PROCESSOR_INTERVAL_MS", "100"],
          ["TX_PARSE_CONCURRENCY", "8"],
        ]),
      );
      const highThroughputLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(highThroughputProvider)),
      );

      return Effect.gen(function* () {
        const config = yield* NodeConfig;

        expect(config.TX_QUEUE_DRAIN_BATCH_SIZE).toBe(500);
        expect(config.TX_QUEUE_PROCESSOR_INTERVAL_MS).toBe(100);
        expect(config.TX_PARSE_CONCURRENCY).toBe(8);
      }).pipe(Effect.provide(highThroughputLayer));
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails closed when production deployment config is absent",
    () => {
      const productionProvider = ConfigProvider.fromMap(
        new Map([
          ["NODE_ENV", "production"],
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
        ]),
      );
      const productionLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(productionProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(productionLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain("REDIS_URL");
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "returns empty genesis UTxOs for Mainnet",
    () => {
      const mainnetProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Blockfrost"],
          [
            "L1_BLOCKFROST_API_URL",
            "https://cardano-mainnet.blockfrost.io/api/v0",
          ],
          ["L1_BLOCKFROST_KEY", "key"],
          ["L1_OGMIOS_KEY", "ogmios"],
          ["L1_KUPO_KEY", "kupo"],
          ["L1_OPERATOR_SEED_PHRASE", "seed"],
          ["L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT", "seed bc"],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed merge"],
          ["NETWORK", "Mainnet"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed c"],
        ]),
      );
      const mainnetLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(mainnetProvider)),
      );
      return Effect.gen(function* () {
        const config = yield* NodeConfig;
        expect(config.GENESIS_UTXOS).toEqual([]);
        expect(config.L1_PROVIDER).toBe("Blockfrost");
      }).pipe(Effect.provide(mainnetLayer));
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES is negative",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES", "-1"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain(
            "SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES",
          );
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when TX_QUEUE_DRAIN_BATCH_SIZE is not positive",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["TX_QUEUE_DRAIN_BATCH_SIZE", "0"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain("TX_QUEUE_DRAIN_BATCH_SIZE");
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when TX_QUEUE_PROCESSOR_INTERVAL_MS is not positive",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["TX_QUEUE_PROCESSOR_INTERVAL_MS", "0"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain(
            "TX_QUEUE_PROCESSOR_INTERVAL_MS",
          );
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when COMMITMENT_MAX_WAIT_MS is negative",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["COMMITMENT_MAX_WAIT_MS", "-1"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain("COMMITMENT_MAX_WAIT_MS");
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG is negative",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG", "-1"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain(
            "COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG",
          );
        }
      });
    },
    { timeout: 10000 },
  );

  it.effect(
    "fails when COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK exceeds COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK",
    () => {
      const invalidProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Kupmios"],
          ["L1_BLOCKFROST_API_URL", "http://localhost:1337"],
          ["L1_BLOCKFROST_KEY", "blockfrost-key"],
          ["L1_OGMIOS_KEY", "ogmios-key"],
          ["L1_KUPO_KEY", "kupo-key"],
          ["L1_OPERATOR_SEED_PHRASE", "seed phrase operator"],
          [
            "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT",
            "seed phrase block commitment",
          ],
          ["L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", "seed phrase merge tx"],
          ["NETWORK", "Preview"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_A", "seed phrase a"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_B", "seed phrase b"],
          ["TESTNET_GENESIS_WALLET_SEED_PHRASE_C", "seed phrase c"],
          ["COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK", "2001"],
          ["COMMITMENT_MAX_TX_REQUESTS_PER_BLOCK", "2000"],
        ]),
      );
      const invalidLayer = NodeConfig.layer.pipe(
        Layer.provide(Layer.setConfigProvider(invalidProvider)),
      );
      return Effect.gen(function* () {
        const result = yield* Effect.either(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(invalidLayer)),
        );
        expect(result._tag).toBe("Left");
        if (result._tag === "Left") {
          expect(result.left).toBeInstanceOf(ConfigError);
          expect(result.left.message).toContain(
            "COMMITMENT_MIN_TX_REQUESTS_PER_BLOCK",
          );
        }
      });
    },
    { timeout: 10000 },
  );
});

describe("ConfigError", () => {
  it("has expected tag", () => {
    const err = new ConfigError({
      message: "bad config",
      cause: undefined,
      fieldsAndValues: [["key", "val"]],
    });
    expect(err._tag).toBe("ConfigError");
    expect(err.message).toBe("bad config");
  });
});
