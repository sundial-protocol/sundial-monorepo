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
        expect(config.TX_QUEUE_CAPACITY).toBe(250_000);
        expect(config.TX_QUEUE_MAX_PENDING).toBe(20_000);
        expect(config.TX_QUEUE_DRAIN_BATCH_SIZE).toBe(250);
        expect(config.TX_QUEUE_OFFER_TIMEOUT_MS).toBe(100);
        expect(config.SUBMIT_SIGNED_TX_TIMEOUT_MS).toBe(30_000);
        expect(config.SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES).toBe(1);
        expect(config.COMMITMENT_WORKER_TIMEOUT_MS).toBe(300_000);
        expect(config.COMMITMENT_MAX_UNSUBMITTED_BLOCK_BACKLOG).toBe(0);
        expect(config.COMMITMENT_WINDOW_WARN_TX_REQUESTS).toBe(50_000);
        expect(config.COMMITMENT_WINDOW_WARN_TOTAL_EVENTS).toBe(60_000);
        expect(config.COMMITMENT_WINDOW_WARN_TOTAL_BYTES).toBe(20_000_000);
      }).pipe(Effect.provide(configLayer)),
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
