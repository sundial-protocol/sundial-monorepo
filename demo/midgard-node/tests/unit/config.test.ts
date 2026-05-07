import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { ConfigProvider, Effect, Exit, Layer } from "effect";

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

describe("NodeConfig resolves with valid env vars", () => {
  it.effect(
    "NodeConfig resolves with valid env vars",
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
});

describe("NodeConfig sets default values when optional vars are absent", () => {
  it.effect(
    "NodeConfig sets default values when optional vars are absent",
    () =>
      Effect.gen(function* () {
        const config = yield* NodeConfig;
        expect(config.PROM_METRICS_PORT).toBe(9464);
        expect(config.POSTGRES_USER).toBe("postgres");
        expect(config.POSTGRES_DB).toBe("midgard");
      }).pipe(Effect.provide(configLayer)),
    { timeout: 10000 },
  );
});

describe("NodeConfig returns empty genesis UTxOs for Mainnet", () => {
  it.effect(
    "NodeConfig returns empty genesis UTxOs for Mainnet",
    () => {
      const mainnetProvider = ConfigProvider.fromMap(
        new Map([
          ["L1_PROVIDER", "Blockfrost"],
          ["L1_BLOCKFROST_API_URL", "http://mainnet"],
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
});

describe("ConfigError has expected tag", () => {
  it("ConfigError has expected tag", () => {
    const err = new ConfigError({
      message: "bad config",
      cause: undefined,
      fieldsAndValues: [["key", "val"]],
    });
    expect(err._tag).toBe("ConfigError");
    expect(err.message).toBe("bad config");
  });
});
