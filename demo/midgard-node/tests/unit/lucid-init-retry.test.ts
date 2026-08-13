import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { ConfigProvider, Duration, Effect, Layer, Schedule } from "effect";

vi.mock("@lucid-evolution/lucid", () => ({
  walletFromSeed: vi.fn(() => ({ address: "addr_test1mock" })),
}));

import { Lucid, lucidInitRetrySchedule } from "@/services/lucid.js";

// Minimal env needed to resolve NodeConfig for the Lucid service's own
// (baked-in) NodeConfig dependency; values are irrelevant to this test as
// long as they satisfy the config schema, since NODE_ROLE=tx-processor must
// short-circuit before any of these fields are actually used.
const testConfigProvider = (overrides: Record<string, string>) =>
  ConfigProvider.fromMap(
    new Map(
      Object.entries({
        L1_PROVIDER: "Kupmios",
        L1_BLOCKFROST_API_URL: "http://localhost:1337",
        L1_BLOCKFROST_KEY: "blockfrost-key",
        L1_OGMIOS_KEY: "ogmios-key",
        L1_KUPO_KEY: "kupo-key",
        L1_OPERATOR_SEED_PHRASE: "seed phrase operator",
        L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT:
          "seed phrase block commitment",
        L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "seed phrase merge tx",
        NETWORK: "Preview",
        TESTNET_GENESIS_WALLET_SEED_PHRASE_A: "seed phrase a",
        TESTNET_GENESIS_WALLET_SEED_PHRASE_B: "seed phrase b",
        TESTNET_GENESIS_WALLET_SEED_PHRASE_C: "seed phrase c",
        ...overrides,
      }),
    ),
  );

describe("Lucid init retry schedule", () => {
  it.effect(
    "backs off exponentially instead of retrying at a fixed 1s interval, and stays capped",
    () =>
      Effect.gen(function* () {
        const delays = yield* Schedule.run(
          lucidInitRetrySchedule.pipe(Schedule.delays),
          0,
          Array.from({ length: 8 }, () => undefined),
        );
        const millis = Array.from(delays).map((d) => Duration.toMillis(d));

        // Grows well past a flat 1s interval within a few attempts...
        expect(millis[3]).toBeGreaterThan(1_500);
        // ...but never exceeds the 30s cap (plus jitter headroom), so it
        // eventually settles rather than growing without bound.
        for (const m of millis) {
          expect(m).toBeLessThanOrEqual(36_000);
        }
      }),
  );
});

describe("Lucid service for NODE_ROLE=tx-processor", () => {
  it.effect(
    "skips L1 initialization entirely instead of retrying against L1 it doesn't need",
    () =>
      Effect.gen(function* () {
        const lucidLayer = Lucid.Default.pipe(
          Layer.provide(
            Layer.setConfigProvider(
              testConfigProvider({
                NODE_ROLE: "tx-processor",
                // Regression guard: if the tx-processor short-circuit is
                // lost, this reintroduces the infinite retry loop against L1
                // that the finding flagged, and the timeout below catches it.
                LUCID_INIT_MAX_RETRIES: "-1",
              }),
            ),
          ),
        );

        const lucid = yield* Lucid.pipe(
          Effect.provide(lucidLayer),
          Effect.timeoutFail({
            duration: "1 second",
            onTimeout: () =>
              new Error(
                "Lucid init for NODE_ROLE=tx-processor did not resolve promptly; " +
                  "it may be retrying L1 connectivity it doesn't need.",
              ),
          }),
        );

        expect(() => lucid.api.utxosAt).toThrow(/degraded mode/);

        const checkReadyResult = yield* Effect.either(lucid.checkReady);
        expect(checkReadyResult._tag).toBe("Left");
      }),
  );
});
