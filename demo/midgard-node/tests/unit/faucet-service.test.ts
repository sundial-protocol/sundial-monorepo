import { describe, expect, it } from "vitest";
import { Effect, Either } from "effect";
import { SqlClient } from "@effect/sql";
import * as Faucet from "@/services/faucet.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";

// Unit-level coverage of the faucet service's configuration and input-guard
// branches that do not require live Lucid/CML wallet derivation or a real
// ledger (those paths are covered by the integration and e2e suites). The
// SqlClient is supplied as an unused stub because these branches short-circuit
// before any database access.
const runClaim = (
  input: Faucet.FaucetClaimInput,
  overrides: Parameters<typeof makeTestNodeConfigLayer>[0],
) =>
  Effect.runPromise(
    Faucet.processClaim(input).pipe(
      Effect.either,
      Effect.provide(makeTestNodeConfigLayer(overrides)),
      Effect.provideService(
        SqlClient.SqlClient,
        {} as unknown as SqlClient.SqlClient,
      ),
    ),
  );

const input: Faucet.FaucetClaimInput = {
  address: "addr_test1_recipient",
  idempotencyKey: "idem-1",
  ipHash: "ip-1",
};

describe("Faucet.processClaim guard branches", () => {
  it("fails with DISABLED when the faucet is turned off", async () => {
    const result = await runClaim(input, { FAUCET_ENABLED: false });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("DISABLED");
    }
  });

  it("fails with DISABLED when enabled but the faucet wallet is unconfigured", async () => {
    const result = await runClaim(input, {
      FAUCET_ENABLED: true,
      FAUCET_API_KEY: "key",
      FAUCET_ADDRESS: "",
      FAUCET_SEED_PHRASE: "",
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("DISABLED");
    }
  });

  it("rejects an unparseable recipient address before any payout", async () => {
    const result = await runClaim(input, {
      FAUCET_ENABLED: true,
      FAUCET_API_KEY: "key",
      FAUCET_ADDRESS: "addr_test1_faucet",
      FAUCET_SEED_PHRASE: "faucet seed phrase",
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("ADDRESS_INVALID");
    }
  });
});
