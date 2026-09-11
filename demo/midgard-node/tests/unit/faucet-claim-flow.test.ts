/**
 * Unit tests exercising the deep body of `Faucet.processClaim` — address
 * validation branches, faucet UTxO selection, transaction building/signing,
 * and the full DB transaction flow (idempotency, cooldown, IP limit,
 * depletion, mempool acceptance, and the terminal error-wrapping catchAll).
 *
 * `@lucid-evolution/lucid` is aliased to a minimal stub under the coverage
 * config (see vitest/node.coverage.config.mts), so this file replaces it with
 * a fully-controlled fake CML implementation via vi.mock, along with the
 * other collaborators faucet.ts talks to (the DB barrel, midgard-ts's
 * encode/convert helpers, and breakDownTx). This lets processClaim's real
 * function bodies run end-to-end without any live crypto or a real database.
 */
import { describe, expect, it, vi, beforeEach } from "vitest";
import { Effect, Either, Option } from "effect";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

// ---------------------------------------------------------------------------
// Fake CML / lucid-evolution plumbing
// ---------------------------------------------------------------------------

const fixtures = vi.hoisted(() => {
  const addressDetails = new Map<string, unknown>();
  const decodableBech32 = new Set<string>();
  return { addressDetails, decodableBech32 };
});

vi.mock("@lucid-evolution/lucid", () => {
  const getAddressDetails = (address: string) => {
    const details = fixtures.addressDetails.get(address.trim());
    if (details === undefined) {
      throw new Error(`unrecognized test address: ${address}`);
    }
    return details;
  };

  const walletFromSeed = (seed: string, _opts: { network: string }) => {
    if (seed === "BAD_SEED") {
      throw new Error("cannot derive wallet from seed");
    }
    return {
      paymentKey:
        seed === "SEED_WITH_BAD_KEY" ? "BAD_SIGNING_KEY" : "GOOD_SIGNING_KEY",
    };
  };

  const mkList = () => {
    const items: unknown[] = [];
    return { add: (x: unknown) => items.push(x), items };
  };

  const CML = {
    Address: {
      from_bech32: (bech32: string) => {
        if (!fixtures.decodableBech32.has(bech32)) {
          throw new Error(`cannot decode bech32: ${bech32}`);
        }
        return { __address: bech32 };
      },
    },
    PrivateKey: {
      from_bech32: (key: string) => {
        if (key === "BAD_SIGNING_KEY") {
          throw new Error("bad signing key");
        }
        return {
          to_public: () => ({ __pub: key }),
          sign: (_bytes: Uint8Array) => ({ __sig: key }),
        };
      },
    },
    TransactionOutput: {
      from_cbor_bytes: (bytes: Buffer) => {
        const spec = JSON.parse(bytes.toString("utf8")) as {
          coin: string;
          hasMultiAssets: boolean;
        };
        return {
          amount: () => ({
            has_multiassets: () => spec.hasMultiAssets,
            coin: () => BigInt(spec.coin),
          }),
        };
      },
      new_conway_format_tx_out: (address: unknown, value: unknown) => ({
        __output: true,
        address,
        value,
      }),
    },
    ConwayFormatTxOut: {
      new: (address: unknown, value: unknown) => ({ address, value }),
    },
    Value: {
      from_coin: (coin: bigint) => ({ __coin: coin }),
    },
    TransactionInput: {
      from_cbor_bytes: (bytes: Buffer) => ({ __input: bytes.toString("hex") }),
    },
    TransactionInputList: { new: mkList },
    TransactionOutputList: { new: mkList },
    TransactionBody: {
      new: (inputs: unknown, outputs: unknown, fee: bigint) => ({
        inputs,
        outputs,
        fee,
        set_network_id: (_n: unknown) => {},
      }),
    },
    NetworkId: {
      new: (id: bigint) => ({ __networkId: id }),
    },
    hash_transaction: (_body: unknown) => ({
      to_raw_bytes: () => new Uint8Array(32),
    }),
    TransactionWitnessSet: {
      new: () => ({ set_vkeywitnesses: (_l: unknown) => {} }),
    },
    VkeywitnessList: { new: mkList },
    Vkeywitness: {
      new: (pub: unknown, sig: unknown) => ({ pub, sig }),
    },
    Transaction: {
      new: (_body: unknown, _witnessSet: unknown, _isValid: boolean) => ({
        to_cbor_bytes: () => new Uint8Array([0xde, 0xad, 0xbe, 0xef]),
      }),
    },
  };

  return { CML, getAddressDetails, walletFromSeed };
});

// midgard-ts's encode/convert helpers: a fixed short encoded length keeps the
// fee-convergence loop in buildAndSignFaucetTx converging on its first pass.
vi.mock("../../../midgard-ts/src/index.js", () => ({
  cmlToMidgard: (_tx: unknown) => ({ __midgardTx: true }),
  encodeTransaction: (_midgardTx: unknown) => new Uint8Array(50),
}));

vi.mock("@/utils.js", () => ({
  breakDownTx: vi.fn(() =>
    Effect.succeed({
      txId: Buffer.alloc(32, 0xee),
      spent: [],
      produced: [],
    }),
  ),
}));

vi.mock("@/database/index.js", async () => {
  const { Effect: E, Option: O } = await import("effect");
  return {
    FaucetClaimsDB: {
      Columns: {
        CLAIM_ID: "claim_id",
        IDEMPOTENCY_KEY: "idempotency_key",
        ADDRESS: "address",
        IP_HASH: "ip_hash",
        AMOUNT_LOVELACE: "amount_lovelace",
        TX_ID: "tx_id",
        CREATED_AT: "created_at",
        NEXT_ELIGIBLE_AT: "next_eligible_at",
      },
      findByIdempotencyKey: vi.fn(() => E.succeed(O.none())),
      findActiveCooldownByAddress: vi.fn(() => E.succeed(O.none())),
      countByIpSince: vi.fn(() => E.succeed(0)),
      insertClaim: vi.fn(() => E.succeed(undefined)),
    },
    MempoolDB: {
      validateAndInsertMultiple: vi.fn(() =>
        E.succeed({ acceptedMessages: [], insertedCount: 1, rejected: [] }),
      ),
    },
    MempoolLedgerDB: {
      retrieveByAddress: vi.fn(() => E.succeed([])),
    },
  };
});

import * as Faucet from "@/services/faucet.js";
import {
  FaucetClaimsDB,
  MempoolDB,
  MempoolLedgerDB,
} from "@/database/index.js";

// ---------------------------------------------------------------------------
// Test address fixtures
// ---------------------------------------------------------------------------

const GOOD_ADDR = "good-recipient-addr";
const REWARD_ADDR = "reward-only-addr";
const MAINNET_ADDR = "mainnet-addr";
const SCRIPT_ADDR = "script-addr";
const UNDECODABLE_ADDR = "undecodable-addr";

fixtures.addressDetails.set(GOOD_ADDR, {
  paymentCredential: { type: "Key", hash: "aa" },
  networkId: 0,
  address: { bech32: "GOOD_ADDR_BECH32" },
});
fixtures.addressDetails.set(REWARD_ADDR, {
  paymentCredential: undefined,
  networkId: 0,
  address: { bech32: "REWARD_ADDR_BECH32" },
});
fixtures.addressDetails.set(MAINNET_ADDR, {
  paymentCredential: { type: "Key", hash: "cc" },
  networkId: 1,
  address: { bech32: "MAINNET_ADDR_BECH32" },
});
fixtures.addressDetails.set(SCRIPT_ADDR, {
  paymentCredential: { type: "Script", hash: "bb" },
  networkId: 0,
  address: { bech32: "SCRIPT_ADDR_BECH32" },
});
fixtures.addressDetails.set(UNDECODABLE_ADDR, {
  paymentCredential: { type: "Key", hash: "dd" },
  networkId: 0,
  address: { bech32: "UNDECODABLE_BECH32" },
});

fixtures.decodableBech32.add("GOOD_ADDR_BECH32");
fixtures.decodableBech32.add("MAINNET_ADDR_BECH32");
fixtures.decodableBech32.add("FAUCET_ADDR_BECH32");

// ---------------------------------------------------------------------------
// Test harness
// ---------------------------------------------------------------------------

const sqlHarness = createMockSqlHarness();

beforeEach(() => {
  sqlHarness.reset();
  vi.clearAllMocks();
});

const baseConfig = {
  FAUCET_ENABLED: true,
  FAUCET_API_KEY: "key",
  FAUCET_ADDRESS: "FAUCET_ADDR_BECH32",
  FAUCET_SEED_PHRASE: "GOOD_SEED",
  FAUCET_MIN_BALANCE_LOVELACE: 0n,
  FAUCET_AMOUNT_LOVELACE: 1_000n,
  FAUCET_DAILY_IP_LIMIT: 5,
  FAUCET_COOLDOWN_SECONDS: 3_600,
} as const;

const runClaim = (
  input: Faucet.FaucetClaimInput,
  overrides: Parameters<typeof makeTestNodeConfigLayer>[0] = {},
) =>
  Effect.runPromise(
    Faucet.processClaim(input).pipe(
      Effect.either,
      Effect.provide(makeTestNodeConfigLayer({ ...baseConfig, ...overrides })),
      Effect.provide(sqlHarness.layer),
    ),
  );

const makeFaucetEntry = (coin: bigint, hasMultiAssets = false) => ({
  outref: Buffer.from(
    `outref-${coin}-${hasMultiAssets}-${Math.random().toString(36).slice(2)}`,
  ),
  output: Buffer.from(
    JSON.stringify({ coin: coin.toString(), hasMultiAssets }),
  ),
});

const input = (overrides: Partial<Faucet.FaucetClaimInput> = {}) => ({
  address: GOOD_ADDR,
  idempotencyKey: "idem-1",
  ipHash: "ip-1",
  ...overrides,
});

// ---------------------------------------------------------------------------
// validateRecipientAddress branches
// ---------------------------------------------------------------------------

describe("processClaim: recipient address validation", () => {
  it("fails with ADDRESS_NO_PAYMENT_CREDENTIAL for a reward-only address", async () => {
    const result = await runClaim(input({ address: REWARD_ADDR }));
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("ADDRESS_NO_PAYMENT_CREDENTIAL");
    }
  });

  it("fails with ADDRESS_NETWORK_MISMATCH for a mainnet address on a testnet node", async () => {
    const result = await runClaim(input({ address: MAINNET_ADDR }));
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("ADDRESS_NETWORK_MISMATCH");
    }
  });

  it("fails with ADDRESS_SCRIPT for a script payment address", async () => {
    const result = await runClaim(input({ address: SCRIPT_ADDR }));
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("ADDRESS_SCRIPT");
    }
  });

  it("fails with ADDRESS_INVALID when the decoded bech32 cannot be re-decoded", async () => {
    const result = await runClaim(input({ address: UNDECODABLE_ADDR }));
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("ADDRESS_INVALID");
    }
  });
});

// ---------------------------------------------------------------------------
// Wallet / key derivation branches
// ---------------------------------------------------------------------------

describe("processClaim: faucet wallet derivation", () => {
  it("fails with INTERNAL when the seed phrase cannot derive a wallet", async () => {
    const result = await runClaim(input(), { FAUCET_SEED_PHRASE: "BAD_SEED" });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("INTERNAL");
      expect(result.left.message).toContain("derive faucet wallet");
    }
  });

  it("resolves networkId for a Mainnet node and still fails cleanly on a bad seed", async () => {
    const result = await runClaim(input({ address: MAINNET_ADDR }), {
      NETWORK: "Mainnet",
      FAUCET_SEED_PHRASE: "BAD_SEED",
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("INTERNAL");
    }
  });

  it("fails with INTERNAL when the derived signing key cannot be decoded", async () => {
    const result = await runClaim(input(), {
      FAUCET_SEED_PHRASE: "SEED_WITH_BAD_KEY",
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("INTERNAL");
      expect(result.left.message).toContain("decode faucet signing key");
    }
  });

  it("fails with INTERNAL when the faucet address cannot be decoded", async () => {
    const result = await runClaim(input(), {
      FAUCET_ADDRESS: "UNDECODABLE_BECH32",
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("INTERNAL");
      expect(result.left.message).toContain("decode faucet address");
    }
  });
});

// ---------------------------------------------------------------------------
// DB transaction flow: idempotency, cooldown, IP limit, depletion
// ---------------------------------------------------------------------------

describe("processClaim: DB transaction guards", () => {
  it("returns the existing claim without a new payout on idempotent replay", async () => {
    const nextEligibleAt = new Date(Date.now() + 3_600_000);
    vi.mocked(FaucetClaimsDB.findByIdempotencyKey).mockReturnValueOnce(
      Effect.succeed(
        Option.some({
          claimId: "existing-claim",
          idempotencyKey: "idem-1",
          address: "GOOD_ADDR_BECH32",
          ipHash: "ip-1",
          amountLovelace: 555n,
          txHashHex: "ab".repeat(32),
          createdAt: new Date(),
          nextEligibleAt,
        }),
      ),
    );

    const result = await runClaim(input());
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) {
      expect(result.right).toEqual({
        claimId: "existing-claim",
        txHash: "ab".repeat(32),
        amount: 555n,
        nextEligibleAt,
        idempotentReplay: true,
      });
    }
    expect(vi.mocked(MempoolLedgerDB.retrieveByAddress)).not.toHaveBeenCalled();
  });

  it("fails with COOLDOWN when the address is in an active cooldown", async () => {
    const nextEligibleAt = new Date(Date.now() + 1_800_000);
    vi.mocked(FaucetClaimsDB.findActiveCooldownByAddress).mockReturnValueOnce(
      Effect.succeed(
        Option.some({
          claimId: "prior-claim",
          idempotencyKey: "idem-prior",
          address: "GOOD_ADDR_BECH32",
          ipHash: "ip-1",
          amountLovelace: 100n,
          txHashHex: "cd".repeat(32),
          createdAt: new Date(),
          nextEligibleAt,
        }),
      ),
    );

    const result = await runClaim(input());
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("COOLDOWN");
      expect(result.left.nextEligibleAt).toEqual(nextEligibleAt);
    }
  });

  it("fails with IP_LIMIT when the daily IP quota is reached", async () => {
    vi.mocked(FaucetClaimsDB.countByIpSince).mockReturnValueOnce(
      Effect.succeed(5),
    );

    const result = await runClaim(input(), { FAUCET_DAILY_IP_LIMIT: 5 });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("IP_LIMIT");
    }
  });

  it("fails with DEPLETED when there are no lovelace-only faucet UTxOs", async () => {
    vi.mocked(MempoolLedgerDB.retrieveByAddress).mockReturnValueOnce(
      Effect.succeed([makeFaucetEntry(999_999n, true)] as never),
    );

    const result = await runClaim(input());
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("DEPLETED");
      expect(result.left.message).toContain("no spendable UTxO");
    }
  });

  it("fails with DEPLETED when the total balance is below the configured minimum", async () => {
    vi.mocked(MempoolLedgerDB.retrieveByAddress).mockReturnValueOnce(
      Effect.succeed([makeFaucetEntry(100n)] as never),
    );

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 1_000n,
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("DEPLETED");
      expect(result.left.message).toContain("below the configured minimum");
    }
  });
});

// ---------------------------------------------------------------------------
// buildAndSignFaucetTx branches
// ---------------------------------------------------------------------------

describe("processClaim: transaction building", () => {
  it("fails with DEPLETED when the selected UTxO cannot cover amount + fee", async () => {
    vi.mocked(MempoolLedgerDB.retrieveByAddress).mockReturnValueOnce(
      Effect.succeed([makeFaucetEntry(1_000n)] as never),
    );

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 100n,
      FAUCET_AMOUNT_LOVELACE: 500n,
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("DEPLETED");
      expect(result.left.message).toContain("cannot cover");
    }
  });

  it("succeeds with an exact-change (zero) faucet output", async () => {
    vi.mocked(MempoolLedgerDB.retrieveByAddress).mockReturnValueOnce(
      Effect.succeed([makeFaucetEntry(210_381n)] as never),
    );

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 0n,
      FAUCET_AMOUNT_LOVELACE: 50_000n,
    });
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) {
      expect(result.right.amount).toBe(50_000n);
      expect(result.right.idempotentReplay).toBe(false);
    }
  });
});

// ---------------------------------------------------------------------------
// Mempool acceptance branches + full happy path
// ---------------------------------------------------------------------------

describe("processClaim: mempool acceptance and happy path", () => {
  const setupSpendableEntries = () => {
    vi.mocked(MempoolLedgerDB.retrieveByAddress).mockReturnValueOnce(
      Effect.succeed([
        makeFaucetEntry(50_000n),
        makeFaucetEntry(999_999_999n, true),
        makeFaucetEntry(300_000n),
      ] as never),
    );
  };

  it("pays out successfully, selecting the largest lovelace-only UTxO", async () => {
    setupSpendableEntries();

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 100_000n,
      FAUCET_AMOUNT_LOVELACE: 100_000n,
    });

    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) {
      expect(result.right.amount).toBe(100_000n);
      expect(result.right.txHash).toBe("ee".repeat(32));
      expect(result.right.idempotentReplay).toBe(false);
      expect(result.right.claimId).toBeTruthy();
    }

    expect(vi.mocked(MempoolDB.validateAndInsertMultiple)).toHaveBeenCalledWith(
      [
        expect.objectContaining({
          arrivalSeq: 0n,
          message: expect.objectContaining({ txCbor: "deadbeef" }),
        }),
      ],
    );

    expect(vi.mocked(FaucetClaimsDB.insertClaim)).toHaveBeenCalledWith(
      expect.objectContaining({
        address: "GOOD_ADDR_BECH32",
        amount_lovelace: 100_000n,
        ip_hash: "ip-1",
        idempotency_key: "idem-1",
      }),
    );
  });

  it("fails with VALIDATION_FAILED and surfaces the rejection reason", async () => {
    setupSpendableEntries();
    vi.mocked(MempoolDB.validateAndInsertMultiple).mockReturnValueOnce(
      Effect.succeed({
        acceptedMessages: [],
        insertedCount: 0,
        rejected: [
          {
            message: { id: "x", txCbor: "", deliveryCount: 0 },
            reason: "duplicate nonce",
          },
        ],
      }),
    );

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 100_000n,
      FAUCET_AMOUNT_LOVELACE: 100_000n,
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("VALIDATION_FAILED");
      expect(result.left.message).toContain("duplicate nonce");
    }
  });

  it("fails with VALIDATION_FAILED using a generic reason when none is given", async () => {
    setupSpendableEntries();
    vi.mocked(MempoolDB.validateAndInsertMultiple).mockReturnValueOnce(
      Effect.succeed({ acceptedMessages: [], insertedCount: 0, rejected: [] }),
    );

    const result = await runClaim(input(), {
      FAUCET_MIN_BALANCE_LOVELACE: 100_000n,
      FAUCET_AMOUNT_LOVELACE: 100_000n,
    });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("VALIDATION_FAILED");
      expect(result.left.message).toContain("not accepted into the mempool");
    }
  });
});

// ---------------------------------------------------------------------------
// Terminal catchAll: non-FaucetClaimError failures get wrapped as INTERNAL
// ---------------------------------------------------------------------------

describe("processClaim: unexpected error wrapping", () => {
  it("wraps a non-FaucetClaimError failure as INTERNAL", async () => {
    vi.mocked(FaucetClaimsDB.findByIdempotencyKey).mockReturnValueOnce(
      Effect.fail(new Error("boom") as never),
    );

    const result = await runClaim(input());
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result)) {
      expect(result.left.code).toBe("INTERNAL");
      expect(result.left.message).toContain(
        "Unexpected error while processing faucet claim",
      );
    }
  });
});
