import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { randomUUID } from "node:crypto";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

// Live faucet-claim E2E. Exercises the real Dockerized node + Postgres stack
// brought up by scripts/test/system/node-faucet-e2e.sh, which seeds a
// genesis-funded faucet wallet and enables POST /faucet/claims.

const API_BASE_URL = process.env.API_BASE_URL ?? "http://127.0.0.1:3000";
const FAUCET_API_KEY = process.env.FAUCET_API_KEY ?? "";
const FAUCET_AMOUNT_LOVELACE = BigInt(
  process.env.FAUCET_AMOUNT_LOVELACE ?? "100000000",
);
// Distinct from any node operator/genesis/faucet seed; only used to derive a
// recipient testnet address for the claim.
const RECIPIENT_SEED =
  process.env.FAUCET_E2E_RECIPIENT_SEED ??
  "test test test test test test test test test test test junk";

const HEALTH_WAIT_TIMEOUT_MS = 90_000;
const UTXO_POLL_TIMEOUT_MS = 60_000;

const recipientAddress = walletFromSeed(RECIPIENT_SEED, {
  network: "Preprod",
}).address;

const claimHeaders = (token = FAUCET_API_KEY) => ({
  "content-type": "application/json",
  authorization: `Bearer ${token}`,
});

const postClaim = (body: unknown, headers: Record<string, string>) =>
  fetch(`${API_BASE_URL}/faucet/claims`, {
    method: "POST",
    headers,
    body: typeof body === "string" ? body : JSON.stringify(body),
  });

const poll = async (
  condition: () => Promise<boolean>,
  timeoutMs: number,
  intervalMs: number,
): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (await condition()) return;
    await new Promise<void>((resolve) => setTimeout(resolve, intervalMs));
  }
  throw new Error(`Timed out after ${timeoutMs}ms`);
};

// Reads the recipient's largest UTxO value (lovelace) from the node, or null.
const largestRecipientLovelace = async (): Promise<bigint | null> => {
  const res = await fetch(
    `${API_BASE_URL}/utxos?address=${encodeURIComponent(recipientAddress)}`,
  );
  if (res.status !== 200) return null;
  const body = (await res.json()) as { utxos: { value: string }[] };
  let max: bigint | null = null;
  for (const utxo of body.utxos) {
    const coin = CML.TransactionOutput.from_cbor_bytes(
      Buffer.from(utxo.value, "hex"),
    )
      .amount()
      .coin();
    if (max === null || coin > max) max = coin;
  }
  return max;
};

describe("faucet claims e2e", () => {
  beforeAll(async () => {
    expect(FAUCET_API_KEY).not.toBe("");
    await poll(
      async () => {
        try {
          const response = await fetch(`${API_BASE_URL}/health/live`);
          return response.status === 200;
        } catch {
          return false;
        }
      },
      HEALTH_WAIT_TIMEOUT_MS,
      1_000,
    );
  });

  afterAll(async () => {
    // No persistent client to close.
  });

  it("rejects an unauthenticated claim with 401", async () => {
    const res = await postClaim(
      { address: recipientAddress, idempotencyKey: randomUUID(), ipHash: "ip" },
      { "content-type": "application/json" },
    );
    expect(res.status).toBe(401);
  });

  it("rejects a wrong bearer token with 401", async () => {
    const res = await postClaim(
      { address: recipientAddress, idempotencyKey: randomUUID(), ipHash: "ip" },
      claimHeaders("wrong-secret"),
    );
    expect(res.status).toBe(401);
  });

  it("rejects an invalid address with 400", async () => {
    const res = await postClaim(
      {
        address: "not-a-valid-address",
        idempotencyKey: randomUUID(),
        ipHash: "ip",
      },
      claimHeaders(),
    );
    expect(res.status).toBe(400);
  });

  it(
    "pays out a claim, exposes it via /utxos, idempotently replays, then enforces cooldown",
    async () => {
      const idempotencyKey = randomUUID();
      const ipHash = `e2e-ip-${randomUUID()}`;

      // 1. A fresh claim succeeds and returns a receipt.
      const claimRes = await postClaim(
        { address: recipientAddress, idempotencyKey, ipHash },
        claimHeaders(),
      );
      expect(claimRes.status).toBe(200);
      const claim = (await claimRes.json()) as {
        claimId: string;
        txHash: string;
        amount: string;
        nextEligibleAt: string;
      };
      expect(claim.txHash).toMatch(/^[0-9a-f]{64}$/);
      expect(BigInt(claim.amount)).toBe(FAUCET_AMOUNT_LOVELACE);
      expect(Number.isNaN(Date.parse(claim.nextEligibleAt))).toBe(false);

      // 2. The signed faucet transaction lands in the mempool.
      await poll(
        async () => {
          const res = await fetch(`${API_BASE_URL}/tx?tx_hash=${claim.txHash}`);
          return res.status === 200;
        },
        UTXO_POLL_TIMEOUT_MS,
        1_000,
      );

      // 3. The recipient now owns a UTxO worth the configured grant amount.
      await poll(
        async () =>
          (await largestRecipientLovelace()) === FAUCET_AMOUNT_LOVELACE,
        UTXO_POLL_TIMEOUT_MS,
        1_000,
      );

      // 4. Replaying the same idempotency key returns the same claim without a
      //    second payout.
      const replayRes = await postClaim(
        { address: recipientAddress, idempotencyKey, ipHash },
        claimHeaders(),
      );
      expect(replayRes.status).toBe(200);
      const replay = (await replayRes.json()) as {
        claimId: string;
        txHash: string;
      };
      expect(replay.claimId).toBe(claim.claimId);
      expect(replay.txHash).toBe(claim.txHash);

      // 5. A new request for the same address (fresh idempotency key) is blocked
      //    by the cooldown.
      const cooldownRes = await postClaim(
        { address: recipientAddress, idempotencyKey: randomUUID(), ipHash },
        claimHeaders(),
      );
      expect(cooldownRes.status).toBe(429);
      const cooldown = (await cooldownRes.json()) as {
        code: string;
        nextEligibleAt?: string;
      };
      expect(cooldown.code).toBe("COOLDOWN");
      expect(cooldown.nextEligibleAt).toBeDefined();
    },
    UTXO_POLL_TIMEOUT_MS * 2 + HEALTH_WAIT_TIMEOUT_MS + 30_000,
  );
});
