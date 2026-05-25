import { existsSync } from "node:fs";
import { createReadStream } from "node:fs";
import { createInterface } from "node:readline";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Redis } from "ioredis";

const API_BASE_URL = process.env.API_BASE_URL ?? "http://127.0.0.1:3000";
const REDIS_URL = process.env.REDIS_URL ?? "redis://127.0.0.1:6379";
const REDIS_STREAM_KEY =
  process.env.REDIS_STREAM_KEY ?? "midgard:tx-submissions";
const REDIS_DEAD_LETTER_STREAM =
  process.env.TX_QUEUE_DEAD_LETTER_STREAM ??
  "midgard:tx-submissions:dead-letter";
const CORPUS_PATH = process.env.CORPUS_PATH ?? "";
const PIPELINE_TX_COUNT = Number(process.env.PIPELINE_TX_COUNT ?? "500");
const SUBMIT_BATCH_SIZE = 100;
const SUBMIT_BATCH_DELAY_MS = 800;
const MEMPOOL_POLL_TIMEOUT_MS = 120_000;
const HEALTH_WAIT_TIMEOUT_MS = 90_000;

interface CorpusTx {
  readonly cborHex: string;
  readonly txId: string;
}

const readCorpusTxs = async (
  corpusPath: string,
  count: number,
): Promise<readonly CorpusTx[]> => {
  const stream = createReadStream(corpusPath, { encoding: "utf8" });
  const rl = createInterface({ input: stream, crlfDelay: Infinity });
  const txs: CorpusTx[] = [];

  for await (const line of rl) {
    if (txs.length >= count) break;
    const trimmed = line.trim();
    if (!trimmed) continue;
    const parsed = JSON.parse(trimmed) as { cborHex: string; txId: string };
    txs.push({ cborHex: parsed.cborHex, txId: parsed.txId });
  }

  rl.close();
  stream.destroy();
  return txs;
};

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

let redis: Redis;

describe("tx ingress full pipeline e2e", () => {
  beforeAll(async () => {
    redis = new Redis(REDIS_URL);

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

    await redis.del(REDIS_STREAM_KEY, REDIS_DEAD_LETTER_STREAM);
  });

  afterAll(async () => {
    await redis.quit();
  });

  it("rejects non-hex submit payloads with 400", async () => {
    const response = await fetch(`${API_BASE_URL}/submit`, {
      method: "POST",
      body: "not-hex",
    });
    expect(response.status).toBe(400);
    await expect(response.json()).resolves.toEqual({
      error: "Invalid CBOR provided",
    });
  });

  it(
    "processes corpus transactions end-to-end through redis stream into mempool",
    async () => {
      if (!CORPUS_PATH || !existsSync(CORPUS_PATH)) {
        throw new Error(
          `CORPUS_PATH must point to a valid corpus JSONL file. Got: "${CORPUS_PATH}"`,
        );
      }

      const txs = await readCorpusTxs(CORPUS_PATH, PIPELINE_TX_COUNT);
      expect(txs.length).toBeGreaterThan(0);

      const submittedIds = new Set<string>();
      const rejectedCount = { value: 0 };

      for (let i = 0; i < txs.length; i += SUBMIT_BATCH_SIZE) {
        const batch = txs.slice(i, i + SUBMIT_BATCH_SIZE);

        const results = await Promise.all(
          batch.map(({ cborHex, txId }) =>
            fetch(`${API_BASE_URL}/submit`, {
              method: "POST",
              body: cborHex,
            }).then(async (res) => ({
              txId,
              status: res.status,
            })),
          ),
        );

        for (const { txId, status } of results) {
          if (status === 200) {
            submittedIds.add(txId);
          } else {
            rejectedCount.value += 1;
          }
        }

        if (i + SUBMIT_BATCH_SIZE < txs.length) {
          await new Promise<void>((resolve) =>
            setTimeout(resolve, SUBMIT_BATCH_DELAY_MS),
          );
        }
      }

      expect(rejectedCount.value).toBe(0);
      expect(submittedIds.size).toBe(txs.length);

      const notYetFound = new Set(submittedIds);

      await poll(
        async () => {
          const ids = [...notYetFound];
          const checks = await Promise.all(
            ids.map((txId) =>
              fetch(`${API_BASE_URL}/tx?tx_hash=${txId}`)
                .then((res) => ({ txId, found: res.status === 200 }))
                .catch(() => ({ txId, found: false as boolean })),
            ),
          );
          for (const { txId, found } of checks) {
            if (found) notYetFound.delete(txId);
          }
          return notYetFound.size === 0;
        },
        MEMPOOL_POLL_TIMEOUT_MS,
        2_000,
      );

      expect(notYetFound.size).toBe(0);
    },
    MEMPOOL_POLL_TIMEOUT_MS + HEALTH_WAIT_TIMEOUT_MS + 30_000,
  );
});
