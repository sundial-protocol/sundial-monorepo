import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Redis } from "ioredis";

const API_BASE_URL = process.env.API_BASE_URL ?? "http://127.0.0.1:3000";
const REDIS_URL = process.env.REDIS_URL ?? "redis://127.0.0.1:6379";
const REDIS_STREAM_KEY =
  process.env.REDIS_STREAM_KEY ?? "midgard:tx-submissions";
const REDIS_DEAD_LETTER_STREAM =
  process.env.TX_QUEUE_DEAD_LETTER_STREAM ??
  "midgard:tx-submissions:dead-letter";
const REDIS_STREAM_CONSUMER_GROUP =
  process.env.REDIS_STREAM_CONSUMER_GROUP ?? "midgard-tx-processors";

const poll = async (
  condition: () => Promise<boolean>,
  timeoutMs: number,
  intervalMs: number,
): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (await condition()) {
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, intervalMs));
  }
  throw new Error(`Timed out after ${timeoutMs}ms`);
};

let redis: Redis;

describe("redis stream ingress e2e", () => {
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
      60_000,
      1_000,
    );

    await redis.del(REDIS_STREAM_KEY, REDIS_DEAD_LETTER_STREAM);
    // DEL removes stream consumer groups; recreate the expected group for this
    // ingress-only E2E environment before assertions.
    try {
      await redis.xgroup(
        "CREATE",
        REDIS_STREAM_KEY,
        REDIS_STREAM_CONSUMER_GROUP,
        "$",
        "MKSTREAM",
      );
    } catch (error) {
      if (!(error instanceof Error) || !error.message.includes("BUSYGROUP")) {
        throw error;
      }
    }
  });

  afterAll(async () => {
    await redis.quit();
  });

  it("rejects non-hex submit payloads", async () => {
    const response = await fetch(`${API_BASE_URL}/submit`, {
      method: "POST",
      body: "not-hex",
    });
    expect(response.status).toBe(400);
    await expect(response.json()).resolves.toEqual({
      error: "Invalid CBOR provided",
    });
  });

  it("accepts hex submit payload and appends it to redis stream", async () => {
    const response = await fetch(`${API_BASE_URL}/submit`, {
      method: "POST",
      body: "aa",
    });

    expect(response.status).toBe(200);
    await expect(response.json()).resolves.toEqual({
      message: "Successfully added the transaction to the queue",
    });

    await poll(
      async () => {
        const len = await redis.xlen(REDIS_STREAM_KEY);
        return len >= 1;
      },
      10_000,
      200,
    );

    const groupInfoRaw = await redis.xinfo("GROUPS", REDIS_STREAM_KEY);
    const groupInfo = groupInfoRaw as Array<Array<string | number>>;
    const hasExpectedGroup = groupInfo.some((entry) => {
      for (let i = 0; i < entry.length - 1; i += 2) {
        if (
          entry[i] === "name" &&
          entry[i + 1] === REDIS_STREAM_CONSUMER_GROUP
        ) {
          return true;
        }
      }
      return false;
    });
    expect(hasExpectedGroup).toBe(true);
  });
});
