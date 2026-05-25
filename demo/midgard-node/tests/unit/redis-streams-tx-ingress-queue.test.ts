import { describe, expect, beforeEach, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import {
  RedisStreamsTxIngressQueueLive,
  TxIngressQueue,
} from "@/services/index.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";

const redisState = vi.hoisted(() => ({
  calls: [] as Array<{ method: string; args: unknown[] }>,
  xgroupImpl: (_args: unknown[]) => Promise.resolve("OK"),
  xaddImpl: (_args: unknown[]) => Promise.resolve("1-0"),
  xackImpl: (_args: unknown[]) => Promise.resolve(1),
  xlenImpl: (_args: unknown[]) => Promise.resolve(0),
  xpendingImpl: (_args: unknown[]) => Promise.resolve([] as unknown[]),
  xautoclaimImpl: (_args: unknown[]) =>
    Promise.resolve(["0-0", []] as unknown[]),
  xreadgroupImpl: (_args: unknown[]) => Promise.resolve(null),
}));

vi.mock("ioredis", () => {
  class MockRedis {
    constructor(_url: string) {}

    duplicate() {
      return new MockRedis("redis://127.0.0.1:6379");
    }

    quit() {
      return Promise.resolve("OK");
    }

    xgroup(...args: unknown[]) {
      redisState.calls.push({ method: "xgroup", args });
      return redisState.xgroupImpl(args);
    }

    xadd(...args: unknown[]) {
      redisState.calls.push({ method: "xadd", args });
      return redisState.xaddImpl(args);
    }

    xack(...args: unknown[]) {
      redisState.calls.push({ method: "xack", args });
      return redisState.xackImpl(args);
    }

    xlen(...args: unknown[]) {
      redisState.calls.push({ method: "xlen", args });
      return redisState.xlenImpl(args);
    }

    xpending(...args: unknown[]) {
      redisState.calls.push({ method: "xpending", args });
      return redisState.xpendingImpl(args);
    }

    xautoclaim(...args: unknown[]) {
      redisState.calls.push({ method: "xautoclaim", args });
      return redisState.xautoclaimImpl(args);
    }

    xreadgroup(...args: unknown[]) {
      redisState.calls.push({ method: "xreadgroup", args });
      return redisState.xreadgroupImpl(args);
    }
  }

  return {
    Redis: MockRedis,
  };
});

const runWithQueue = <A, E>(effect: Effect.Effect<A, E, TxIngressQueue>) =>
  effect.pipe(
    Effect.provide(RedisStreamsTxIngressQueueLive),
    Effect.provide(makeTestNodeConfigLayer()),
    Effect.scoped,
  );

beforeEach(() => {
  redisState.calls.length = 0;
  redisState.xgroupImpl = () => Promise.resolve("OK");
  redisState.xaddImpl = () => Promise.resolve("1-0");
  redisState.xackImpl = () => Promise.resolve(1);
  redisState.xlenImpl = () => Promise.resolve(0);
  redisState.xpendingImpl = () => Promise.resolve([] as unknown[]);
  redisState.xautoclaimImpl = () => Promise.resolve(["0-0", []] as unknown[]);
  redisState.xreadgroupImpl = () => Promise.resolve(null);
});

describe("RedisStreamsTxIngressQueue", () => {
  it.effect("tolerates BUSYGROUP when creating consumer group", () =>
    Effect.gen(function* () {
      redisState.xgroupImpl = () =>
        Promise.reject(
          new Error("BUSYGROUP Consumer Group name already exists"),
        );

      const queue = yield* TxIngressQueue;
      yield* queue.ensureConsumerGroup;

      const xgroupCalls = redisState.calls.filter((c) => c.method === "xgroup");
      expect(xgroupCalls.length).toBe(1);
    }).pipe(runWithQueue),
  );

  it.effect(
    "dead-letters and acknowledges message when max delivery attempts are reached",
    () =>
      Effect.gen(function* () {
        const queue = yield* TxIngressQueue;

        const disposition = yield* queue.handleFailedMessage(
          {
            id: "10-1",
            txCbor: "aa",
            deliveryCount: 5,
          },
          "test failure",
        );

        expect(disposition).toBe("dead_lettered");

        const xaddCalls = redisState.calls.filter((c) => c.method === "xadd");
        const xackCalls = redisState.calls.filter((c) => c.method === "xack");
        expect(xaddCalls.length).toBe(1);
        expect(xackCalls.length).toBe(1);
      }).pipe(runWithQueue),
  );

  it.effect("parses reclaimed stream entries with delivery count", () =>
    Effect.gen(function* () {
      redisState.xautoclaimImpl = () =>
        Promise.resolve(["0-0", [["10-0", ["tx_cbor", "aa"]]]] as unknown[]);
      redisState.xpendingImpl = () =>
        Promise.resolve([["10-0", "worker-a", 1000, 2]] as unknown[]);

      const queue = yield* TxIngressQueue;
      const messages = yield* queue.consumeBatch(1, 1000);

      expect(messages).toHaveLength(1);
      expect(messages[0]).toEqual({
        id: "10-0",
        txCbor: "aa",
        deliveryCount: 2,
      });
    }).pipe(runWithQueue),
  );
});
