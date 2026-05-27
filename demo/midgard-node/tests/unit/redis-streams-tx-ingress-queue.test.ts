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
  callImpl: (_args: unknown[]) => Promise.resolve([] as unknown[]),
  xautoclaimImpl: (_args: unknown[]) =>
    Promise.resolve(["0-0", []] as unknown[]),
  xreadgroupImpl: (_args: unknown[]) => Promise.resolve(null),
  delImpl: (_args: unknown[]) => Promise.resolve(1),
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

    call(...args: unknown[]) {
      redisState.calls.push({ method: "call", args });
      return redisState.callImpl(args);
    }

    xautoclaim(...args: unknown[]) {
      redisState.calls.push({ method: "xautoclaim", args });
      return redisState.xautoclaimImpl(args);
    }

    xreadgroup(...args: unknown[]) {
      redisState.calls.push({ method: "xreadgroup", args });
      return redisState.xreadgroupImpl(args);
    }

    del(...args: unknown[]) {
      redisState.calls.push({ method: "del", args });
      return redisState.delImpl(args);
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
  redisState.callImpl = () => Promise.resolve([] as unknown[]);
  redisState.xautoclaimImpl = () => Promise.resolve(["0-0", []] as unknown[]);
  redisState.xreadgroupImpl = () => Promise.resolve(null);
  redisState.delImpl = () => Promise.resolve(1);
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

  it.effect("uses XINFO GROUPS lag as stream depth snapshot", () =>
    Effect.gen(function* () {
      redisState.xlenImpl = () => Promise.resolve(10_000);
      redisState.xpendingImpl = () => Promise.resolve([0, "0-0", "0-0", []]);
      redisState.callImpl = () =>
        Promise.resolve([
          [
            "name",
            "midgard-tx-processors",
            "consumers",
            1,
            "pending",
            0,
            "last-delivered-id",
            "1779703559252-0",
            "entries-read",
            10_000,
            "lag",
            0,
          ],
        ]);

      const queue = yield* TxIngressQueue;
      const snapshot = yield* queue.refreshSnapshotMetrics;

      expect(snapshot).toEqual({
        streamDepth: 0,
        pendingCount: 0,
        lagCount: 0,
      });
    }).pipe(runWithQueue),
  );

  it.effect("clear deletes both streams and recreates the consumer group", () =>
    Effect.gen(function* () {
      const queue = yield* TxIngressQueue;
      yield* queue.clear;

      const delCalls = redisState.calls.filter((c) => c.method === "del");
      const xgroupCalls = redisState.calls.filter((c) => c.method === "xgroup");
      expect(delCalls.length).toBe(2);
      expect(xgroupCalls.length).toBe(1);
      expect(xgroupCalls[0].args).toEqual([
        "CREATE",
        "midgard:tx-submissions",
        "midgard-tx-processors",
        "0",
        "MKSTREAM",
      ]);
    }).pipe(runWithQueue),
  );

  it.effect(
    "snapshotMetrics returns zero pending/lag when consumer group does not exist",
    () =>
      Effect.gen(function* () {
        redisState.xlenImpl = () => Promise.resolve(10_000);
        redisState.xpendingImpl = () =>
          Promise.reject(
            new Error(
              "NOGROUP No such consumer group 'midgard-tx-processors' for key name 'midgard:tx-submissions'",
            ),
          );
        redisState.callImpl = () => Promise.resolve([]);

        const queue = yield* TxIngressQueue;
        const snapshot = yield* queue.refreshSnapshotMetrics;

        expect(snapshot.pendingCount).toBe(0);
        expect(snapshot.lagCount).toBe(10_000);
      }).pipe(runWithQueue),
  );

  it.effect(
    "snapshotMetrics returns zero when stream does not exist (XINFO_GROUPS no such key)",
    () =>
      Effect.gen(function* () {
        redisState.xlenImpl = () => Promise.resolve(0);
        redisState.xpendingImpl = () =>
          Promise.reject(
            new Error(
              "NOGROUP No such consumer group 'midgard-tx-processors' for key name 'midgard:tx-submissions'",
            ),
          );
        redisState.callImpl = () =>
          Promise.reject(new Error("ERR no such key"));

        const queue = yield* TxIngressQueue;
        const snapshot = yield* queue.refreshSnapshotMetrics;

        expect(snapshot.pendingCount).toBe(0);
        expect(snapshot.lagCount).toBe(0);
        expect(snapshot.streamDepth).toBe(0);
      }).pipe(runWithQueue),
  );

  it.effect("enqueue avoids XGROUP_CREATE on hot path", () =>
    Effect.gen(function* () {
      const queue = yield* TxIngressQueue;

      const id = yield* queue.enqueue("aa");

      expect(id).toBe("1-0");
      const xgroupCalls = redisState.calls.filter((c) => c.method === "xgroup");
      const xaddCalls = redisState.calls.filter((c) => c.method === "xadd");
      expect(xgroupCalls.length).toBe(0);
      expect(xaddCalls.length).toBe(1);
    }).pipe(runWithQueue),
  );

  it.effect("snapshotMetrics returns cached values without Redis calls", () =>
    Effect.gen(function* () {
      redisState.xlenImpl = () => Promise.resolve(10);
      redisState.xpendingImpl = () => Promise.resolve([2, "0-0", "0-0", []]);
      redisState.callImpl = () =>
        Promise.resolve([
          [
            "name",
            "midgard-tx-processors",
            "consumers",
            1,
            "pending",
            2,
            "last-delivered-id",
            "1779703559252-0",
            "entries-read",
            8,
            "lag",
            8,
          ],
        ]);

      const queue = yield* TxIngressQueue;
      const beforeCalls = redisState.calls.length;
      const defaultSnapshot = yield* queue.snapshotMetrics;
      const afterDefaultCalls = redisState.calls.length;

      expect(defaultSnapshot).toEqual({
        streamDepth: 0,
        pendingCount: 0,
        lagCount: 0,
      });
      expect(afterDefaultCalls).toBe(beforeCalls);

      const refreshed = yield* queue.refreshSnapshotMetrics;
      const cached = yield* queue.snapshotMetrics;
      const afterCachedCalls = redisState.calls.length;

      expect(refreshed).toEqual({
        streamDepth: 8,
        pendingCount: 2,
        lagCount: 8,
      });
      expect(cached).toEqual(refreshed);
      expect(afterCachedCalls).toBeGreaterThan(afterDefaultCalls);
    }).pipe(runWithQueue),
  );

  it.effect(
    "consumeBatch recreates consumer group on NOGROUP and returns empty batch",
    () =>
      Effect.gen(function* () {
        redisState.xautoclaimImpl = () =>
          Promise.reject(
            new Error(
              "NOGROUP No such consumer group 'midgard-tx-processors' for key name 'midgard:tx-submissions'",
            ),
          );

        const queue = yield* TxIngressQueue;
        const messages = yield* queue.consumeBatch(10, 1000);

        expect(messages).toEqual([]);
        const xgroupCalls = redisState.calls.filter(
          (c) => c.method === "xgroup",
        );
        expect(xgroupCalls.length).toBe(1);
        expect(xgroupCalls[0].args).toEqual([
          "CREATE",
          "midgard:tx-submissions",
          "midgard-tx-processors",
          "0",
          "MKSTREAM",
        ]);
      }).pipe(runWithQueue),
  );
});
