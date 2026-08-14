import { describe, expect, it } from "vitest";
import { Effect, Layer } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { HttpServerResponse } from "@effect/platform";
import * as SDK from "@al-ft/midgard-sdk";
import { getHealthReadyHandlerForTesting } from "@/commands/listen.js";
import {
  TxIngressQueue,
  TxIngressQueueError,
  TxIngressQueueService,
} from "@/services/tx-ingress-queue.js";
import { Lucid } from "@/services/lucid.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const makeQueueStub = (
  overrides?: Partial<TxIngressQueueService>,
): TxIngressQueueService => ({
  ping: Effect.void,
  enqueue: (_txCbor: string) => Effect.succeed("1-0"),
  rawXadd: (_txCbor, callback) => callback(null, "1-0"),
  ensureConsumerGroup: Effect.void,
  consumeBatch: (_maxCount: number, _blockMs: number) => Effect.succeed([]),
  ack: (_messageIds: readonly string[]) => Effect.succeed(0),
  handleFailedMessage: (_message, _reason) => Effect.succeed("retry"),
  refreshSnapshotMetrics: Effect.succeed({
    streamDepth: 0,
    pendingCount: 0,
    lagCount: 0,
  }),
  snapshotMetrics: Effect.succeed({
    streamDepth: 0,
    pendingCount: 0,
    lagCount: 0,
  }),
  clear: Effect.void,
  ...overrides,
});

const makeLucidLayer = (checkReady: Effect.Effect<void, SDK.LucidError>) =>
  Layer.succeed(
    Lucid,
    Lucid.of({
      _tag: "Lucid",
      api: {} as never,
      mainApi: {} as never,
      blockCommitmentApi: {} as never,
      mergeApi: {} as never,
      reinitializeMergeApi: Effect.void,
      switchToOperatorsMainWallet: Effect.void,
      switchToOperatorsBlockCommitmentWallet: Effect.void,
      switchToOperatorsMergingWallet: Effect.void,
      checkReady,
    }),
  );

const healthyLucidLayer = makeLucidLayer(Effect.void);
const unhealthyLucidLayer = makeLucidLayer(
  Effect.fail(
    new SDK.LucidError({
      message: "L1 provider readiness check failed",
      cause: "connection refused",
    }),
  ),
);

const failingSqlLayer = Layer.succeed(
  SqlClient.SqlClient,
  Object.assign(
    () => Effect.fail(new SqlError.SqlError({ message: "connection refused" })),
    {
      withTransaction: <A, E, R>(eff: Effect.Effect<A, E, R>) => eff,
      insert: (obj: unknown) => obj,
      in: (_col: string, vals: readonly unknown[]) => vals,
      literal: (s: string) => s,
    },
  ) as unknown as SqlClient.SqlClient,
);

const runHealthReadyHandler = (
  queue: TxIngressQueueService,
  lucidLayer: Layer.Layer<Lucid>,
  sqlLayer: Layer.Layer<SqlClient.SqlClient>,
) =>
  getHealthReadyHandlerForTesting.pipe(
    Effect.provideService(TxIngressQueue, queue),
    Effect.provide(lucidLayer),
    Effect.provide(sqlLayer),
  );

describe("getHealthReadyHandler", () => {
  it("returns 200 ready when database, redis, and L1 provider are all reachable", async () => {
    const response = await Effect.runPromise(
      runHealthReadyHandler(
        makeQueueStub(),
        healthyLucidLayer,
        createMockSqlHarness().layer,
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(200);
    expect(body).toEqual({ status: "ready" });
  });

  it("returns 503 and reports database when Postgres is down", async () => {
    const response = await Effect.runPromise(
      runHealthReadyHandler(
        makeQueueStub(),
        healthyLucidLayer,
        failingSqlLayer,
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(503);
    expect(body).toEqual({ status: "not_ready", failing: ["database"] });
  });

  it("returns 503 and reports redis when the ingress queue is unreachable", async () => {
    const unhealthyQueue = makeQueueStub({
      ping: Effect.fail(
        new TxIngressQueueError({
          operation: "PING",
          message: "Redis Streams operation failed: PING",
          cause: "connection refused",
        }),
      ),
    });

    const response = await Effect.runPromise(
      runHealthReadyHandler(
        unhealthyQueue,
        healthyLucidLayer,
        createMockSqlHarness().layer,
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(503);
    expect(body).toEqual({ status: "not_ready", failing: ["redis"] });
  });

  it("returns 503 and reports l1Provider when the L1 provider is unreachable", async () => {
    const response = await Effect.runPromise(
      runHealthReadyHandler(
        makeQueueStub(),
        unhealthyLucidLayer,
        createMockSqlHarness().layer,
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = await webResponse.json();

    expect(webResponse.status).toBe(503);
    expect(body).toEqual({ status: "not_ready", failing: ["l1Provider"] });
  });

  it("returns 503 and reports every failing subsystem when all dependencies are down", async () => {
    const unhealthyQueue = makeQueueStub({
      ping: Effect.fail(
        new TxIngressQueueError({
          operation: "PING",
          message: "Redis Streams operation failed: PING",
          cause: "connection refused",
        }),
      ),
    });

    const response = await Effect.runPromise(
      runHealthReadyHandler(
        unhealthyQueue,
        unhealthyLucidLayer,
        failingSqlLayer,
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = (await webResponse.json()) as {
      status: string;
      failing: readonly string[];
    };

    expect(webResponse.status).toBe(503);
    expect(body.status).toBe("not_ready");
    expect(new Set(body.failing)).toEqual(
      new Set(["database", "redis", "l1Provider"]),
    );
  });
});
