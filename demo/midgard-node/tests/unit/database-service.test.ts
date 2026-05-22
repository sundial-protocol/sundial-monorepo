import { beforeEach, describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { SqlClient } from "@effect/sql";
import { Context, Data, Effect, Layer } from "effect";

type PgLayerConfig = {
  host: string;
  username: string;
  password: unknown;
  database: string;
  maxConnections: number;
};

const pgLayerMock = vi.hoisted(() =>
  vi.fn((config: PgLayerConfig) =>
    Layer.succeed(SqlClient.SqlClient, { config } as never),
  ),
);

vi.mock("@effect/sql-pg", () => ({
  PgClient: {
    layer: pgLayerMock,
  },
}));

vi.mock("@/services/config.js", () => {
  const NodeConfigTag = Context.GenericTag<{
    POSTGRES_HOST: string;
    POSTGRES_USER: string;
    POSTGRES_PASSWORD: string;
    POSTGRES_DB: string;
  }>("@/services/config/NodeConfig");
  class ConfigError extends Data.TaggedError("ConfigError")<{
    message: string;
    cause?: unknown;
    fieldsAndValues?: readonly (readonly [string, string])[];
  }> {}
  return {
    ConfigError,
    NodeConfig: Object.assign(NodeConfigTag, {
      layer: Layer.succeed(NodeConfigTag, {
        POSTGRES_HOST: "postgres",
        POSTGRES_USER: "postgres",
        POSTGRES_PASSWORD: "postgres",
        POSTGRES_DB: "midgard",
      }),
    }),
  };
});

const readSqlClient = <E, R>(layer: Layer.Layer<SqlClient.SqlClient, E, R>) =>
  Effect.gen(function* () {
    return yield* SqlClient.SqlClient;
  }).pipe(Effect.provide(layer));

describe("Database service pools", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.resetModules();
  });

  it.effect(
    "builds separate RPC and Sequencer pools with different maxConnections",
    () =>
      Effect.gen(function* () {
        const { Database } = yield* Effect.promise(
          () => import("@/services/database.js"),
        );

        yield* readSqlClient(Database.Rpc.layer);
        yield* readSqlClient(Database.Sequencer.layer);

        expect(pgLayerMock).toHaveBeenCalledTimes(2);
        expect(pgLayerMock.mock.calls[0]?.[0]).toMatchObject({
          maxConnections: 20,
        });
        expect(pgLayerMock.mock.calls[1]?.[0]).toMatchObject({
          maxConnections: 5,
        });
      }),
  );
});
