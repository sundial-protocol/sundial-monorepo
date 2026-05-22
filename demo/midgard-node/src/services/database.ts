import { Data, Duration, Effect, Layer, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient } from "@effect/sql";
import { ConfigError, NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";

export class DatabaseInitializationError extends Data.TaggedError(
  "DatabaseInitializationError",
)<SDK.GenericErrorFields> {}

const RPC_POOL_MAX_CONNECTIONS = 20;
const SEQUENCER_POOL_MAX_CONNECTIONS = 5;

const createPgLayer = (
  poolName: "rpc" | "sequencer",
  maxConnections: number,
): Layer.Layer<
  SqlClient.SqlClient,
  DatabaseInitializationError | ConfigError,
  NodeConfig
> =>
  Layer.unwrapEffect(
    Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      yield* Effect.logInfo(
        `📚 Opening PostgreSQL ${poolName} pool (maxConnections=${maxConnections})...`,
      );
      const pgClientLayer = PgClient.layer({
        host: nodeConfig.POSTGRES_HOST,
        username: nodeConfig.POSTGRES_USER,
        password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
        database: nodeConfig.POSTGRES_DB,
        maxConnections,
        idleTimeout: Duration.minutes(5),
        connectTimeout: Duration.seconds(2),
      });
      return Layer.mapError(pgClientLayer, (e) => {
        return new DatabaseInitializationError({
          message: `Failed to initialize PostgreSQL ${poolName} pool`,
          cause: e,
        });
      });
    }).pipe(Effect.orDie),
  );

const RpcSqlClientLive = createPgLayer("rpc", RPC_POOL_MAX_CONNECTIONS);
const SequencerSqlClientLive = createPgLayer(
  "sequencer",
  SEQUENCER_POOL_MAX_CONNECTIONS,
);

export const Database = {
  Rpc: {
    layer: Layer.provide(RpcSqlClientLive, NodeConfig.layer),
  },
  Sequencer: {
    layer: Layer.provide(SequencerSqlClientLive, NodeConfig.layer),
  },
  layer: Layer.provide(RpcSqlClientLive, NodeConfig.layer),
};

export type Database = SqlClient.SqlClient;
