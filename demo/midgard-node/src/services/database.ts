import { Effect, Layer, Redacted } from "effect";
import { SqlClient } from "@effect/sql";
import { NodeConfigDep } from "@/config.js";
import { PgClient } from "@effect/sql-pg";

export type Database = SqlClient.SqlClient;

export const mkPgConfig = (nodeConfig: NodeConfigDep) => {
  return {
    host: nodeConfig.POSTGRES_HOST,
    username: nodeConfig.POSTGRES_USER,
    password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
    database: nodeConfig.POSTGRES_DB,
    maxConnections: 20,
    idleTimeout: 30_000,
    connectTimeout: 2_000,
    trace: "all"
  };
}