import dotenv from "dotenv";
import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";
import { Lucid } from "@/services/lucid.js";
import { Effect, Layer } from "effect";
import { makeTestSqlLayer } from "./integration/harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./integration/harness/node-config-layer.js";
import * as InitDB from "../src/database/init.js";

dotenv.config({ path: ".env" });

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

// Database-only layers backed by an in-process PGlite instance.
// Each call creates a fresh isolated database with the schema pre-initialized,
// so callers can run DB operations without a real PostgreSQL server.
export const provideDbOnlyLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) => {
  const layers = Layer.mergeAll(makeTestSqlLayer(), makeTestNodeConfigLayer());
  return InitDB.program.pipe(Effect.andThen(eff), Effect.provide(layers));
};
