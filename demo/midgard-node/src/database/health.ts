import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { Database } from "@/services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "health";

export const checkReady: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`SELECT 1`;
  }).pipe(
    Effect.withLogSpan("database health check"),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `health db: readiness check failed: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed database readiness check"),
    Effect.asVoid,
  );
