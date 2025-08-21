import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Data, Effect } from "effect";

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: clearing error: ${JSON.stringify(e)}`),
    ),
    mapDeleteError(tableName),
  );

  export class DatabaseError extends Data.TaggedError("DatabaseError")<{
  readonly message: string;
  readonly operation:
    | "select"
    | "insert"
    | "update"
    | "delete"
    | "createTable"
    | "other";
  readonly table?: string;
  readonly cause?: unknown;
}> {
  static select(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({ message, operation: "select", table, cause });
  }

  static insert(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({ message, operation: "insert", table, cause });
  }

  static update(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({ message, operation: "update", table, cause });
  }

  static delete(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({ message, operation: "delete", table, cause });
  }

  static createTable(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({
      message,
      operation: "createTable",
      table,
      cause,
    });
  }

  static other(message: string, table?: string, cause?: unknown) {
    return new DatabaseError({ message, operation: "other", table, cause });
  }
}

/**
 * Helper function to map SQL errors to DatabaseError for createTable operations
 * @param tableName - Name of the table being created
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapCreateTableError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? DatabaseError.createTable(
          `Failed to create table ${tableName}`,
          tableName,
          error,
        )
      : DatabaseError.createTable(
          `Unknown error during table creation: ${error}`,
          tableName,
          error,
        ),
  );

/**
 * Helper function to map SQL errors to DatabaseError for select operations
 * @param tableName - Name of the table being queried
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapSelectError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? DatabaseError.select(
          `Failed to select from table ${tableName}`,
          tableName,
          error,
        )
      : DatabaseError.select(
          `Unknown error during select: ${error}`,
          tableName,
          error,
        ),
  );

/**
 * Helper function to map SQL errors to DatabaseError for insert operations
 * @param tableName - Name of the table being inserted into
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapInsertError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? DatabaseError.insert(
          `Failed to insert into table ${tableName}`,
          tableName,
          error,
        )
      : DatabaseError.insert(
          `Unknown error during insert: ${error}`,
          tableName,
          error,
        ),
  );

/**
 * Helper function to map SQL errors to DatabaseError for update operations
 * @param tableName - Name of the table being updated
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapUpdateError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? DatabaseError.update(
          `Failed to update table ${tableName}`,
          tableName,
          error,
        )
      : DatabaseError.update(
          `Unknown error during update: ${error}`,
          tableName,
          error,
        ),
  );

/**
 * Helper function to map SQL errors to DatabaseError for delete operations
 * @param tableName - Name of the table being deleted from
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapDeleteError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? DatabaseError.delete(
          `Failed to delete from table ${tableName}`,
          tableName,
          error,
        )
      : DatabaseError.delete(
          `Unknown error during delete: ${error}`,
          tableName,
          error,
        ),
  );
