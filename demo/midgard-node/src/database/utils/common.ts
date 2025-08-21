import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

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

export type DatabaseError =
  | SelectError
  | InsertError
  | UpdateError
  | DeleteError
  | CreateTableError
  | OtherDatabaseError;

export class SelectError extends Data.TaggedError("SelectError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

export class InsertError extends Data.TaggedError("InsertError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

export class UpdateError extends Data.TaggedError("UpdateError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

export class DeleteError extends Data.TaggedError("DeleteError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

export class CreateTableError extends Data.TaggedError("CreateTableError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

export class OtherDatabaseError extends Data.TaggedError("OtherDatabaseError")<{
  readonly message: string;
  readonly table?: string;
  readonly cause?: unknown;
}> {}

/**
 * Helper function to map SQL errors to DatabaseError for createTable operations
 * @param tableName - Name of the table being created
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapCreateTableError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? new CreateTableError({
          message: `Failed to create table ${tableName}`,
          table: tableName,
          cause: error,
        })
      : new CreateTableError({
          message: `Unknown error during table creation: ${error}`,
          table: tableName,
          cause: error,
        }),
  );

/**
 * Helper function to map SQL errors to DatabaseError for select operations
 * @param tableName - Name of the table being queried
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapSelectError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? new SelectError({
          message: `Failed to select from table ${tableName}`,
          table: tableName,
          cause: error,
        })
      : new SelectError({
          message: `Unknown error during select: ${error}`,
          table: tableName,
          cause: error,
        }),
  );

/**
 * Helper function to map SQL errors to DatabaseError for insert operations
 * @param tableName - Name of the table being inserted into
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapInsertError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? new InsertError({
          message: `Failed to insert into table ${tableName}`,
          table: tableName,
          cause: error,
        })
      : new InsertError({
          message: `Unknown error during insert: ${error}`,
          table: tableName,
          cause: error,
        }),
  );

/**
 * Helper function to map SQL errors to DatabaseError for update operations
 * @param tableName - Name of the table being updated
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapUpdateError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? new UpdateError({
          message: `Failed to update table ${tableName}`,
          table: tableName,
          cause: error,
        })
      : new UpdateError({
          message: `Unknown error during update: ${error}`,
          table: tableName,
          cause: error,
        }),
  );

/**
 * Helper function to map SQL errors to DatabaseError for delete operations
 * @param tableName - Name of the table being deleted from
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapDeleteError = (tableName: string) =>
  Effect.mapError((error: unknown) =>
    error instanceof SqlError.SqlError
      ? new DeleteError({
          message: `Failed to delete from table ${tableName}`,
          table: tableName,
          cause: error,
        })
      : new DeleteError({
          message: `Unknown error during delete: ${error}`,
          table: tableName,
          cause: error,
        }),
  );
