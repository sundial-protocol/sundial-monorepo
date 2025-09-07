import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { GenericErrorFields } from "@/utils.js";

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DBDeleteError, Database> =>
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

type DBErrorFields = GenericErrorFields & {
  readonly table?: string;
};

export class DBSelectError extends Data.TaggedError(
  "DBSelectError",
)<DBErrorFields> {}
export class DBInsertError extends Data.TaggedError(
  "DBInsertError",
)<DBErrorFields> {}
export class DBUpdateError extends Data.TaggedError(
  "DBUpdateError",
)<DBErrorFields> {}
export class DBDeleteError extends Data.TaggedError(
  "DBDeleteError",
)<DBErrorFields> {}
export class DBCreateError extends Data.TaggedError(
  "DBCreateError",
)<DBErrorFields> {}
export class DBOtherError extends Data.TaggedError(
  "DBOtherError",
)<DBErrorFields> {}

type DBErrorConstructor<T> = new (args: DBErrorFields) => T;

const createMapError = <E>(
  ErrorClass: DBErrorConstructor<E>,
  action: string,
): ((
  tableName: string,
) => <A, R>(self: Effect.Effect<A, unknown, R>) => Effect.Effect<A, E, R>) => {
  return (tableName: string) =>
    Effect.mapError((error: unknown) =>
      error instanceof SqlError.SqlError
        ? new ErrorClass({
            message: `Failed to ${action}`,
            table: tableName,
            cause: error,
          })
        : new ErrorClass({
            message: `Unknown error during ${action}`,
            table: tableName,
            cause: error,
          }),
    );
};

/**
 * Helper function to map SQL errors to DatabaseError for select operations
 * @param tableName - Name of the table being queried
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapSelectError = createMapError(
  DBSelectError,
  "select from table",
);

/**
 * Helper function to map SQL errors to DatabaseError for insert operations
 * @param tableName - Name of the table being inserted into
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapInsertError = createMapError(
  DBInsertError,
  "insert into table",
);

/**
 * Helper function to map SQL errors to DatabaseError for update operations
 * @param tableName - Name of the table being updated
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapUpdateError = createMapError(DBUpdateError, "update table");

/**
 * Helper function to map SQL errors to DatabaseError for delete operations
 * @param tableName - Name of the table being deleted from
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapDeleteError = createMapError(
  DBDeleteError,
  "delete from table",
);

/**
 * Helper function to map SQL errors to DatabaseError for createTable operations
 * @param tableName - Name of the table being created
 * @returns Effect that transforms SqlError to DatabaseError
 */
export const mapCreateTableError = createMapError(
  DBCreateError,
  "create table",
);
