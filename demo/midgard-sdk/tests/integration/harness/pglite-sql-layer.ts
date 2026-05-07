import { Context, Layer } from "effect";

// Lightweight SQL service contract used by SDK integration harnesses.
// This intentionally avoids pulling in extra SQL adapter packages here.
export type TestSqlClient = {
  mode: "memory" | "path";
  database: string;
};

export const TestSqlClientTag = Context.GenericTag<TestSqlClient>(
  "sdk.tests/SqlClient",
);

/** Returns an isolated in-memory SQL layer placeholder for integration tests. */
export const makeTestSqlLayer = (): Layer.Layer<TestSqlClient> =>
  Layer.succeed(TestSqlClientTag, {
    mode: "memory",
    database: "memory://",
  });

/** Returns a filesystem-path SQL layer placeholder for reopen/persistence tests. */
export const makeTestSqlLayerWithPath = (
  dbPath: string,
): Layer.Layer<TestSqlClient> =>
  Layer.succeed(TestSqlClientTag, {
    mode: "path",
    database: dbPath,
  });
