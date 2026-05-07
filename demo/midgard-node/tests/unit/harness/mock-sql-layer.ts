import { SqlClient } from "@effect/sql";
import { Effect, Layer } from "effect";

export type MockSqlRow = Record<string, unknown>;

export type SqlInvocation = {
  template: string;
  values: readonly unknown[];
};

export type MockSqlHarness = {
  layer: Layer.Layer<SqlClient.SqlClient>;
  setRows: (rows: readonly MockSqlRow[]) => void;
  reset: () => void;
  getCalls: () => readonly SqlInvocation[];
  getCallCount: () => number;
};

const isTaggedTemplateInput = (
  value: unknown,
): value is TemplateStringsArray => {
  return (
    Array.isArray(value) && Object.prototype.hasOwnProperty.call(value, "raw")
  );
};

export const createMockSqlHarness = (
  initialRows: readonly MockSqlRow[] = [],
): MockSqlHarness => {
  let rows: MockSqlRow[] = [...initialRows];
  const calls: SqlInvocation[] = [];

  const sql = Object.assign(
    (stringsOrStr: unknown, ...values: readonly unknown[]) => {
      if (isTaggedTemplateInput(stringsOrStr)) {
        calls.push({
          template: stringsOrStr.join("?"),
          values,
        });
        return Effect.succeed([...rows]);
      }
      return stringsOrStr;
    },
    {
      withTransaction: <A, E, R>(eff: Effect.Effect<A, E, R>) => eff,
      insert: (obj: unknown) => obj,
      in: (_col: string, vals: readonly unknown[]) => vals,
      literal: (s: string) => s,
    },
  );

  const layer = Layer.succeed(
    SqlClient.SqlClient,
    sql as unknown as SqlClient.SqlClient,
  );

  return {
    layer,
    setRows: (nextRows) => {
      rows = [...nextRows];
    },
    reset: () => {
      rows = [];
      calls.length = 0;
    },
    getCalls: () => calls,
    getCallCount: () => calls.length,
  };
};
