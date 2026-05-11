import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";

vi.mock("@/database/utils/tx.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: { TX_ID: "tx_id", TX: "tx", TIMESTAMPTZ: "time_stamp_tz" },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    retrieveValue: vi.fn(() => E.succeed(undefined)),
    retrieveValues: vi.fn(() => E.succeed([])),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    delMultiple: vi.fn(() => E.succeed(undefined)),
  };
});

import * as Tx from "@/database/utils/tx.js";
import * as ImmutableDB from "@/database/immutable.js";

const txIdA = Buffer.alloc(32, 0xaa);
const txCborA = Buffer.alloc(64, 0xbb);
const txIdB = Buffer.alloc(32, 0xcc);
const txCborB = Buffer.alloc(64, 0xdd);

const noopSqlLayer = Layer.succeed(
  SqlClient.SqlClient,
  {} as SqlClient.SqlClient,
);

describe("ImmutableDB", () => {
  it.effect("insertTx inserts one tx", () => {
    const entry = { tx_id: txIdA, tx: txCborA };
    return ImmutableDB.insertTx(entry as any).pipe(
      Effect.map(() => {
        expect(vi.mocked(Tx.insertEntry)).toHaveBeenCalledWith(
          "immutable",
          entry,
        );
      }),
      Effect.provide(noopSqlLayer),
    );
  });

  it.effect("insertTxs inserts multiple txs", () => {
    const entries = [
      { tx_id: txIdA, tx: txCborA },
      { tx_id: txIdB, tx: txCborB },
    ];
    return ImmutableDB.insertTxs(entries as any[]).pipe(
      Effect.map(() => {
        expect(vi.mocked(Tx.insertEntries)).toHaveBeenCalledWith(
          "immutable",
          entries,
        );
      }),
      Effect.provide(noopSqlLayer),
    );
  });

  it.effect("retrieveTxCborsByHashes retrieves by hash list", () => {
    vi.mocked(Tx.retrieveValues).mockReturnValue(
      Effect.succeed([txCborA, txCborB]),
    );
    return ImmutableDB.retrieveTxCborsByHashes([txIdA, txIdB] as any[]).pipe(
      Effect.map((result) => {
        expect(result.length).toBe(2);
        expect(vi.mocked(Tx.retrieveValues)).toHaveBeenCalledWith("immutable", [
          txIdA,
          txIdB,
        ]);
      }),
      Effect.provide(noopSqlLayer),
    );
  });
});
