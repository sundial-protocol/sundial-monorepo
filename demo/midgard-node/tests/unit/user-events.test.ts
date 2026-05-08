import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";

vi.mock("@/database/utils/user-events.js", async () => {
  const { Effect: E } = await import("effect");
  return {
    Columns: {
      ID: "event_id",
      INFO: "event_info",
      ASSET_NAME: "asset_name",
      L1_UTXO_CBOR: "l1_utxo_cbor",
      INCLUSION_TIME: "inclusion_time",
    },
    createTable: vi.fn(() => E.succeed(undefined)),
    insertEntry: vi.fn(() => E.succeed(undefined)),
    insertEntries: vi.fn(() => E.succeed(undefined)),
    retrieveTimeBoundEntries: vi.fn(() => E.succeed([])),
    retrieveAllEntries: vi.fn(() => E.succeed([])),
    delEntries: vi.fn(() => E.succeed(undefined)),
  };
});

import * as UserEvents from "@/database/utils/user-events.js";
import * as DepositsDB from "@/database/deposits.js";
import * as TxOrdersDB from "@/database/txOrders.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";

const eventId = Buffer.alloc(32, 0xaa);
const eventInfo = Buffer.alloc(32, 0xbb);
const l1UxoCbor = Buffer.alloc(64, 0xcc);
const inclusionTime = new Date("2024-06-01T00:00:00Z");

const testEntry: UserEvents.Entry = {
  event_id: eventId,
  event_info: eventInfo,
  asset_name: "aabbcc",
  l1_utxo_cbor: l1UxoCbor,
  inclusion_time: inclusionTime,
};

const noopSqlLayer = Layer.succeed(
  SqlClient.SqlClient,
  {} as SqlClient.SqlClient,
);

beforeEach(() => {
  vi.clearAllMocks();
});

describe("DepositsDB", () => {
  it.effect("insertEntry delegates to user-events with deposits table", () =>
    DepositsDB.insertEntry(testEntry).pipe(
      Effect.map(() => {
        expect(vi.mocked(UserEvents.insertEntry)).toHaveBeenCalledWith(
          "deposits_utxos",
          testEntry,
        );
      }),
      Effect.provide(noopSqlLayer),
    ),
  );

  it.effect("insertEntry called twice for duplicates", () =>
    DepositsDB.insertEntry(testEntry).pipe(
      Effect.andThen(() => DepositsDB.insertEntry(testEntry)),
      Effect.map(() => {
        expect(vi.mocked(UserEvents.insertEntry)).toHaveBeenCalledTimes(2);
      }),
      Effect.provide(noopSqlLayer),
    ),
  );

  it.effect(
    "retrieveTimeBoundEntries delegates to user-events with dates",
    () => {
      const start = new Date("2024-01-01T00:00:00Z");
      const end = new Date("2024-12-31T23:59:59Z");
      vi.mocked(UserEvents.retrieveTimeBoundEntries).mockReturnValue(
        Effect.succeed([testEntry]),
      );
      return DepositsDB.retrieveTimeBoundEntries(start, end).pipe(
        Effect.map((entries) => {
          expect(entries.length).toBe(1);
          expect(
            vi.mocked(UserEvents.retrieveTimeBoundEntries),
          ).toHaveBeenCalledWith("deposits_utxos", start, end);
        }),
        Effect.provide(noopSqlLayer),
      );
    },
  );
});

describe("WithdrawalsDB", () => {
  it.effect(
    "insertEntries delegates to user-events with withdrawals table",
    () => {
      const entries = [
        testEntry,
        { ...testEntry, event_id: Buffer.alloc(32, 0xdd) },
      ];
      return WithdrawalsDB.insertEntries(entries).pipe(
        Effect.map(() => {
          expect(vi.mocked(UserEvents.insertEntries)).toHaveBeenCalledWith(
            "withdrawal_order_utxos",
            entries,
          );
        }),
        Effect.provide(noopSqlLayer),
      );
    },
  );
});

describe("TxOrdersDB", () => {
  it.effect(
    "insertEntries delegates to user-events with tx orders table",
    () => {
      const entries = [testEntry];
      return TxOrdersDB.insertEntries(entries).pipe(
        Effect.map(() => {
          expect(vi.mocked(UserEvents.insertEntries)).toHaveBeenCalledWith(
            "transaction_order_utxos",
            entries,
          );
        }),
        Effect.provide(noopSqlLayer),
      );
    },
  );
});
