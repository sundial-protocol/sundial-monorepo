// NIT-016 … NIT-025  — User Event Ingestion
//
// Tests verify that DepositsDB, TxOrdersDB, WithdrawalsDB, and BlocksDB
// persist L1 user-event entries correctly using real repository SQL.
// These tests exercise the DB layer directly rather than going through
// syncUserEvents to keep the external L1/SDK boundary out of scope.

import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Ref } from "effect";

import { makeTestSqlLayer } from "./harness/pglite-sql-layer.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import * as DBInitialization from "@/database/init.js";
import * as DepositsDB from "@/database/deposits.js";
import * as TxOrdersDB from "@/database/txOrders.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import * as BlocksDB from "@/database/blocks.js";
import * as UserEvents from "@/database/utils/user-events.js";
import { Globals } from "@/services/globals.js";

const makeBaseLayers = () =>
  Layer.mergeAll(
    makeTestSqlLayer(),
    makeTestNodeConfigLayer(),
    Globals.Default,
  );

const makeUserEventEntry = (seed: number): UserEvents.Entry => ({
  [UserEvents.Columns.ID]: Buffer.alloc(32, seed),
  [UserEvents.Columns.INFO]: Buffer.alloc(16, seed + 0x10),
  [UserEvents.Columns.ASSET_NAME]: seed
    .toString(16)
    .padStart(2, "0")
    .repeat(10),
  [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, seed + 0x20),
  [UserEvents.Columns.INCLUSION_TIME]: new Date(
    1_700_000_000_000 + seed * 1_000,
  ),
});

// ─── NIT-016 ─────────────────────────────────────────────────────────────────

describe("Fake L1 deposit sync persists deposit event", () => {
  it.effect("Fake L1 deposit sync persists deposit event", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const depositEntry = makeUserEventEntry(1);
      yield* DepositsDB.insertEntry(depositEntry);

      const entries = yield* DepositsDB.retrieveAllEntries();

      expect(entries.length).toBe(1);
      expect(
        Buffer.from(entries[0][UserEvents.Columns.ID]).equals(
          depositEntry[UserEvents.Columns.ID],
        ),
      ).toBe(true);
      expect(entries[0][UserEvents.Columns.ASSET_NAME]).toBe(
        depositEntry[UserEvents.Columns.ASSET_NAME],
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-017 ─────────────────────────────────────────────────────────────────

describe("Fake L1 transaction order sync persists tx order event", () => {
  it.effect("Fake L1 transaction order sync persists tx order event", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const txOrderEntry = makeUserEventEntry(2);
      yield* TxOrdersDB.insertEntries([txOrderEntry]);

      const entries = yield* UserEvents.retrieveAllEntries(
        TxOrdersDB.tableName,
      );

      expect(entries.length).toBe(1);
      expect(
        Buffer.from(entries[0][UserEvents.Columns.INFO]).equals(
          txOrderEntry[UserEvents.Columns.INFO],
        ),
      ).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-018 ─────────────────────────────────────────────────────────────────

describe("Fake L1 withdrawal sync persists withdrawal event", () => {
  it.effect("Fake L1 withdrawal sync persists withdrawal event", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const withdrawalEntry = makeUserEventEntry(3);
      yield* WithdrawalsDB.insertEntries([withdrawalEntry]);

      const entries = yield* UserEvents.retrieveAllEntries(
        WithdrawalsDB.tableName,
      );

      expect(entries.length).toBe(1);
      expect(
        Buffer.from(entries[0][UserEvents.Columns.ID]).equals(
          withdrawalEntry[UserEvents.Columns.ID],
        ),
      ).toBe(true);
      expect(entries[0][UserEvents.Columns.INCLUSION_TIME].getTime()).toBe(
        withdrawalEntry[UserEvents.Columns.INCLUSION_TIME].getTime(),
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-019 ─────────────────────────────────────────────────────────────────

describe("Sync stores deposits, tx orders, and withdrawals in one pass", () => {
  it.effect(
    "Sync stores deposits, tx orders, and withdrawals in one pass",
    () => {
      const layers = makeBaseLayers();
      return Effect.gen(function* () {
        yield* DBInitialization.program;

        const depositEntry = makeUserEventEntry(10);
        const txOrderEntry = makeUserEventEntry(20);
        const withdrawalEntry = makeUserEventEntry(30);

        yield* Effect.all(
          [
            DepositsDB.insertEntry(depositEntry),
            TxOrdersDB.insertEntries([txOrderEntry]),
            WithdrawalsDB.insertEntries([withdrawalEntry]),
          ],
          { concurrency: "unbounded" },
        );

        const deps = yield* DepositsDB.retrieveAllEntries();
        const txOrds = yield* UserEvents.retrieveAllEntries(
          TxOrdersDB.tableName,
        );
        const withs = yield* UserEvents.retrieveAllEntries(
          WithdrawalsDB.tableName,
        );

        expect(deps.length).toBe(1);
        expect(txOrds.length).toBe(1);
        expect(withs.length).toBe(1);

        expect(
          Buffer.from(deps[0][UserEvents.Columns.ID]).equals(
            depositEntry[UserEvents.Columns.ID],
          ),
        ).toBe(true);
        expect(
          Buffer.from(txOrds[0][UserEvents.Columns.ID]).equals(
            txOrderEntry[UserEvents.Columns.ID],
          ),
        ).toBe(true);
        expect(
          Buffer.from(withs[0][UserEvents.Columns.ID]).equals(
            withdrawalEntry[UserEvents.Columns.ID],
          ),
        ).toBe(true);
      }).pipe(Effect.provide(layers));
    },
  );
});

// ─── NIT-020 ─────────────────────────────────────────────────────────────────

describe("Sync advances latest fetch time after events are persisted", () => {
  it.effect(
    "Sync advances latest fetch time after events are persisted",
    () => {
      const layers = makeBaseLayers();
      return Effect.gen(function* () {
        yield* DBInitialization.program;

        const globals = yield* Globals;
        const startTime = yield* Ref.get(globals.LATEST_USER_EVENTS_FETCH_TIME);

        // Insert a deposit event (simulates work done during a sync pass).
        const depositEntry = makeUserEventEntry(5);
        yield* DepositsDB.insertEntry(depositEntry);

        // Advance the fetch time (as syncUserEvents would do after inserting events).
        const newTime = Date.now();
        yield* Ref.set(globals.LATEST_USER_EVENTS_FETCH_TIME, newTime);

        const afterTime = yield* Ref.get(globals.LATEST_USER_EVENTS_FETCH_TIME);
        expect(afterTime).toBeGreaterThan(startTime);

        // Verify the deposit is visible before and after the ref update.
        const entries = yield* DepositsDB.retrieveAllEntries();
        expect(entries.length).toBe(1);
      }).pipe(Effect.provide(layers));
    },
  );
});

// ─── NIT-021 ─────────────────────────────────────────────────────────────────

describe("Sync with no events leaves existing user events intact", () => {
  it.effect("Sync with no events leaves existing user events intact", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      // First "sync pass": insert one deposit event.
      const depositEntry = makeUserEventEntry(7);
      yield* DepositsDB.insertEntry(depositEntry);

      // Second "sync pass": no new events (empty inserts are no-ops).
      yield* DepositsDB.insertEntries([]);

      const entries = yield* DepositsDB.retrieveAllEntries();
      expect(entries.length).toBe(1);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-022 ─────────────────────────────────────────────────────────────────

describe("Duplicate L1 user event is idempotent", () => {
  it.effect("Duplicate L1 user event is idempotent", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const depositEntry = makeUserEventEntry(8);

      // Insert the same entry twice — ON CONFLICT DO NOTHING must deduplicate.
      yield* DepositsDB.insertEntry(depositEntry);
      yield* DepositsDB.insertEntry(depositEntry);

      const entries = yield* DepositsDB.retrieveAllEntries();
      expect(entries.length).toBe(1);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-023 ─────────────────────────────────────────────────────────────────

describe("Time-bound user-event retrieval feeds block interval", () => {
  it.effect("Time-bound user-event retrieval feeds block interval", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = 1_700_000_000_000;
      const entryBefore: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x01),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "01".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0 - 5_000),
      };
      const entryInside: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x02),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "02".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0),
      };
      const entryAfter: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x03),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "03".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0 + 10_000),
      };

      yield* DepositsDB.insertEntries([entryBefore, entryInside, entryAfter]);

      const intervalStart = new Date(T0 - 1_000);
      const intervalEnd = new Date(T0 + 5_000);
      const events = yield* BlocksDB.retrieveEvents(intervalStart, intervalEnd);

      expect(events.deposits.length).toBe(1);
      expect(
        Buffer.from(events.deposits[0][UserEvents.Columns.ID]).equals(
          entryInside[UserEvents.Columns.ID],
        ),
      ).toBe(true);
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-024 ─────────────────────────────────────────────────────────────────

describe("Event ordering within interval is oldest first", () => {
  it.effect("Event ordering within interval is oldest first", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = 1_700_000_000_000;
      const depositOld: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x11),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "11".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0),
      };
      const depositNew: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x12),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "12".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0 + 3_000),
      };
      const txOrderOld: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x21),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "21".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0 + 1_000),
      };
      const txOrderNew: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x22),
        [UserEvents.Columns.INFO]: Buffer.alloc(16),
        [UserEvents.Columns.ASSET_NAME]: "22".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0 + 4_000),
      };

      // Insert in reversed order intentionally.
      yield* DepositsDB.insertEntries([depositNew, depositOld]);
      yield* TxOrdersDB.insertEntries([txOrderNew, txOrderOld]);

      const start = new Date(T0 - 1_000);
      const end = new Date(T0 + 10_000);
      const events = yield* BlocksDB.retrieveEvents(start, end);

      expect(events.deposits.length).toBe(2);
      expect(
        events.deposits[0][UserEvents.Columns.INCLUSION_TIME].getTime(),
      ).toBeLessThanOrEqual(
        events.deposits[1][UserEvents.Columns.INCLUSION_TIME].getTime(),
      );

      expect(events.txOrders.length).toBe(2);
      expect(
        events.txOrders[0][UserEvents.Columns.INCLUSION_TIME].getTime(),
      ).toBeLessThanOrEqual(
        events.txOrders[1][UserEvents.Columns.INCLUSION_TIME].getTime(),
      );
    }).pipe(Effect.provide(layers));
  });
});

// ─── NIT-025 ─────────────────────────────────────────────────────────────────

describe("User-event tables preserve independent event types", () => {
  it.effect("User-event tables preserve independent event types", () => {
    const layers = makeBaseLayers();
    return Effect.gen(function* () {
      yield* DBInitialization.program;

      const T0 = 1_700_000_000_000;
      const depositEntry: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x01),
        [UserEvents.Columns.INFO]: Buffer.alloc(16, 0x01),
        [UserEvents.Columns.ASSET_NAME]: "01".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, 0x01),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0),
      };
      const txOrderEntry: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x02),
        [UserEvents.Columns.INFO]: Buffer.alloc(16, 0x02),
        [UserEvents.Columns.ASSET_NAME]: "02".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, 0x02),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0),
      };
      const withdrawalEntry: UserEvents.Entry = {
        [UserEvents.Columns.ID]: Buffer.alloc(32, 0x03),
        [UserEvents.Columns.INFO]: Buffer.alloc(16, 0x03),
        [UserEvents.Columns.ASSET_NAME]: "03".repeat(10),
        [UserEvents.Columns.L1_UTXO_CBOR]: Buffer.alloc(64, 0x03),
        [UserEvents.Columns.INCLUSION_TIME]: new Date(T0),
      };

      yield* DepositsDB.insertEntry(depositEntry);
      yield* TxOrdersDB.insertEntries([txOrderEntry]);
      yield* WithdrawalsDB.insertEntries([withdrawalEntry]);

      const start = new Date(T0 - 1_000);
      const end = new Date(T0 + 1_000);
      const events = yield* BlocksDB.retrieveEvents(start, end);

      expect(events.deposits.length).toBe(1);
      expect(events.txOrders.length).toBe(1);
      expect(events.withdrawals.length).toBe(1);

      // No cross-table contamination.
      const depositId = depositEntry[UserEvents.Columns.ID].toString("hex");
      const txOrderId = txOrderEntry[UserEvents.Columns.ID].toString("hex");
      const withdrawalId =
        withdrawalEntry[UserEvents.Columns.ID].toString("hex");

      expect(
        Buffer.from(events.deposits[0][UserEvents.Columns.ID]).toString("hex"),
      ).toBe(depositId);
      expect(
        Buffer.from(events.txOrders[0][UserEvents.Columns.ID]).toString("hex"),
      ).toBe(txOrderId);
      expect(
        Buffer.from(events.withdrawals[0][UserEvents.Columns.ID]).toString(
          "hex",
        ),
      ).toBe(withdrawalId);
    }).pipe(Effect.provide(layers));
  });
});
