import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { Layer } from "effect";
import { SqlClient } from "@effect/sql";

// Mock all createTable calls - init.ts calls each one
vi.mock("@/database/addressHistory.js", async () => {
  const { Effect: E } = await import("effect");
  return { createTable: vi.fn(() => E.succeed(undefined)), tableName: "address_history", Columns: {}, Status: {}, EventType: {} };
});
vi.mock("@/database/blocks.js", async () => {
  const { Effect: E } = await import("effect");
  return { createTable: vi.fn(() => E.succeed(undefined)), tableName: "blocks" };
});
vi.mock("@/database/blocksTxs.js", async () => {
  const { Effect: E } = await import("effect");
  return { createTable: vi.fn(() => E.succeed(undefined)), tableName: "blocks_txs", Columns: {}, ColumnsIndices: {} };
});
vi.mock("@/database/confirmedLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "confirmed_ledger", insertMultiple: vi.fn(() => E.succeed(undefined)), retrieve: E.succeed([]), clearUTxOs: vi.fn(() => E.succeed(undefined)), clear: E.succeed(undefined) };
});
vi.mock("@/database/latestLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "latest_ledger", insertMultiple: vi.fn(() => E.succeed(undefined)), retrieveByOutRef: vi.fn(() => E.succeed(undefined)), retrieveEntries: vi.fn(() => E.succeed([])), retrieve: E.succeed([]), retrieveNoTimeStamps: E.succeed([]), clearUTxOs: vi.fn(() => E.succeed(undefined)), clear: E.succeed(undefined) };
});
vi.mock("@/database/mempoolLedger.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "mempool_ledger", insert: vi.fn(() => E.succeed(undefined)), retrieve: E.succeed([]), retrieveByAddress: vi.fn(() => E.succeed([])), retrieveByOutRefs: vi.fn(() => E.succeed([])), retrieveByOutRef: vi.fn(() => E.succeed(undefined)), clearUTxOs: vi.fn(() => E.succeed(undefined)), clear: E.succeed(undefined) };
});
vi.mock("@/database/immutable.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "immutable", insertTx: vi.fn(() => E.succeed(undefined)), insertTxs: vi.fn(() => E.succeed(undefined)), retrieve: E.succeed([]), retrieveTxCborByHash: vi.fn(() => E.succeed(undefined)), retrieveTxCborsByHashes: vi.fn(() => E.succeed([])), clearTxs: vi.fn(() => E.succeed(undefined)), clear: E.succeed(undefined) };
});
vi.mock("@/database/mempool.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "mempool", insertMultiple: vi.fn(() => E.succeed(undefined)), retrieveTxCborByHash: vi.fn(() => E.succeed(undefined)), retrieveTxCborsByHashes: vi.fn(() => E.succeed([])), retrieve: E.succeed([]), retrieveTimeBoundEntries: vi.fn(() => E.succeed([])), retrieveTxCount: E.succeed(0n), clearTxs: vi.fn(() => E.succeed(undefined)), clear: E.succeed(undefined) };
});
vi.mock("@/database/deposits.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "deposits_utxos", insertEntry: vi.fn(() => E.succeed(undefined)), insertEntries: vi.fn(() => E.succeed(undefined)), retrieveTimeBoundEntries: vi.fn(() => E.succeed([])), retrieveAllEntries: vi.fn(() => E.succeed([])), delEntries: vi.fn(() => E.succeed(undefined)) };
});
vi.mock("@/database/txOrders.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "transaction_order_utxos", insertEntries: vi.fn(() => E.succeed(undefined)), retrieveTimeBoundEntries: vi.fn(() => E.succeed([])) };
});
vi.mock("@/database/withdrawals.js", async () => {
  const { Effect: E } = await import("effect");
  return { tableName: "withdrawal_order_utxos", insertEntries: vi.fn(() => E.succeed(undefined)), retrieveTimeBoundEntries: vi.fn(() => E.succeed([]) ) };
});
vi.mock("@/database/utils/ledger.js", async () => ({
  Columns: { TX_ID: "tx_id", OUTREF: "outref", OUTPUT: "output", ADDRESS: "address", TIMESTAMPTZ: "time_stamp_tz" },
  createTable: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  insertEntries: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveAllEntries: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  retrieveByOutRef: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveByOutRefs: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  retrieveEntriesWithAddress: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  delEntries: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveAllEntriesNoTimeStamps: vi.fn(async () => (await import("effect")).Effect.succeed([])),
}));
vi.mock("@/database/utils/tx.js", async () => ({
  Columns: { TX_ID: "tx_id", TX: "tx", TIMESTAMPTZ: "time_stamp_tz" },
  createTable: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  insertEntry: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  insertEntries: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveAllEntries: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  retrieveValue: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveValues: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  retrieveTimeBoundEntries: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  delMultiple: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
}));
vi.mock("@/database/utils/user-events.js", async () => ({
  Columns: { ID: "event_id", INFO: "event_info", ASSET_NAME: "asset_name", L1_UTXO_CBOR: "l1_utxo_cbor", INCLUSION_TIME: "inclusion_time" },
  createTable: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  insertEntry: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  insertEntries: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
  retrieveTimeBoundEntries: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  retrieveAllEntries: vi.fn(async () => (await import("effect")).Effect.succeed([])),
  delEntries: vi.fn(async () => (await import("effect")).Effect.succeed(undefined)),
}));

import * as DBInitialization from "@/database/init.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as BlocksDB from "@/database/blocks.js";
import * as BlocksTxsDB from "@/database/blocksTxs.js";

// Minimal mock SqlClient for the SET statements in init.ts
const mockSql = Object.assign(
  (_strings: TemplateStringsArray, ..._values: unknown[]) =>
    Effect.succeed([]),
  {
    withTransaction: (eff: Effect.Effect<unknown>) => eff,
    insert: (obj: unknown) => obj,
    in: (_col: string, vals: unknown[]) => vals,
    literal: (s: string) => s,
  },
);
const mockDbLayer = Layer.succeed(
  SqlClient.SqlClient,
  mockSql as unknown as SqlClient.SqlClient,
);

describe("Database initialization creates schema", () => {
  it.effect("Database initialization creates schema", () =>
    DBInitialization.program.pipe(
      Effect.provide(mockDbLayer),
      Effect.map(() => {
        expect(vi.mocked(AddressHistoryDB.createTable)).toHaveBeenCalled();
        expect(vi.mocked(BlocksDB.createTable)).toHaveBeenCalled();
        expect(vi.mocked(BlocksTxsDB.createTable)).toHaveBeenCalled();
      }),
    ),
  );
});
