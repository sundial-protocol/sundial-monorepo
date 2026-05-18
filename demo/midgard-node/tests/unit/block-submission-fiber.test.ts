import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Metric, Option } from "effect";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { Lucid } from "@/services/lucid.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

// Hoisted so values can be swapped per test.
const retrieveFn = vi.hoisted(() => vi.fn());
const setStatusFn = vi.hoisted(() => vi.fn());

vi.mock("@/database/index.js", () => ({
  BlocksDB: {
    get retrieveEarliestUnsubmittedEntry() {
      return retrieveFn();
    },
    setStatusOfEntry: (...args: unknown[]) => setStatusFn(...args),
    retrieveEvents: () =>
      Effect.succeed({
        withdrawals: [],
        txOrders: [],
        txRequests: [],
        deposits: [],
      }),
    Columns: {
      L1_CBOR: "l1_cbor",
      EVENT_START_TIME: "event_start_time",
      EVENT_END_TIME: "event_end_time",
      HEADER_HASH: "header_hash",
    },
    Status: { SUBMITTED: 1 },
  },
  LatestLedgerDB: {
    tableName: "latest_ledger",
    insertMultiple: () => Effect.succeed(undefined),
    clearUTxOs: () => Effect.succeed(undefined),
  },
  MempoolDB: { clearTxs: () => Effect.succeed(undefined) },
  ImmutableDB: { insertTxs: () => Effect.succeed(undefined) },
  BlocksTxsDB: { insert: () => Effect.succeed(undefined) },
  AddressHistoryDB: {
    aggregateProcessedTxs: () =>
      Effect.succeed({
        addressHistoryEntries: [],
        collectiveProduced: [],
        collectiveSpent: [],
      }),
    upsertEntries: () => Effect.succeed(undefined),
    depositEntryToEntry: () => Effect.succeed({}),
    resolvedWithdrawalToEntry: () => ({}),
    Status: { SUBMITTED: "SUBMITTED" },
  },
  DepositsDB: { entryToLedgerEntry: () => Effect.succeed({}) },
  WithdrawalsDB: { resolveEntry: () => Effect.succeed({}) },
  Ledger: { Columns: { OUTREF: "outref" } },
  UserEvents: { Columns: { INFO: "info" } },
  Tx: { Columns: { TX: "tx", TX_ID: "tx_id" } },
}));

vi.mock("@al-ft/midgard-sdk", () => ({
  bufferToHex: (b: Buffer) => b.toString("hex"),
  LucidError: class LucidError extends Error {
    _tag = "LucidError";
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
    }
  },
  CmlDeserializationError: class CmlDeserializationError extends Error {
    _tag = "CmlDeserializationError";
  },
}));

vi.mock("@/utils.js", () => ({
  batchProgram: (_size: number, total: number, _label: string, _fn: unknown) =>
    total === 0 ? Effect.succeed(undefined) : Effect.succeed(undefined),
  breakDownTx: () =>
    Effect.succeed({
      spent: [],
      produced: [],
      txId: Buffer.alloc(32),
      txCbor: Buffer.alloc(0),
    }),
}));

// Mirror metric to read its global state.
const submitCounter = Metric.counter("submit_block_count", {
  description:
    "A counter for blocks successfully submitted to L1 and marked SUBMITTED in BlocksDB",
  bigint: true,
  incremental: true,
}).register();

// Import after mocks are set up.
import { submitEarliestBlock } from "@/fibers/block-submission.js";

const sqlHarness = createMockSqlHarness();

const fakeSubmitProgram = vi.fn(() => Effect.succeed("faketxhash"));
const fakeCompleteProgram = vi.fn<
  () => Effect.Effect<
    { submitProgram: typeof fakeSubmitProgram },
    { _tag: "RunTimeError" },
    never
  >
>(() => Effect.succeed({ submitProgram: fakeSubmitProgram }));
const fakeFromTx = vi.fn(() => ({ completeProgram: fakeCompleteProgram }));

const fakeLucidLayer = Layer.succeed(
  Lucid,
  Lucid.of({
    _tag: "Lucid",
    api: { fromTx: fakeFromTx } as never,
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsBlockCommitmentWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
  }),
);

const baseLayer = Layer.mergeAll(
  sqlHarness.layer,
  fakeLucidLayer,
  Layer.succeed(AlwaysSucceedsContract, null as any),
  makeTestNodeConfigLayer(),
);

function runAction(layer = baseLayer) {
  return submitEarliestBlock.pipe(Effect.provide(layer));
}

function runActionExpectLucidFailure(layer = baseLayer) {
  return submitEarliestBlock.pipe(
    Effect.catchTag("LucidError", () => Effect.void),
    Effect.provide(layer),
  );
}

const fakeBlockEntry = {
  l1_cbor: Buffer.from("deadbeef", "hex"),
  event_start_time: new Date(0),
  event_end_time: new Date(1000),
  header_hash: Buffer.alloc(32),
};

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  retrieveFn.mockReturnValue(Effect.succeed(Option.none()));
  setStatusFn.mockReturnValue(Effect.succeed(undefined));
  fakeSubmitProgram.mockReturnValue(Effect.succeed("faketxhash"));
  fakeCompleteProgram.mockReturnValue(
    Effect.succeed({ submitProgram: fakeSubmitProgram }),
  );
});

describe("submitEarliestBlock — submit_block_count counter", () => {
  it.effect("does NOT increment when no unsubmitted block is found", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.none()));

      const before = yield* Metric.value(submitCounter);
      yield* runAction();
      const after = yield* Metric.value(submitCounter);

      expect(after.count - before.count).toBe(0n);
      expect(fakeFromTx).not.toHaveBeenCalled();
      expect(setStatusFn).not.toHaveBeenCalled();
    }),
  );

  it.effect("increments by 1 after a block is successfully submitted", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));

      const before = yield* Metric.value(submitCounter);
      yield* runAction();
      const after = yield* Metric.value(submitCounter);

      expect(after.count - before.count).toBe(1n);
      expect(fakeFromTx).toHaveBeenCalledWith("deadbeef");
      expect(fakeCompleteProgram).toHaveBeenCalledTimes(1);
      expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
      expect(setStatusFn).toHaveBeenCalledTimes(1);
      expect(setStatusFn).toHaveBeenCalledWith(fakeBlockEntry, 1);
    }),
  );

  it.effect("does NOT increment when L1 submission fails", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
      fakeCompleteProgram.mockReturnValue(
        Effect.fail({ _tag: "RunTimeError" }),
      );

      const before = yield* Metric.value(submitCounter);
      yield* runActionExpectLucidFailure();
      const after = yield* Metric.value(submitCounter);

      expect(after.count - before.count).toBe(0n);
      expect(fakeSubmitProgram).not.toHaveBeenCalled();
      expect(setStatusFn).not.toHaveBeenCalled();
    }),
  );
});
