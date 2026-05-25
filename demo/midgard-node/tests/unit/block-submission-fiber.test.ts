import { describe, expect, vi, beforeEach } from "vitest";
import { it } from "@effect/vitest";
import { Effect, Layer, Metric, Option } from "effect";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { Lucid } from "@/services/lucid.js";
import { makeTestNodeConfigLayer } from "./harness/node-config-layer.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";
import { metricDelta } from "./harness/metric-snapshot.js";

// Hoisted so values can be swapped per test.
const retrieveFn = vi.hoisted(() => vi.fn());
const setStatusFn = vi.hoisted(() => vi.fn());
const countByStatusFn = vi.hoisted(() => vi.fn());
const SUBMITTED_STATUS_SENTINEL = vi.hoisted(() => 91_337);
const UNSUBMITTED_STATUS_SENTINEL = vi.hoisted(() => 0);
const fromCborBytesFn = vi.hoisted(() => vi.fn());

vi.mock("@/database/index.js", () => ({
  BlocksDB: {
    get retrieveEarliestUnsubmittedEntry() {
      return retrieveFn();
    },
    setStatusOfEntry: (...args: unknown[]) => setStatusFn(...args),
    countByStatus: (...args: unknown[]) => countByStatusFn(...args),
    countWithMinimumStatus: () => Effect.succeed(0n),
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
    Status: {
      UNSUBMITTED: UNSUBMITTED_STATUS_SENTINEL,
      SUBMITTED: SUBMITTED_STATUS_SENTINEL,
    },
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

vi.mock("@lucid-evolution/lucid", () => ({
  CML: {
    Transaction: {
      from_cbor_bytes: (...args: unknown[]) => fromCborBytesFn(...args),
    },
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

// Import after mocks are set up.
import {
  blockSubmissionMetrics,
  submitEarliestBlock,
} from "@/fibers/block-submission.js";

const sqlHarness = createMockSqlHarness();

const readSubmitCounter = Metric.value(
  blockSubmissionMetrics.submitBlockCounter,
);
const readL1CommitmentFeesCounter = Metric.value(
  blockSubmissionMetrics.l1CommitmentFeesLovelaceCounter,
);
const readL1CommitmentFeeLastGauge = Metric.value(
  blockSubmissionMetrics.l1CommitmentFeeLovelaceLastGauge,
);

const fakeSubmitProgram = vi.fn(() => Effect.succeed("faketxhash"));
const fakeCompleteProgram = vi.fn<
  () => Effect.Effect<
    { submitProgram: typeof fakeSubmitProgram },
    { _tag: "RunTimeError" },
    never
  >
>(() => Effect.succeed({ submitProgram: fakeSubmitProgram }));
const fakeFromTx = vi.fn(() => ({
  sign: {
    withWallet: () => ({
      completeProgram: fakeCompleteProgram,
    }),
  },
}));

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
  countByStatusFn.mockReturnValue(Effect.succeed(0n));
  fakeSubmitProgram.mockReturnValue(Effect.succeed("faketxhash"));
  fakeCompleteProgram.mockReturnValue(
    Effect.succeed({ submitProgram: fakeSubmitProgram }),
  );
  fromCborBytesFn.mockReturnValue({
    body: () => ({
      fee: () => 42n,
    }),
  });
});

describe("submitEarliestBlock — submit_block_count counter", () => {
  it.effect("does NOT increment when no unsubmitted block is found", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.none()));

      const delta = yield* metricDelta(
        readSubmitCounter,
        runAction(),
        (state) => state.count,
      );

      expect(delta).toBe(0n);
      expect(fakeFromTx).not.toHaveBeenCalled();
      expect(setStatusFn).not.toHaveBeenCalled();
    }),
  );

  it.effect("increments by 1 after a block is successfully submitted", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));

      const delta = yield* metricDelta(
        readSubmitCounter,
        runAction(),
        (state) => state.count,
      );

      expect(delta).toBe(1n);
      expect(fakeFromTx).toHaveBeenCalledWith("deadbeef");
      expect(fakeCompleteProgram).toHaveBeenCalledTimes(1);
      expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
      expect(setStatusFn).toHaveBeenCalledTimes(1);
      expect(setStatusFn).toHaveBeenCalledWith(
        fakeBlockEntry,
        SUBMITTED_STATUS_SENTINEL,
      );
    }),
  );

  it.effect(
    "updates L1 commitment fee metrics after successful submission",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));

        const feeDelta = (yield* metricDelta(
          readL1CommitmentFeesCounter,
          runAction(),
          (state) => state.count,
        )) as bigint;

        expect(feeDelta).toBe(42n);

        const lastFeeState = yield* readL1CommitmentFeeLastGauge.pipe(
          Effect.provide(baseLayer),
        );
        expect(lastFeeState.value).toBe(42n);
      }),
  );

  it.effect(
    "does not fail block submission when fee extraction fails; fee metrics stay unchanged",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        fromCborBytesFn.mockImplementation(() => {
          throw new Error("bad cbor");
        });

        const feeDelta = (yield* metricDelta(
          readL1CommitmentFeesCounter,
          runAction(),
          (state) => state.count,
        )) as bigint;

        expect(feeDelta).toBe(0n);
        expect(setStatusFn).toHaveBeenCalledTimes(1);
      }),
  );

  it.effect("does NOT increment when L1 submission fails", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
      fakeCompleteProgram.mockReturnValue(
        Effect.fail({ _tag: "RunTimeError" }),
      );

      const delta = yield* metricDelta(
        readSubmitCounter,
        runActionExpectLucidFailure(),
        (state) => state.count,
      );

      expect(delta).toBe(0n);
      expect(fakeSubmitProgram).not.toHaveBeenCalled();
      expect(setStatusFn).not.toHaveBeenCalled();
    }),
  );
});
