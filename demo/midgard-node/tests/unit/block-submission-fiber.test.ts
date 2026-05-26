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
const setL1CborFn = vi.hoisted(() => vi.fn());
const countByStatusFn = vi.hoisted(() => vi.fn());
const countPendingBlocksFn = vi.hoisted(() => vi.fn());
const SUBMITTED_STATUS_SENTINEL = vi.hoisted(() => 91_337);
const SUBMITTING_STATUS_SENTINEL = vi.hoisted(() => 1);
const UNSUBMITTED_STATUS_SENTINEL = vi.hoisted(() => 0);
const fromCborBytesFn = vi.hoisted(() => vi.fn());
const deserializeUTxOsFn = vi.hoisted(() => vi.fn());
const reinitializeMergeApiFn = vi.hoisted(() => vi.fn());

vi.mock("@/database/utils/common.js", () => ({
  deserializeUTxOsFromStorage: (...args: unknown[]) =>
    deserializeUTxOsFn(...args),
}));

vi.mock("@/database/index.js", () => ({
  BlocksDB: {
    get retrieveEarliestPendingEntry() {
      return retrieveFn();
    },
    setStatusOfEntry: (...args: unknown[]) => setStatusFn(...args),
    setL1CborOfEntry: (...args: unknown[]) => setL1CborFn(...args),
    countByStatus: (...args: unknown[]) => countByStatusFn(...args),
    countWithMinimumStatus: () => Effect.succeed(0n),
    get countPendingBlocks() {
      return countPendingBlocksFn();
    },
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
      PRODUCED_UTXOS: "produced_utxos",
    },
    Status: {
      UNSUBMITTED: UNSUBMITTED_STATUS_SENTINEL,
      SUBMITTING: SUBMITTING_STATUS_SENTINEL,
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
  isIdempotentSubmitErrorCandidate,
  submitEarliestBlock,
} from "@/fibers/block-submission.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";

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
const readSignTimeoutCounter = Metric.value(
  blockSubmissionMetrics.submitBlockSignTimeoutsCounter,
);
const readSignRecoveredCounter = Metric.value(
  blockSubmissionMetrics.submitBlockSignRecoveredCounter,
);
const readSignReinitCounter = Metric.value(
  blockSubmissionMetrics.submitBlockSignReinitCounter,
);

const SIGNED_TX_CBOR_HEX = "cafebabe";
const fakeSubmitProgram = vi.fn<() => Effect.Effect<string, unknown, never>>(
  () => Effect.succeed("faketxhash"),
);
const makeSignedTxArtifact = (cborHex = SIGNED_TX_CBOR_HEX) => ({
  submitProgram: fakeSubmitProgram,
  toCBOR: () => cborHex,
});
const fakeCompleteSignedFromCborProgram = vi.hoisted(() => vi.fn());
const fakeSignCompleteProgram = vi.hoisted(() => vi.fn());
const fakeFromTx = vi.fn((txHex: string) => ({
  completeProgram: () => fakeCompleteSignedFromCborProgram(txHex),
  sign: {
    withWallet: () => ({
      completeProgram: () => fakeSignCompleteProgram(txHex),
    }),
  },
}));
const fakeMainFromTx = vi.fn(() => {
  throw new Error(
    "mainApi.fromTx should not be used for block submission signing",
  );
});
const fakeUtxosByOutRef = vi.fn(async () => [] as unknown[]);
const fakeMainApi = {
  utxosByOutRef: fakeUtxosByOutRef,
  fromTx: fakeMainFromTx,
} as never;
const fakeMergeApi = { fromTx: fakeFromTx } as never;

const fakeLucidLayer = Layer.succeed(
  Lucid,
  Lucid.of({
    _tag: "Lucid",
    api: fakeMainApi,
    mainApi: fakeMainApi,
    blockCommitmentApi: fakeMainApi,
    mergeApi: fakeMergeApi,
    reinitializeMergeApi: Effect.suspend(() => reinitializeMergeApiFn()),
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

function runActionExpectSubmitFailure(layer = baseLayer) {
  return submitEarliestBlock.pipe(
    Effect.catchTag("TxSubmitError", () => Effect.void),
    Effect.provide(layer),
  );
}

function runActionExpectSignFailure(layer = baseLayer) {
  return submitEarliestBlock.pipe(
    Effect.catchTag("TxSignError", () => Effect.void),
    Effect.provide(layer),
  );
}

const fakeBlockEntry = {
  l1_cbor: Buffer.from("deadbeef", "hex"),
  event_start_time: new Date(0),
  event_end_time: new Date(1000),
  header_hash: Buffer.alloc(32),
  produced_utxos: Buffer.from("[]", "utf8"),
};

beforeEach(() => {
  vi.clearAllMocks();
  sqlHarness.reset();
  retrieveFn.mockReturnValue(Effect.succeed(Option.none()));
  setStatusFn.mockReturnValue(Effect.succeed(undefined));
  setL1CborFn.mockReturnValue(Effect.succeed(undefined));
  countByStatusFn.mockReturnValue(Effect.succeed(0n));
  countPendingBlocksFn.mockReturnValue(Effect.succeed(0n));
  fakeSubmitProgram.mockReturnValue(Effect.succeed("faketxhash"));
  reinitializeMergeApiFn.mockReturnValue(Effect.succeed(undefined));
  fakeCompleteSignedFromCborProgram.mockImplementation((txHex: string) =>
    txHex === SIGNED_TX_CBOR_HEX
      ? Effect.succeed(makeSignedTxArtifact(SIGNED_TX_CBOR_HEX))
      : Effect.fail({ _tag: "TxSignerError" }),
  );
  fakeSignCompleteProgram.mockReturnValue(
    Effect.succeed(makeSignedTxArtifact(SIGNED_TX_CBOR_HEX)),
  );
  fromCborBytesFn.mockReturnValue({
    body: () => ({
      fee: () => 42n,
    }),
  });
  // Default: produced UTxOs empty → pre-check short-circuits to false.
  deserializeUTxOsFn.mockReturnValue(Effect.succeed([]));
  // Default: provider returns no UTxOs → pre-check returns false.
  fakeUtxosByOutRef.mockResolvedValue([]);
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
      // First call: SUBMITTING; second call (inside SQL tx): SUBMITTED.
      expect(setStatusFn).toHaveBeenCalledTimes(2);
      expect(setStatusFn).toHaveBeenNthCalledWith(
        1,
        fakeBlockEntry,
        SUBMITTING_STATUS_SENTINEL,
      );
      expect(setStatusFn).toHaveBeenNthCalledWith(
        2,
        fakeBlockEntry,
        SUBMITTED_STATUS_SENTINEL,
      );
      expect(fakeFromTx).toHaveBeenCalledWith("deadbeef");
      expect(fakeMainFromTx).not.toHaveBeenCalled();
      expect(fakeSignCompleteProgram).toHaveBeenCalledTimes(1);
      expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
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
        // SUBMITTING + SUBMITTED both called despite fee failure.
        expect(setStatusFn).toHaveBeenCalledTimes(2);
      }),
  );

  it.effect("does NOT increment when L1 submission fails", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
      fakeSignCompleteProgram.mockReturnValue(
        Effect.fail({ _tag: "RunTimeError" }),
      );

      const delta = yield* metricDelta(
        readSubmitCounter,
        runActionExpectLucidFailure(),
        (state) => state.count,
      );

      expect(delta).toBe(0n);
      expect(fakeSubmitProgram).not.toHaveBeenCalled();
      // SUBMITTING is written before the L1 call even when submission fails.
      expect(setStatusFn).toHaveBeenCalledTimes(1);
      expect(setStatusFn).toHaveBeenCalledWith(
        fakeBlockEntry,
        SUBMITTING_STATUS_SENTINEL,
      );
    }),
  );

  it.effect(
    "persists signed artifact once and retries submit without re-signing",
    () =>
      Effect.gen(function* () {
        const signedBlockEntry = {
          ...fakeBlockEntry,
          l1_cbor: Buffer.from(SIGNED_TX_CBOR_HEX, "hex"),
        };
        retrieveFn
          .mockReturnValueOnce(Effect.succeed(Option.some(fakeBlockEntry)))
          .mockReturnValueOnce(Effect.succeed(Option.some(signedBlockEntry)));
        fakeSubmitProgram
          .mockReturnValueOnce(
            Effect.fail(
              new TxSubmitError({
                message: "temporary submit failure",
                cause: "node busy",
                txHash: "<unknown>",
              }),
            ),
          )
          .mockReturnValueOnce(Effect.succeed("faketxhash-retry"));

        yield* runActionExpectSubmitFailure();
        const delta = yield* metricDelta(
          readSubmitCounter,
          runAction(),
          (state) => state.count,
        );

        expect(delta).toBe(1n);
        expect(fakeSignCompleteProgram).toHaveBeenCalledTimes(1);
        expect(setL1CborFn).toHaveBeenCalledTimes(1);
        expect(setL1CborFn).toHaveBeenCalledWith(
          fakeBlockEntry,
          Buffer.from(SIGNED_TX_CBOR_HEX, "hex"),
        );
        expect(fakeFromTx).toHaveBeenCalledWith("deadbeef");
        expect(fakeFromTx).toHaveBeenCalledWith(SIGNED_TX_CBOR_HEX);
      }),
  );

  it.effect(
    "recovers from sign timeout by reinitializing signer context and retrying",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        fakeSignCompleteProgram
          .mockReturnValueOnce(
            Effect.fail(
              new TxSignError({
                message:
                  "Timed out while signing L1 commitment tx (header_hash=test)",
                cause: "Timed out waiting for sign flow",
                txHash: "<unknown>",
              }),
            ),
          )
          .mockReturnValueOnce(
            Effect.succeed(makeSignedTxArtifact(SIGNED_TX_CBOR_HEX)),
          );

        const beforeTimeout = (yield* readSignTimeoutCounter.pipe(
          Effect.provide(baseLayer),
        )).count;
        const beforeRecovered = (yield* readSignRecoveredCounter.pipe(
          Effect.provide(baseLayer),
        )).count;
        const beforeReinit = (yield* readSignReinitCounter.pipe(
          Effect.provide(baseLayer),
        )).count;

        yield* runAction();

        const afterTimeout = (yield* readSignTimeoutCounter.pipe(
          Effect.provide(baseLayer),
        )).count;
        const afterRecovered = (yield* readSignRecoveredCounter.pipe(
          Effect.provide(baseLayer),
        )).count;
        const afterReinit = (yield* readSignReinitCounter.pipe(
          Effect.provide(baseLayer),
        )).count;

        expect(afterTimeout - beforeTimeout).toBe(1n);
        expect(afterRecovered - beforeRecovered).toBe(1n);
        expect(afterReinit - beforeReinit).toBe(1n);
        expect(reinitializeMergeApiFn).toHaveBeenCalledTimes(1);
      }),
  );

  it.effect(
    "stops after bounded sign-timeout recovery retries are exhausted",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        fakeSignCompleteProgram
          .mockReturnValueOnce(
            Effect.fail(
              new TxSignError({
                message:
                  "Timed out while signing L1 commitment tx (header_hash=test)",
                cause: "Timed out waiting for sign flow",
                txHash: "<unknown>",
              }),
            ),
          )
          .mockReturnValueOnce(
            Effect.fail(
              new TxSignError({
                message:
                  "Timed out while signing L1 commitment tx (header_hash=test)",
                cause: "Timed out waiting for sign flow",
                txHash: "<unknown>",
              }),
            ),
          );

        yield* runActionExpectSignFailure();

        expect(reinitializeMergeApiFn).toHaveBeenCalledTimes(1);
        expect(fakeSubmitProgram).not.toHaveBeenCalled();
      }),
  );
});

describe("submitEarliestBlock — L1 pre-check (M-52)", () => {
  it.effect("skips sign+submit and marks SUBMITTED when tx already on L1", () =>
    Effect.gen(function* () {
      retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
      // Pre-check finds UTxOs → tx is already on L1.
      deserializeUTxOsFn.mockReturnValue(
        Effect.succeed([{ txHash: "aabbcc", outputIndex: 0 }]),
      );
      fakeUtxosByOutRef.mockResolvedValue([
        { txHash: "aabbcc", outputIndex: 0 },
      ]);

      const delta = yield* metricDelta(
        readSubmitCounter,
        runAction(),
        (state) => state.count,
      );

      expect(delta).toBe(1n);
      // sign+submit path skipped entirely.
      expect(fakeFromTx).not.toHaveBeenCalled();
      expect(fakeCompleteSignedFromCborProgram).not.toHaveBeenCalled();
      expect(fakeSignCompleteProgram).not.toHaveBeenCalled();
      expect(fakeSubmitProgram).not.toHaveBeenCalled();
      // SUBMITTING then SUBMITTED still called.
      expect(setStatusFn).toHaveBeenCalledTimes(2);
      expect(setStatusFn).toHaveBeenNthCalledWith(
        1,
        fakeBlockEntry,
        SUBMITTING_STATUS_SENTINEL,
      );
      expect(setStatusFn).toHaveBeenNthCalledWith(
        2,
        fakeBlockEntry,
        SUBMITTED_STATUS_SENTINEL,
      );
    }),
  );

  it.effect(
    "falls back to normal submission when pre-check provider call fails",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        // Pre-check has UTxOs to check but provider throws.
        deserializeUTxOsFn.mockReturnValue(
          Effect.succeed([{ txHash: "aabbcc", outputIndex: 0 }]),
        );
        fakeUtxosByOutRef.mockRejectedValue(new Error("provider unavailable"));

        const delta = yield* metricDelta(
          readSubmitCounter,
          runAction(),
          (state) => state.count,
        );

        // Submission proceeded normally after pre-check failure.
        expect(delta).toBe(1n);
        expect(fakeFromTx).toHaveBeenCalledWith("deadbeef");
        expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
      }),
  );
});

describe("submitEarliestBlock — idempotent submit recovery", () => {
  it.effect(
    "treats 'already included / inputs spent' submit errors as idempotent-success when re-check confirms L1 inclusion",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        deserializeUTxOsFn.mockReturnValue(
          Effect.succeed([{ txHash: "aabbcc", outputIndex: 0 }]),
        );
        // First pre-check: not found on L1, then retry pre-check after submit
        // error: found on L1.
        fakeUtxosByOutRef
          .mockResolvedValueOnce([])
          .mockResolvedValueOnce([{ txHash: "aabbcc", outputIndex: 0 }]);
        fakeSubmitProgram.mockReturnValue(
          Effect.fail(
            new TxSubmitError({
              message:
                'Failed to submit previously built and signed tx: ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included"',
              cause: "ConwayMempoolFailure All inputs are spent",
              txHash: "<unknown>",
            }),
          ) as any,
        );

        const delta = yield* metricDelta(
          readSubmitCounter,
          runAction(),
          (state) => state.count,
        );

        expect(delta).toBe(1n);
        expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
        expect(setStatusFn).toHaveBeenCalledTimes(2);
        expect(setStatusFn).toHaveBeenNthCalledWith(
          1,
          fakeBlockEntry,
          SUBMITTING_STATUS_SENTINEL,
        );
        expect(setStatusFn).toHaveBeenNthCalledWith(
          2,
          fakeBlockEntry,
          SUBMITTED_STATUS_SENTINEL,
        );
      }),
  );

  it.effect(
    "fails when idempotent-success candidate submit error cannot be confirmed on L1",
    () =>
      Effect.gen(function* () {
        retrieveFn.mockReturnValue(Effect.succeed(Option.some(fakeBlockEntry)));
        deserializeUTxOsFn.mockReturnValue(
          Effect.succeed([{ txHash: "aabbcc", outputIndex: 0 }]),
        );
        // Both checks say not found on L1.
        fakeUtxosByOutRef.mockResolvedValue([]);
        fakeSubmitProgram.mockReturnValue(
          Effect.fail(
            new TxSubmitError({
              message:
                'Failed to submit previously built and signed tx: ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included"',
              cause: "ConwayMempoolFailure All inputs are spent",
              txHash: "<unknown>",
            }),
          ) as any,
        );

        const delta = yield* metricDelta(
          readSubmitCounter,
          runActionExpectSubmitFailure(),
          (state) => state.count,
        );

        expect(delta).toBe(0n);
        expect(fakeSubmitProgram).toHaveBeenCalledTimes(1);
        expect(setStatusFn).toHaveBeenCalledTimes(1);
        expect(setStatusFn).toHaveBeenNthCalledWith(
          1,
          fakeBlockEntry,
          SUBMITTING_STATUS_SENTINEL,
        );
      }),
  );
});

describe("isIdempotentSubmitErrorCandidate", () => {
  it("matches known already-included markers", () => {
    const error = new TxSubmitError({
      message:
        'TxSubmitFail: ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included"',
      cause: "BadInputsUTxO",
      txHash: "<unknown>",
    });
    expect(isIdempotentSubmitErrorCandidate(error)).toBe(true);
  });

  it("does not match unrelated submit failures", () => {
    const error = new TxSubmitError({
      message: "TxSubmitFail: fee too small",
      cause: "InsufficientFee",
      txHash: "<unknown>",
    });
    expect(isIdempotentSubmitErrorCandidate(error)).toBe(false);
  });
});
