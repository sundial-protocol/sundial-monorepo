import * as SDK from "@al-ft/midgard-sdk";
import { CML, type TxSigned } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import {
  Cause,
  Effect,
  Metric,
  MetricBoundaries,
  Option,
  Schedule,
} from "effect";
import { deserializeUTxOsFromStorage } from "@/database/utils/common.js";
import { performance } from "node:perf_hooks";
import {
  DepositsDB,
  LatestLedgerDB,
  MempoolDB,
  BlocksDB,
  Ledger,
  Tx,
  UserEvents,
  ImmutableDB,
  BlocksTxsDB,
  WithdrawalsDB,
  AddressHistoryDB,
} from "@/database/index.js";
import { batchProgram, breakDownTx, ProcessedTx } from "@/utils.js";

const submitBlockCounter = Metric.counter("submit_block_count", {
  description:
    "A counter for blocks successfully submitted to L1 and marked SUBMITTED in BlocksDB",
  bigint: true,
  incremental: true,
}).register();

const submitBlockFailuresCounter = Metric.counter("submit_block_failures", {
  description:
    "A counter for block submission failures before a block is marked SUBMITTED",
  bigint: true,
  incremental: true,
}).register();

const submitBlockSignDurationHistogram = Metric.histogram(
  "submit_block_sign_duration_seconds",
  MetricBoundaries.exponential({ start: 0.05, factor: 2, count: 14 }),
  "Histogram of L1 commitment sign stage duration in seconds",
).register();

const submitBlockSubmitDurationHistogram = Metric.histogram(
  "submit_block_submit_duration_seconds",
  MetricBoundaries.exponential({ start: 0.05, factor: 2, count: 14 }),
  "Histogram of L1 commitment submit stage duration in seconds",
).register();

const submitBlockSignTimeoutsCounter = Metric.counter(
  "submit_block_sign_timeouts",
  {
    description:
      "A counter for sign-stage timeouts while preparing an L1 commitment transaction",
    bigint: true,
    incremental: true,
  },
).register();

const submitBlockSignRecoveredCounter = Metric.counter(
  "submit_block_sign_recovered",
  {
    description:
      "A counter for sign-stage timeout recoveries that succeeded after signer context reinitialization",
    bigint: true,
    incremental: true,
  },
).register();

const submitBlockSignReinitCounter = Metric.counter(
  "submit_block_sign_reinit",
  {
    description:
      "A counter for signer-context reinitialization attempts after sign-stage timeouts",
    bigint: true,
    incremental: true,
  },
).register();

const submitBlockSubmitTimeoutsCounter = Metric.counter(
  "submit_block_submit_timeouts",
  {
    description:
      "A counter for submit-stage timeouts while submitting an L1 commitment transaction",
    bigint: true,
    incremental: true,
  },
).register();

const l1CommitmentFeesLovelaceCounter = Metric.counter(
  "l1_commitment_fees_lovelace",
  {
    description:
      "A counter for total lovelace fees spent for submitted L1 commitment transactions",
    bigint: true,
    incremental: true,
  },
).register();

const l1CommitmentFeeLovelaceLastGauge = Metric.gauge(
  "l1_commitment_fee_lovelace_last",
  {
    description:
      "A gauge for the lovelace fee of the most recently submitted L1 commitment transaction",
    bigint: true,
  },
).register();

const unsubmittedBlockBacklogGauge = Metric.gauge("unsubmitted_block_backlog", {
  description:
    "Current number of rows in unsubmitted_blocks with status=UNSUBMITTED",
  bigint: true,
}).register();

export const blockSubmissionMetrics = {
  submitBlockCounter,
  submitBlockFailuresCounter,
  submitBlockSignDurationHistogram,
  submitBlockSubmitDurationHistogram,
  submitBlockSignTimeoutsCounter,
  submitBlockSignRecoveredCounter,
  submitBlockSignReinitCounter,
  submitBlockSubmitTimeoutsCounter,
  l1CommitmentFeesLovelaceCounter,
  l1CommitmentFeeLovelaceLastGauge,
  unsubmittedBlockBacklogGauge,
} as const;

export const initializeSubmissionMetrics = Effect.all([
  Metric.incrementBy(blockSubmissionMetrics.submitBlockCounter, 0n),
  Metric.incrementBy(blockSubmissionMetrics.submitBlockFailuresCounter, 0n),
  Metric.incrementBy(blockSubmissionMetrics.submitBlockSignTimeoutsCounter, 0n),
  Metric.incrementBy(
    blockSubmissionMetrics.submitBlockSignRecoveredCounter,
    0n,
  ),
  Metric.incrementBy(blockSubmissionMetrics.submitBlockSignReinitCounter, 0n),
  Metric.incrementBy(
    blockSubmissionMetrics.submitBlockSubmitTimeoutsCounter,
    0n,
  ),
  Metric.update(blockSubmissionMetrics.submitBlockSignDurationHistogram, 0),
  Metric.update(blockSubmissionMetrics.submitBlockSubmitDurationHistogram, 0),
  Metric.incrementBy(
    blockSubmissionMetrics.l1CommitmentFeesLovelaceCounter,
    0n,
  ),
  Metric.set(blockSubmissionMetrics.l1CommitmentFeeLovelaceLastGauge, 0n),
  Metric.set(blockSubmissionMetrics.unsubmittedBlockBacklogGauge, 0n),
]);

const loadSubmissionMetricsBaselineFromDb = Effect.gen(function* () {
  const [submittedOrLaterCount, pendingCount] = yield* Effect.all(
    [
      BlocksDB.countWithMinimumStatus(BlocksDB.Status.SUBMITTED),
      BlocksDB.countPendingBlocks,
    ],
    { concurrency: "unbounded" },
  );
  return {
    submittedOrLaterCount,
    pendingCount,
  };
});

const reconcileSubmissionMetricsFromDb = Effect.gen(function* () {
  // On node boot, restore counters/gauges from persisted block statuses.
  yield* initializeSubmissionMetrics;
  const { submittedOrLaterCount, pendingCount } =
    yield* loadSubmissionMetricsBaselineFromDb;
  yield* Metric.incrementBy(
    blockSubmissionMetrics.submitBlockCounter,
    submittedOrLaterCount,
  );
  yield* Metric.set(
    blockSubmissionMetrics.unsubmittedBlockBacklogGauge,
    pendingCount,
  );
});

const refreshUnsubmittedBacklogGaugeFromDb = Effect.gen(function* () {
  const pendingCount = yield* BlocksDB.countPendingBlocks;
  yield* Metric.set(
    blockSubmissionMetrics.unsubmittedBlockBacklogGauge,
    pendingCount,
  );
});

// Submission DB batching uses adaptive sizing so small blocks avoid oversized
// transactions while large replay windows reduce round-trips.
const MIN_SUBMISSION_BATCH_SIZE = 250;
const DEFAULT_SUBMISSION_BATCH_SIZE = 1000;
const MAX_SUBMISSION_BATCH_SIZE = 5000;
const SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS = 30_000;
const SIGN_STAGE_TIMEOUT_CAUSE = "Timed out waiting for sign flow";
const SUBMIT_STAGE_TIMEOUT_CAUSE = "Timed out waiting for submit flow";
const IDEMPOTENT_SUBMIT_ERROR_MARKERS = [
  "all inputs are spent. transaction has probably already been included",
  "all inputs are spent",
  "badinputsutxo",
] as const;

const stringifyUnknownErrorCause = (value: unknown): string => {
  if (typeof value === "string") {
    return value;
  }
  if (value instanceof Error) {
    return `${value.message} ${stringifyUnknownErrorCause((value as Error & { cause?: unknown }).cause)}`;
  }
  if (value === null || value === undefined) {
    return "";
  }
  try {
    return JSON.stringify(value);
  } catch {
    return String(value);
  }
};

export const isIdempotentSubmitErrorCandidate = (
  error: TxSubmitError,
): boolean => {
  const haystack =
    `${error.message} ${stringifyUnknownErrorCause(error.cause)}`.toLowerCase();
  return IDEMPOTENT_SUBMIT_ERROR_MARKERS.some((marker) =>
    haystack.includes(marker),
  );
};

export const resolveSubmissionBatchSize = (itemCount: number): number => {
  if (itemCount >= 20_000) return MAX_SUBMISSION_BATCH_SIZE;
  if (itemCount >= 5_000) return 2500;
  if (itemCount >= 1_000) return DEFAULT_SUBMISSION_BATCH_SIZE;
  return MIN_SUBMISSION_BATCH_SIZE;
};

const isWrappedTxSignerError = (error: TxSignError): boolean =>
  typeof error.cause === "object" &&
  error.cause !== null &&
  "_tag" in error.cause &&
  (error.cause as { _tag?: string })._tag === "TxSignerError";

const submitSignedTxProgram = (
  signedTx: TxSigned,
  headerHashHex: string,
  timeoutMs: number,
): Effect.Effect<string, TxSubmitError, never> =>
  Effect.gen(function* () {
    const submitStartedAtMs = performance.now();
    return yield* signedTx.submitProgram().pipe(
      Effect.timeoutFail({
        duration: `${timeoutMs} millis`,
        onTimeout: () =>
          new TxSubmitError({
            message: `Timed out after ${timeoutMs}ms while submitting L1 commitment tx (header_hash=${headerHashHex})`,
            cause: SUBMIT_STAGE_TIMEOUT_CAUSE,
            txHash: "<unknown>",
          }),
      }),
      Effect.ensuring(
        Metric.update(
          blockSubmissionMetrics.submitBlockSubmitDurationHistogram,
          (performance.now() - submitStartedAtMs) / 1000,
        ),
      ),
      Effect.tapErrorTag("TxSubmitError", (error) =>
        error.cause === SUBMIT_STAGE_TIMEOUT_CAUSE
          ? Metric.increment(
              blockSubmissionMetrics.submitBlockSubmitTimeoutsCounter,
            )
          : Effect.void,
      ),
      Effect.mapError(
        (error) =>
          new TxSubmitError({
            message: `Failed to submit previously built and signed tx: ${error.message}`,
            cause: error,
            txHash: "<unknown>",
          }),
      ),
    );
  });

const completeSignedTxFromCborProgram = (
  l1CborBytes: Buffer,
): Effect.Effect<TxSigned, TxSignError | SDK.LucidError, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const mergeApi = lucid.mergeApi;
    const signedTxHex = SDK.bufferToHex(l1CborBytes);
    return yield* mergeApi.fromTx(signedTxHex).completeProgram();
  }).pipe(
    Effect.mapError((e) => {
      const commonMsg = "Failed to complete transaction from persisted CBOR";
      if (e._tag === "TxSignerError") {
        return new TxSignError({
          message: `${commonMsg} due to a bad signature`,
          cause: e,
          txHash: "<unknown>",
        });
      } else if (e._tag === "RunTimeError") {
        return new SDK.LucidError({
          message: `${commonMsg} due to an unknown error`,
          cause: e,
        });
      } else {
        return e;
      }
    }),
  );

const signTxFromCborSingleAttemptProgram = (
  l1CborBytes: Buffer,
  headerHashHex: string,
): Effect.Effect<TxSigned, TxSignError | SDK.LucidError, Lucid | NodeConfig> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const mergeApi = lucid.mergeApi;
    const signedTxHex = SDK.bufferToHex(l1CborBytes);
    const timeoutMs = Math.max(
      SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS,
      nodeConfig.SUBMIT_SIGNED_TX_TIMEOUT_MS,
    );
    const signStartedAtMs = performance.now();
    // Some commitment txs require an additional operator witness (e.g. merge
    // signer) beyond the witness already embedded by the commitment worker.
    // Sign exactly once, persist the signed artifact, then retry submit without
    // re-running the sign flow.
    return yield* mergeApi
      .fromTx(signedTxHex)
      .sign.withWallet()
      .completeProgram()
      .pipe(
        Effect.timeoutFail({
          duration: `${timeoutMs} millis`,
          onTimeout: () =>
            new TxSignError({
              message: `Timed out after ${timeoutMs}ms while signing L1 commitment tx (header_hash=${headerHashHex})`,
              cause: SIGN_STAGE_TIMEOUT_CAUSE,
              txHash: "<unknown>",
            }),
        }),
        Effect.ensuring(
          Metric.update(
            blockSubmissionMetrics.submitBlockSignDurationHistogram,
            (performance.now() - signStartedAtMs) / 1000,
          ),
        ),
        Effect.tapErrorTag("TxSignError", (error) =>
          error.cause === SIGN_STAGE_TIMEOUT_CAUSE
            ? Metric.increment(
                blockSubmissionMetrics.submitBlockSignTimeoutsCounter,
              )
            : Effect.void,
        ),
      );
  }).pipe(
    Effect.mapError((e) => {
      const commonMsg = "Failed to sign commitment tx from CBOR";
      if (e._tag === "TxSignerError") {
        return new TxSignError({
          message: `${commonMsg} due to a bad signature`,
          cause: e,
          txHash: "<unknown>",
        });
      } else if (e._tag === "RunTimeError") {
        return new SDK.LucidError({
          message: `${commonMsg} due to an unknown error`,
          cause: e,
        });
      } else {
        return e;
      }
    }),
  );

const reinitializeMergeSignerContextProgram: Effect.Effect<
  void,
  SDK.LucidError,
  Lucid
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  yield* lucid.reinitializeMergeApi.pipe(
    Effect.mapError(
      (error) =>
        new SDK.LucidError({
          message:
            "Failed to reinitialize merge signer context after sign timeout",
          cause: error,
        }),
    ),
  );
});

const signTxFromCborProgram = (
  l1CborBytes: Buffer,
  headerHashHex: string,
): Effect.Effect<TxSigned, TxSignError | SDK.LucidError, Lucid | NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const maxRecoveryRetries =
      nodeConfig.SUBMIT_SIGN_TIMEOUT_RECOVERY_MAX_RETRIES;

    const attempt = (
      recoveryRetriesRemaining: number,
    ): Effect.Effect<
      TxSigned,
      TxSignError | SDK.LucidError,
      Lucid | NodeConfig
    > =>
      signTxFromCborSingleAttemptProgram(l1CborBytes, headerHashHex).pipe(
        Effect.catchTag("TxSignError", (error) =>
          Effect.gen(function* () {
            if (
              error.cause !== SIGN_STAGE_TIMEOUT_CAUSE ||
              recoveryRetriesRemaining <= 0
            ) {
              return yield* Effect.fail(error);
            }

            yield* Effect.logWarning(
              `🔗 ⚠️  Sign stage timed out for header_hash=${headerHashHex}; reinitializing merge signer context and retrying (remaining_retries=${recoveryRetriesRemaining}).`,
            );
            yield* Metric.increment(
              blockSubmissionMetrics.submitBlockSignReinitCounter,
            );
            yield* reinitializeMergeSignerContextProgram;
            const signedTx = yield* attempt(recoveryRetriesRemaining - 1);
            yield* Metric.increment(
              blockSubmissionMetrics.submitBlockSignRecoveredCounter,
            );
            return signedTx;
          }),
        ),
      );

    return yield* attempt(maxRecoveryRetries);
  });

const signedTxToCborBytesProgram = (
  signedTx: TxSigned,
): Effect.Effect<Buffer, SDK.LucidError, never> =>
  Effect.try({
    try: () => Buffer.from(signedTx.toCBOR(), "hex"),
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to serialize signed commitment tx to CBOR bytes",
        cause,
      }),
  });

const submitTxCborWithoutSigningProgram = (
  l1CborBytes: Buffer,
  headerHashHex: string,
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | SDK.LucidError,
  Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const timeoutMs = Math.max(
      SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS,
      nodeConfig.SUBMIT_SIGNED_TX_TIMEOUT_MS,
    );
    const signedTx = yield* completeSignedTxFromCborProgram(l1CborBytes);
    return yield* submitSignedTxProgram(signedTx, headerHashHex, timeoutMs);
  });

const extractL1CommitmentFeeLovelace = (
  l1CborBytes: Buffer,
): Effect.Effect<bigint, SDK.CmlDeserializationError, never> =>
  Effect.try({
    try: () => CML.Transaction.from_cbor_bytes(l1CborBytes).body().fee(),
    catch: (cause) =>
      new SDK.CmlDeserializationError({
        message: "Failed to deserialize submitted L1 commitment CBOR",
        cause,
      }),
  });

/**
 * Going through withdrawal events and resolving their spent outrefs from
 * `LatestLedgerDB`, since it's the ledger that represents the state after
 * the latest submitted block.
 *
 * TODO: Allowing DataCoercionError to bubble up from here might be
 *       incorrect. WithdrawalsDB is most likely trust-worthy at this point.
 */
const processWithdrawalsProgram = (withdrawals: readonly UserEvents.Entry[]) =>
  Effect.gen(function* () {
    const withdrawnOutRefs: Buffer[] = [];
    const withdrawalAddressHistoryEntries: AddressHistoryDB.Entry[] = [];
    yield* Effect.forEach(withdrawals, (w) =>
      WithdrawalsDB.resolveEntry(LatestLedgerDB.tableName, w).pipe(
        Effect.andThen((resolvedWithdrawal) =>
          Effect.sync(() => {
            withdrawnOutRefs.push(
              resolvedWithdrawal.ledgerEntry[Ledger.Columns.OUTREF],
            );
            withdrawalAddressHistoryEntries.push(
              AddressHistoryDB.resolvedWithdrawalToEntry(
                resolvedWithdrawal,
                AddressHistoryDB.Status.SUBMITTED,
              ),
            );
          }),
        ),
      ),
    );
    return {
      withdrawnOutRefs,
      withdrawalAddressHistoryEntries,
    };
  });

const processTxOrdersProgram = (
  txOrders: readonly UserEvents.Entry[],
  concurrency: number,
) =>
  Effect.forEach(
    txOrders,
    (txOrder) => breakDownTx(txOrder[UserEvents.Columns.INFO]),
    { concurrency },
  );

const processTxRequestsProgram = (
  txRequests: readonly MempoolDB.EntryWithEffects[],
  concurrency: number,
) =>
  Effect.gen(function* () {
    const processedTxRequests = yield* Effect.forEach(
      txRequests,
      (entry) => MempoolDB.toProcessedTx(entry),
      { concurrency },
    );
    const mempoolTxHashes = processedTxRequests.map(
      (processedTx) => processedTx.txId,
    );
    return {
      mempoolTxHashes,
      processedTxRequests,
    };
  });

/*
 * Add produced ledger entries and corresponding address history entries for
 * the deposit events.
 */
const processDepositsProgram = (deposits: readonly UserEvents.Entry[]) =>
  Effect.gen(function* () {
    const depositLedgerEntries: Ledger.Entry[] = [];
    const depositAddressHistoryEntries: AddressHistoryDB.Entry[] = [];
    yield* Effect.forEach(deposits, (deposit) =>
      Effect.gen(function* () {
        const ledgerEntry = yield* DepositsDB.entryToLedgerEntry(deposit);
        const addressHistoryEntry = yield* AddressHistoryDB.depositEntryToEntry(
          deposit,
          AddressHistoryDB.Status.SUBMITTED,
        );
        depositLedgerEntries.push(ledgerEntry);
        depositAddressHistoryEntries.push(addressHistoryEntry);
      }),
    );
    return {
      depositLedgerEntries,
      depositAddressHistoryEntries,
    };
  });

/**
 * Given an event interval, this function looks up the user events tables plus
 * `MempoolDB` to gather all the events that fall within that window. Uses
 * `LatestLedgerDB` as the state prior to the collected events.
 *
 * Here we are trusting the built and signed block transaction. Meaning, we
 * don't validate the order in which we apply the events to the ledger.
 *
 * Returns a set of values prepared for updating relevant tables.
 */
const processEventsForLedgerApplication = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  {
    txRequests: readonly MempoolDB.EntryWithEffects[];
    allProducedLedgerEntries: Ledger.Entry[];
    allSpentOutRefs: Buffer[];
    mempoolTxHashes: Buffer[];
    allAddressHistoryEntries: AddressHistoryDB.Entry[];
  },
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | DatabaseError
  | NotFoundError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const txParseConcurrency = nodeConfig.TX_PARSE_CONCURRENCY;
    const blockEvents = yield* BlocksDB.retrieveEvents(startDate, endDate);

    const { withdrawnOutRefs, withdrawalAddressHistoryEntries } =
      yield* processWithdrawalsProgram(blockEvents.withdrawals);

    const processedTxOrders = yield* processTxOrdersProgram(
      blockEvents.txOrders,
      txParseConcurrency,
    );

    const { mempoolTxHashes, processedTxRequests } =
      yield* processTxRequestsProgram(
        blockEvents.txRequests,
        txParseConcurrency,
      );

    const { depositLedgerEntries, depositAddressHistoryEntries } =
      yield* processDepositsProgram(blockEvents.deposits);

    // Going through all the collected `ProcessedTx` values and getting prepared
    // values for updating the ledger, and also adding/updating entries to
    // `AddressHistoryDB`.
    const {
      addressHistoryEntries: l2TxsAddressHistoryEntries,
      collectiveProduced,
      collectiveSpent,
    } = yield* AddressHistoryDB.aggregateProcessedTxs(
      LatestLedgerDB.tableName,
      [...processedTxOrders, ...processedTxRequests],
      AddressHistoryDB.Status.SUBMITTED,
    );

    const allAddressHistoryEntries: AddressHistoryDB.Entry[] = [
      ...withdrawalAddressHistoryEntries,
      ...l2TxsAddressHistoryEntries,
      ...depositAddressHistoryEntries,
    ];
    const allProducedLedgerEntries: Ledger.Entry[] = [
      ...collectiveProduced,
      ...depositLedgerEntries,
    ];
    const allSpentOutRefs: Buffer[] = [...withdrawnOutRefs, ...collectiveSpent];

    return {
      txRequests: blockEvents.txRequests,
      allProducedLedgerEntries,
      allSpentOutRefs,
      mempoolTxHashes,
      allAddressHistoryEntries,
    };
  });

// Checks whether the commitment tx for a block is already on L1 by querying
// for any of its produced UTxOs. Returns false on any provider error so the
// caller falls through to normal submission.
const checkL1TxProgram = (
  producedUtxosBytes: Buffer,
  headerHashHex: string,
): Effect.Effect<boolean, never, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const producedUTxOs =
      yield* deserializeUTxOsFromStorage(producedUtxosBytes);
    if (producedUTxOs.length === 0) {
      return false;
    }
    const outRefs = producedUTxOs.map((utxo) => ({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
    }));
    const foundUTxOs = yield* Effect.promise(() =>
      lucid.api.utxosByOutRef(outRefs),
    );
    const found = foundUTxOs.length > 0;
    if (found) {
      yield* Effect.logInfo(
        `🔗 ✅ Commitment tx already on L1 (header_hash=${headerHashHex}), skipping submission.`,
      );
    }
    return found;
  }).pipe(
    Effect.catchAllCause((cause) =>
      Effect.logWarning(
        `🔗 ⚠️  L1 pre-check failed for header_hash=${headerHashHex}, will attempt submission: ${Cause.pretty(cause)}`,
      ).pipe(Effect.as(false)),
    ),
  );

export const submitEarliestBlock = Effect.gen(function* () {
  const optPendingBlock = yield* BlocksDB.retrieveEarliestPendingEntry;
  yield* Option.match(optPendingBlock, {
    onNone: () =>
      Effect.gen(function* () {
        yield* Effect.logInfo("No unsubmitted blocks in queue.");
        yield* refreshUnsubmittedBacklogGaugeFromDb;
      }),
    onSome: (blockEntry) =>
      Effect.gen(function* () {
        const headerHashHex = SDK.bufferToHex(
          blockEntry[BlocksDB.Columns.HEADER_HASH],
        );

        // Mark SUBMITTING before any L1 interaction so crash-recovery on
        // restart can distinguish in-flight blocks from never-tried ones (M-53).
        yield* BlocksDB.setStatusOfEntry(
          blockEntry,
          BlocksDB.Status.SUBMITTING,
        );

        yield* Effect.logInfo("🔗 ✉️  Submitting block commitment...");

        // L1 pre-check: if the commitment tx already landed (e.g. a prior
        // attempt timed out), skip sign+submit and go straight to DB apply
        // (M-52).
        const txAlreadyOnL1 = yield* checkL1TxProgram(
          blockEntry[BlocksDB.Columns.PRODUCED_UTXOS],
          headerHashHex,
        );
        let submissionTxCborBytes = blockEntry[BlocksDB.Columns.L1_CBOR];

        if (!txAlreadyOnL1) {
          yield* submitTxCborWithoutSigningProgram(
            submissionTxCborBytes,
            headerHashHex,
          ).pipe(
            Effect.catchTag("TxSignError", (signError) =>
              Effect.gen(function* () {
                if (!isWrappedTxSignerError(signError)) {
                  return yield* Effect.fail(signError);
                }
                yield* Effect.logInfo(
                  `🔗 ✍️  Missing/invalid signatures for header_hash=${headerHashHex}; signing once and persisting signed artifact.`,
                );
                const signedTx = yield* signTxFromCborProgram(
                  submissionTxCborBytes,
                  headerHashHex,
                );
                const signedTxCborBytes =
                  yield* signedTxToCborBytesProgram(signedTx);
                yield* BlocksDB.setL1CborOfEntry(blockEntry, signedTxCborBytes);
                submissionTxCborBytes = signedTxCborBytes;
                const nodeConfig = yield* NodeConfig;
                const timeoutMs = Math.max(
                  SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS,
                  nodeConfig.SUBMIT_SIGNED_TX_TIMEOUT_MS,
                );
                return yield* submitSignedTxProgram(
                  signedTx,
                  headerHashHex,
                  timeoutMs,
                );
              }),
            ),
            Effect.tap((txHash) =>
              Effect.logInfo(`🔗 🚀 Block commitment submitted: ${txHash}`),
            ),
            Effect.asVoid,
            Effect.catchTag("TxSubmitError", (submitError) =>
              Effect.gen(function* () {
                if (!isIdempotentSubmitErrorCandidate(submitError)) {
                  return yield* Effect.fail(submitError);
                }
                yield* Effect.logWarning(
                  `🔗 ⚠️  Submit failed with an idempotent-success candidate for header_hash=${headerHashHex}; re-checking L1 inclusion.`,
                );
                const confirmedOnL1 = yield* checkL1TxProgram(
                  blockEntry[BlocksDB.Columns.PRODUCED_UTXOS],
                  headerHashHex,
                );
                if (!confirmedOnL1) {
                  return yield* Effect.fail(submitError);
                }
                yield* Effect.logInfo(
                  `🔗 ✅ Idempotent submit recovery succeeded for header_hash=${headerHashHex}; continuing with DB apply and status transition.`,
                );
                return;
              }),
            ),
          );
        }
        yield* extractL1CommitmentFeeLovelace(submissionTxCborBytes).pipe(
          Effect.flatMap((l1CommitmentFeeLovelace) =>
            Effect.all([
              Metric.incrementBy(
                blockSubmissionMetrics.l1CommitmentFeesLovelaceCounter,
                l1CommitmentFeeLovelace,
              ),
              Metric.set(
                blockSubmissionMetrics.l1CommitmentFeeLovelaceLastGauge,
                l1CommitmentFeeLovelace,
              ),
            ]),
          ),
          Effect.catchAll((error) =>
            Effect.logWarning(
              `Failed to update L1 commitment fee metrics for block ${SDK.bufferToHex(
                blockEntry[BlocksDB.Columns.HEADER_HASH],
              )}: ${error.message}`,
            ),
          ),
        );

        const {
          txRequests,
          allProducedLedgerEntries,
          allSpentOutRefs,
          mempoolTxHashes,
          allAddressHistoryEntries,
        } = yield* processEventsForLedgerApplication(
          blockEntry[BlocksDB.Columns.EVENT_START_TIME],
          blockEntry[BlocksDB.Columns.EVENT_END_TIME],
        );

        const addToLedgerProgram = batchProgram(
          resolveSubmissionBatchSize(allProducedLedgerEntries.length),
          allProducedLedgerEntries.length,
          "Insert new entries to LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.insertMultipleOrIgnore(
              allProducedLedgerEntries.slice(startIndex, endIndex),
            ),
          1,
        );

        const removeFromLedgerProgram = batchProgram(
          resolveSubmissionBatchSize(allSpentOutRefs.length),
          allSpentOutRefs.length,
          "Remove spent outrefs from LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.clearUTxOs(
              allSpentOutRefs.slice(startIndex, endIndex),
            ),
          1,
        );

        // We intentionally apply inserts before removals so same-block spends
        // of newly produced UTxOs are resolved deterministically.
        const updateLatestLedgerDBProgram = Effect.gen(function* () {
          yield* addToLedgerProgram;
          yield* removeFromLedgerProgram;
        });

        const transferMempoolTxsProgram = batchProgram(
          resolveSubmissionBatchSize(txRequests.length),
          txRequests.length,
          "Transfer of MempoolDB entries to ImmutableDB and BlocksTxsDB",
          (startIndex, endIndex) => {
            const txsBatch = txRequests
              .slice(startIndex, endIndex)
              .map((entry) => MempoolDB.toTxEntry(entry));
            const txHashesBatch = mempoolTxHashes.slice(startIndex, endIndex);
            return Effect.gen(function* () {
              yield* ImmutableDB.insertTxsOrIgnore(txsBatch);
              yield* BlocksTxsDB.insertOrIgnore(
                blockEntry[BlocksDB.Columns.HEADER_HASH],
                txHashesBatch,
              );
              yield* MempoolDB.clearTxs(txHashesBatch);
            });
          },
          1,
        );

        const addToAddressHistoryProgram = batchProgram(
          resolveSubmissionBatchSize(allAddressHistoryEntries.length),
          allAddressHistoryEntries.length,
          "Insert AddressHistoryDB entries for all events",
          (startIndex, endIndex) =>
            AddressHistoryDB.upsertEntries(
              allAddressHistoryEntries.slice(startIndex, endIndex),
            ),
          1,
        );

        const sql = yield* SqlClient.SqlClient;
        yield* sql.withTransaction(
          Effect.gen(function* () {
            yield* updateLatestLedgerDBProgram;
            yield* transferMempoolTxsProgram;
            yield* addToAddressHistoryProgram;
            yield* BlocksDB.setStatusOfEntry(
              blockEntry,
              BlocksDB.Status.SUBMITTED,
            );
          }),
        );
        yield* Metric.increment(blockSubmissionMetrics.submitBlockCounter);
        yield* refreshUnsubmittedBacklogGaugeFromDb;
      }),
  });
});

// On startup, resets any blocks left in SUBMITTING state (crashed mid-flight)
// back to UNSUBMITTED so the normal loop picks them up with the L1 pre-check.
export const reconcileSubmittingBlocks: Effect.Effect<void, never, Database> =
  Effect.gen(function* () {
    const count = yield* BlocksDB.resetSubmittingToUnsubmitted;
    if (count > 0) {
      yield* Effect.logInfo(
        `🔗 🔄 Reconciled ${count} SUBMITTING block(s) back to UNSUBMITTED for L1 pre-check on retry.`,
      );
    }
  }).pipe(Effect.catchAllCause(Effect.logWarning));

export const blockSubmissionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  NodeConfig | Database | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Block submission fiber started.");
    yield* reconcileSubmittingBlocks;
    yield* reconcileSubmissionMetricsFromDb.pipe(
      Effect.catchAllCause(Effect.logWarning),
    );
    const action = submitEarliestBlock.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.tapError(() =>
        Metric.increment(blockSubmissionMetrics.submitBlockFailuresCounter),
      ),
      Effect.ensuring(
        refreshUnsubmittedBacklogGaugeFromDb.pipe(
          Effect.catchAllCause(Effect.logWarning),
        ),
      ),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
