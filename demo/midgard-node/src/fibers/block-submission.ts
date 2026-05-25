import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import { Effect, Metric, Option, Schedule } from "effect";
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
  l1CommitmentFeesLovelaceCounter,
  l1CommitmentFeeLovelaceLastGauge,
  unsubmittedBlockBacklogGauge,
} as const;

export const initializeSubmissionMetrics = Effect.all([
  Metric.incrementBy(blockSubmissionMetrics.submitBlockCounter, 0n),
  Metric.incrementBy(
    blockSubmissionMetrics.l1CommitmentFeesLovelaceCounter,
    0n,
  ),
  Metric.set(blockSubmissionMetrics.l1CommitmentFeeLovelaceLastGauge, 0n),
  Metric.set(blockSubmissionMetrics.unsubmittedBlockBacklogGauge, 0n),
]);

const loadSubmissionMetricsBaselineFromDb = Effect.gen(function* () {
  const [submittedOrLaterCount, unsubmittedCount] = yield* Effect.all(
    [
      BlocksDB.countWithMinimumStatus(BlocksDB.Status.SUBMITTED),
      BlocksDB.countByStatus(BlocksDB.Status.UNSUBMITTED),
    ],
    { concurrency: "unbounded" },
  );
  return {
    submittedOrLaterCount,
    unsubmittedCount,
  };
});

const reconcileSubmissionMetricsFromDb = Effect.gen(function* () {
  // On node boot, restore counters/gauges from persisted block statuses.
  yield* initializeSubmissionMetrics;
  const { submittedOrLaterCount, unsubmittedCount } =
    yield* loadSubmissionMetricsBaselineFromDb;
  yield* Metric.incrementBy(
    blockSubmissionMetrics.submitBlockCounter,
    submittedOrLaterCount,
  );
  yield* Metric.set(
    blockSubmissionMetrics.unsubmittedBlockBacklogGauge,
    unsubmittedCount,
  );
});

const refreshUnsubmittedBacklogGaugeFromDb = Effect.gen(function* () {
  const unsubmittedCount = yield* BlocksDB.countByStatus(
    BlocksDB.Status.UNSUBMITTED,
  );
  yield* Metric.set(
    blockSubmissionMetrics.unsubmittedBlockBacklogGauge,
    unsubmittedCount,
  );
});

// For database operations.
const BATCH_SIZE = 100;
const SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS = 30_000;

const submitSignedTxCBOR = (
  l1CborBytes: Buffer,
  headerHashHex: string,
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | SDK.LucidError,
  Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const signedTxHex = SDK.bufferToHex(l1CborBytes);
    const timeoutMs = Math.max(
      SUBMIT_SIGNED_TX_TIMEOUT_FALLBACK_MS,
      nodeConfig.WAIT_BETWEEN_BLOCK_SUBMISSIONS,
    );
    // Some commitment txs require an additional operator witness (e.g. merge
    // signer) beyond the witness already embedded by the commitment worker.
    // Re-signing with the merge wallet preserves existing witnesses while
    // appending the currently required one before submit.
    yield* lucid.switchToOperatorsMergingWallet;
    const signedTx = yield* lucid.api
      .fromTx(signedTxHex)
      .sign.withWallet()
      .completeProgram();
    return yield* signedTx.submitProgram().pipe(
      Effect.timeoutFail({
        duration: `${timeoutMs} millis`,
        onTimeout: () =>
          new TxSubmitError({
            message: `Timed out after ${timeoutMs}ms while submitting L1 commitment tx (header_hash=${headerHashHex})`,
            cause: "Timed out waiting for submitProgram()",
            txHash: "<unknown>",
          }),
      }),
    );
  }).pipe(
    Effect.mapError((e) => {
      const commonMsg = "Failed to submit previously built and signed tx";
      if (e._tag === "TxSubmitError") {
        return new TxSubmitError({
          message: `${commonMsg}: ${e.message}`,
          cause: e,
          txHash: "<unknown>",
        });
      } else if (e._tag === "TxSignerError") {
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

export const submitEarliestBlock = Effect.gen(function* () {
  const optUnsubmittedBlock = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;
  yield* Option.match(optUnsubmittedBlock, {
    onNone: () =>
      Effect.gen(function* () {
        yield* Effect.logInfo("No unsubmitted blocks in queue.");
        yield* refreshUnsubmittedBacklogGaugeFromDb;
      }),
    onSome: (blockEntry) =>
      Effect.gen(function* () {
        yield* Effect.logInfo("🔗 ✉️  Submitting block commitment...");
        const txHash = yield* submitSignedTxCBOR(
          blockEntry[BlocksDB.Columns.L1_CBOR],
          SDK.bufferToHex(blockEntry[BlocksDB.Columns.HEADER_HASH]),
        );
        yield* Effect.logInfo(`🔗 🚀 Block commitment submitted: ${txHash}`);
        yield* extractL1CommitmentFeeLovelace(
          blockEntry[BlocksDB.Columns.L1_CBOR],
        ).pipe(
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
          BATCH_SIZE,
          allProducedLedgerEntries.length,
          "Insert new entries to LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.insertMultipleOrIgnore(
              allProducedLedgerEntries.slice(startIndex, endIndex),
            ),
          1,
        );

        const removeFromLedgerProgram = batchProgram(
          BATCH_SIZE,
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
          BATCH_SIZE,
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
          BATCH_SIZE,
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

export const blockSubmissionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  NodeConfig | Database | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Block submission fiber started.");
    yield* reconcileSubmissionMetricsFromDb.pipe(
      Effect.catchAllCause(Effect.logWarning),
    );
    const action = submitEarliestBlock.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.ensuring(
        refreshUnsubmittedBacklogGaugeFromDb.pipe(
          Effect.catchAllCause(Effect.logWarning),
        ),
      ),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
