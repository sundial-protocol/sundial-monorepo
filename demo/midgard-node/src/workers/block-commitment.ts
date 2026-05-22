import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Option, pipe } from "effect";
import {
  applyTxRequestsToLedger,
  applyDepositsToLedger,
  WorkerInput,
  WorkerOutput,
  applyWithdrawalsToLedger,
  applyTxOrdersToLedger,
  buildNewBlockEntry,
  applyBlockCommitmentLedgerProjection,
} from "./utils/block-commitment.js";
import {
  Database,
  Lucid,
  AlwaysSucceedsContract,
  NodeConfig,
} from "@/services/index.js";
import {
  MempoolLedgerDB,
  BlocksDB,
  MempoolDB,
  Tx,
  UserEvents,
} from "@/database/index.js";
import { TxSignError } from "@/transactions/utils.js";
import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql";

const sumBufferBytes = (buffers: readonly Buffer[]): number =>
  buffers.reduce((acc, next) => acc + next.length, 0);

const buildPreflightWindowStats = (events: BlocksDB.Events): BlocksDB.Stats => {
  const { withdrawals, txOrders, txRequests, deposits } = events;
  return {
    [BlocksDB.Columns.DEPOSITS_COUNT]: deposits.length,
    [BlocksDB.Columns.TX_REQUESTS_COUNT]: txRequests.length,
    [BlocksDB.Columns.TX_ORDERS_COUNT]: txOrders.length,
    [BlocksDB.Columns.WITHDRAWALS_COUNT]: withdrawals.length,
    [BlocksDB.Columns.TOTAL_EVENTS_SIZE]:
      sumBufferBytes(
        withdrawals.map((entry) => entry[UserEvents.Columns.INFO]),
      ) +
      sumBufferBytes(txOrders.map((entry) => entry[UserEvents.Columns.INFO])) +
      sumBufferBytes(txRequests.map((entry) => entry[Tx.Columns.TX])) +
      sumBufferBytes(deposits.map((entry) => entry[UserEvents.Columns.INFO])),
  };
};

const mainProgram: Effect.Effect<
  string | BlocksDB.Stats,
  | SDK.CborDeserializationError
  | SDK.CborSerializationError
  | SDK.CmlDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | MptError
  | TxSignError,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.gen(function* () {
  const optLatestBlock = yield* BlocksDB.retrieveLatestEntry;
  return yield* Option.match(optLatestBlock, {
    onNone: () =>
      Effect.succeed(
        "BlocksDB is empty; cold-start seeding must complete before running commitment worker",
      ),
    onSome: (latestBlock) =>
      Effect.gen(function* () {
        const nodeConfig = yield* NodeConfig;
        const currentDate = new Date();
        const startDate = latestBlock[BlocksDB.Columns.EVENT_END_TIME];
        const events = yield* BlocksDB.retrieveEventsForCommitment(
          latestBlock,
          currentDate,
        );
        const { withdrawals, txOrders, txRequests, deposits } = events;
        const processedTxRequests = yield* Effect.forEach(txRequests, (entry) =>
          MempoolDB.toProcessedTx(entry),
        );
        const preflightStats = buildPreflightWindowStats(events);
        const thresholdBreaches =
          BlocksDB.getCommitmentWindowWarningThresholdBreaches(preflightStats, {
            txRequestsCount: nodeConfig.COMMITMENT_WINDOW_WARN_TX_REQUESTS,
            totalEventsCount: nodeConfig.COMMITMENT_WINDOW_WARN_TOTAL_EVENTS,
            totalEventsSizeBytes: nodeConfig.COMMITMENT_WINDOW_WARN_TOTAL_BYTES,
          });
        const totalEventsCount = BlocksDB.getTotalEventsCount(preflightStats);
        yield* Effect.logInfo(
          `Commitment preflight window: start=${startDate.toISOString()} end=${currentDate.toISOString()} duration_ms=${currentDate.getTime() - startDate.getTime()} tx_requests=${preflightStats[BlocksDB.Columns.TX_REQUESTS_COUNT]} tx_orders=${preflightStats[BlocksDB.Columns.TX_ORDERS_COUNT]} deposits=${preflightStats[BlocksDB.Columns.DEPOSITS_COUNT]} withdrawals=${preflightStats[BlocksDB.Columns.WITHDRAWALS_COUNT]} total_events=${totalEventsCount} total_events_size_bytes=${preflightStats[BlocksDB.Columns.TOTAL_EVENTS_SIZE]}`,
        );
        if (thresholdBreaches.length > 0) {
          yield* Effect.logWarning(
            `Commitment preflight threshold breach: header_hash=${latestBlock[BlocksDB.Columns.HEADER_HASH].toString("hex")} breaches=${thresholdBreaches.join(",")} tx_requests=${preflightStats[BlocksDB.Columns.TX_REQUESTS_COUNT]}/${nodeConfig.COMMITMENT_WINDOW_WARN_TX_REQUESTS} total_events=${totalEventsCount}/${nodeConfig.COMMITMENT_WINDOW_WARN_TOTAL_EVENTS} total_events_size_bytes=${preflightStats[BlocksDB.Columns.TOTAL_EVENTS_SIZE]}/${nodeConfig.COMMITMENT_WINDOW_WARN_TOTAL_BYTES}`,
          );
        }
        const ledgerTrie = yield* MidgardMpt.create(
          "ledger",
          nodeConfig.LEDGER_MPT_DB_PATH,
        );
        yield* ledgerTrie.checkpoint();

        return yield* Effect.gen(function* () {
          const { withdrawnOutRefs, withdrawalsRoot, sizeOfWithdrawals } =
            yield* applyWithdrawalsToLedger(ledgerTrie, withdrawals);
          const {
            txOrdersCount,
            spentByTxOrders,
            producedByTxOrders,
            txsTrie,
            sizeOfTxOrders,
          } = yield* applyTxOrdersToLedger(ledgerTrie, txOrders);
          const { txRequestsCount, txsRoot, sizeOfTxRequests } =
            yield* applyTxRequestsToLedger(
              ledgerTrie,
              txsTrie,
              processedTxRequests,
            );
          const { depositLedgerEntries, depositsRoot, sizeOfDeposits } =
            yield* applyDepositsToLedger(ledgerTrie, deposits);

          const ledgerRoot = yield* ledgerTrie.getRootHex();

          const stats: BlocksDB.Stats = {
            [BlocksDB.Columns.DEPOSITS_COUNT]: depositLedgerEntries.length,
            [BlocksDB.Columns.TX_REQUESTS_COUNT]: txRequestsCount,
            [BlocksDB.Columns.TX_ORDERS_COUNT]: txOrdersCount,
            [BlocksDB.Columns.WITHDRAWALS_COUNT]: withdrawnOutRefs.length,
            [BlocksDB.Columns.TOTAL_EVENTS_SIZE]:
              sizeOfWithdrawals +
              sizeOfTxOrders +
              sizeOfTxRequests +
              sizeOfDeposits,
          };

          const newBlockEntry = yield* buildNewBlockEntry(
            latestBlock,
            ledgerRoot,
            txsRoot,
            depositsRoot,
            withdrawalsRoot,
            currentDate,
            stats,
          );

          const sql = yield* SqlClient.SqlClient;

          // TODO: We are not adding any entries to `AddressHistoryDB` here, but
          //       we probably should.
          yield* sql
            .withTransaction(
              Effect.gen(function* () {
                yield* BlocksDB.upsert(newBlockEntry);
                yield* applyBlockCommitmentLedgerProjection(
                  depositLedgerEntries,
                  producedByTxOrders,
                  withdrawnOutRefs,
                  spentByTxOrders,
                );
              }),
            )
            .pipe(
              sqlErrorToDatabaseError(
                `(${BlocksDB.tableName} & ${MempoolLedgerDB.tableName})`,
                "Updating MempoolLedgerDB and BlocksDB after updating ledger MPT for block commitment failed",
              ),
            );
          yield* ledgerTrie.commit();
          return stats;
        }).pipe(Effect.tapError((_) => ledgerTrie.revert()));
      }),
  });
});

const wrapper = (_workerInput: WorkerInput) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Starting block commitment process...");
    const result = yield* mainProgram;
    if (typeof result === "string") {
      const output: WorkerOutput = {
        type: "FailureOutput",
        error: result,
      };
      return output;
    }
    const output: WorkerOutput = {
      type: "SuccessfulCommitmentOutput",
      stats: result,
    };
    return output;
  });

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(Database.layer),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
);

const serializeError = (e: unknown, depth = 0): string => {
  if (depth > 5 || e == null) return String(e);
  if (e instanceof Error) {
    const errCause = (e as unknown as Record<string, unknown>).cause;
    const causeStr =
      errCause != null
        ? `\n  cause: ${serializeError(errCause, depth + 1)}`
        : "";
    return `${e.constructor?.name ?? "Error"}: ${e.message}${causeStr}`;
  }
  if (typeof e === "object" && "message" in e) {
    const cause = (e as Record<string, unknown>).cause;
    const causeStr =
      cause != null ? `\n  cause: ${serializeError(cause, depth + 1)}` : "";
    return `${String((e as Record<string, unknown>).message)}${causeStr}`;
  }
  return String(e);
};

Effect.runPromise(
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailureOutput",
        error: `Block commitment worker failure: ${serializeError(Cause.squash(cause))}`,
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `👷 Block commitment work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
