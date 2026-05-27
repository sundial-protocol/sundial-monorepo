import * as SDK from "@al-ft/midgard-sdk";
import { fromHex } from "@lucid-evolution/lucid";
import { BlocksDB } from "@/database/index.js";
import {
  DatabaseError,
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Metric, MetricBoundaries, Option } from "effect";
import { performance } from "node:perf_hooks";

const MAX_STATE_QUEUE_TRAVERSAL_HOPS = 100_000;
const BLOCKS_DB_SEED_TIMEOUT_FLOOR_MS = 120_000;

const seedBlocksDbAttemptsCounter = Metric.counter(
  "blocks_db_seed_attempts_total",
  {
    description: "Total number of cold-start attempts to seed BlocksDB",
    bigint: true,
    incremental: true,
  },
).register();

const seedBlocksDbSuccessCounter = Metric.counter(
  "blocks_db_seed_success_total",
  {
    description: "Total number of successful BlocksDB seed operations",
    bigint: true,
    incremental: true,
  },
).register();

const seedBlocksDbFailuresCounter = Metric.counter(
  "blocks_db_seed_failures_total",
  {
    description: "Total number of failed BlocksDB seed operations",
    bigint: true,
    incremental: true,
  },
).register();

const seedBlocksDbTraversalHopsGauge = Metric.gauge(
  "blocks_db_seed_traversal_hops_last",
  {
    description:
      "Number of state-queue link traversals performed during the most recent successful BlocksDB seed",
  },
).register();

const seedBlocksDbDurationHistogram = Metric.histogram(
  "blocks_db_seed_duration_seconds",
  MetricBoundaries.exponential({ start: 0.1, factor: 2, count: 13 }),
  "Histogram of cold-start BlocksDB seed duration",
).register();

export const blocksDbSeedingMetrics = {
  seedBlocksDbAttemptsCounter,
  seedBlocksDbSuccessCounter,
  seedBlocksDbFailuresCounter,
  seedBlocksDbTraversalHopsGauge,
  seedBlocksDbDurationHistogram,
} as const;

type SeedResult = "already-seeded" | "seeded" | "retry-later";

const followRootLinkedTailFromSnapshot = (
  firstLinkedNode: SDK.StateQueueUTxO,
  snapshot: readonly SDK.StateQueueUTxO[],
): Effect.Effect<{ tail: SDK.StateQueueUTxO; traversedHops: number }, SDK.LucidError> =>
  Effect.gen(function* () {
    const byKey = new Map<string, SDK.StateQueueUTxO>();
    for (const stateQueueUTxO of snapshot) {
      if (stateQueueUTxO.datum.key !== "Empty") {
        byKey.set(stateQueueUTxO.datum.key.Key.key, stateQueueUTxO);
      }
    }

    let traversedHops = 1;
    let current = firstLinkedNode;
    while (current.datum.next !== "Empty") {
      if (traversedHops > MAX_STATE_QUEUE_TRAVERSAL_HOPS) {
        return yield* Effect.fail(
          new SDK.LucidError({
            message:
              "State-queue traversal exceeded maximum allowed hops while seeding BlocksDB",
            cause: `max_hops=${MAX_STATE_QUEUE_TRAVERSAL_HOPS}`,
          }),
        );
      }
      const nextKey = current.datum.next.Key.key;
      const next = byKey.get(nextKey);
      if (next === undefined) {
        return yield* Effect.fail(
          new SDK.LucidError({
            message:
              "Failed to follow root-linked state-queue path while seeding BlocksDB",
            cause: `missing_link_key=${nextKey}`,
          }),
        );
      }
      current = next;
      traversedHops += 1;
    }

    return { tail: current, traversedHops };
  });

const seedBlocksDBFromChain: Effect.Effect<
  { traversedHops: number },
  | SDK.LucidError
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.MissingDatumError
  | SDK.UnauthenticUtxoError
  | DatabaseError,
  AlwaysSucceedsContract | Database | Lucid
> = Effect.gen(function* () {
  yield* Effect.logInfo(
    "🔹 BlocksDB is empty - attempting to seed from chain...",
  );
  const lucid = yield* Lucid;
  const blockCommitmentApi = lucid.blockCommitmentApi;
  const { stateQueue } = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: stateQueue.spendingScriptAddress,
    stateQueuePolicyId: stateQueue.policyId,
  };
  const { confirmed, link } =
    yield* SDK.fetchConfirmedStateAndItsLinkByUnitProgram(
      blockCommitmentApi,
      fetchConfig,
    );
  const { tail: latestStateQueueUTxO, traversedHops } =
    link === undefined
      ? { tail: confirmed, traversedHops: 0 }
      : yield* Effect.gen(function* () {
          // Snapshot once and walk the root-linked chain in memory.
          const snapshot = yield* SDK.fetchUnsortedStateQueueUTxOsProgram(
            blockCommitmentApi,
            fetchConfig,
          );
          return yield* followRootLinkedTailFromSnapshot(link, snapshot);
        });
  const headerHashHex =
    yield* SDK.headerHashFromStateQueueUTxO(latestStateQueueUTxO);
  const serializedWalletUTxOs = yield* serializeUTxOsForStorage([]);
  const serializedProducedUTxOs = yield* serializeUTxOsForStorage([
    latestStateQueueUTxO.utxo,
  ]);

  const now = new Date();
  const seedEntry: BlocksDB.EntryNoMeta = {
    [BlocksDB.Columns.HEADER_HASH]: Buffer.from(fromHex(headerHashHex)),
    [BlocksDB.Columns.EVENT_START_TIME]: now,
    [BlocksDB.Columns.EVENT_END_TIME]: now,
    [BlocksDB.Columns.NEW_WALLET_UTXOS]: serializedWalletUTxOs,
    [BlocksDB.Columns.PRODUCED_UTXOS]: serializedProducedUTxOs,
    [BlocksDB.Columns.L1_CBOR]: Buffer.alloc(0),
    [BlocksDB.Columns.STATUS]: BlocksDB.Status.SUBMITTED,
    [BlocksDB.Columns.DEPOSITS_COUNT]: 0,
    [BlocksDB.Columns.TX_REQUESTS_COUNT]: 0,
    [BlocksDB.Columns.TX_ORDERS_COUNT]: 0,
    [BlocksDB.Columns.WITHDRAWALS_COUNT]: 0,
    [BlocksDB.Columns.TOTAL_EVENTS_SIZE]: 0,
  };

  yield* BlocksDB.upsert(seedEntry);
  yield* Effect.logInfo(
    `🔹 ✅ BlocksDB seeded from chain successfully. traversed_hops=${traversedHops}`,
  );
  return { traversedHops };
});

export const ensureBlocksDBSeededFromChain: Effect.Effect<
  SeedResult,
  never,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.gen(function* () {
  const optLatestBlockResult = yield* Effect.either(
    BlocksDB.retrieveLatestEntry,
  );
  if (optLatestBlockResult._tag === "Left") {
    yield* Metric.increment(seedBlocksDbFailuresCounter);
    yield* Effect.logWarning(
      `Failed to read latest block while checking seed precondition. Will retry on next cycle. cause=${optLatestBlockResult.left.message}`,
    );
    return "retry-later";
  }

  const optLatestBlock = optLatestBlockResult.right;
  if (Option.isSome(optLatestBlock)) {
    return "already-seeded";
  }

  const nodeConfig = yield* NodeConfig;
  const seedTimeoutMs = Math.max(
    BLOCKS_DB_SEED_TIMEOUT_FLOOR_MS,
    nodeConfig.COMMITMENT_WORKER_TIMEOUT_MS,
  );

  yield* Metric.increment(seedBlocksDbAttemptsCounter);
  const seedStartMs = performance.now();
  const seedResult = yield* Effect.either(
    seedBlocksDBFromChain.pipe(
      Effect.timeoutFail({
        duration: `${seedTimeoutMs} millis`,
        onTimeout: () =>
          new SDK.LucidError({
            message: `Timed out after ${seedTimeoutMs}ms while seeding BlocksDB from chain`,
            cause: "Timed out waiting for provider/state-queue data",
          }),
      }),
    ),
  );
  yield* Metric.update(
    seedBlocksDbDurationHistogram,
    (performance.now() - seedStartMs) / 1000,
  );

  if (seedResult._tag === "Right") {
    yield* Metric.increment(seedBlocksDbSuccessCounter);
    yield* Metric.set(
      seedBlocksDbTraversalHopsGauge,
      seedResult.right.traversedHops,
    );
    return "seeded";
  }

  yield* Metric.increment(seedBlocksDbFailuresCounter);
  yield* Effect.logWarning(
    `Failed to seed BlocksDB from chain. Will retry on next cycle. cause=${seedResult.left.message}`,
  );
  return "retry-later";
});
