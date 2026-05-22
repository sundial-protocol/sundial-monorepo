import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, fromHex } from "@lucid-evolution/lucid";
import { BlocksDB } from "@/database/index.js";
import {
  DatabaseError,
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import { AlwaysSucceedsContract, Database, Lucid } from "@/services/index.js";
import { Effect, Metric, MetricBoundaries, Option } from "effect";
import { performance } from "node:perf_hooks";

const MAX_STATE_QUEUE_TRAVERSAL_HOPS = 100_000;

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

const fetchTailByTraversal = (
  lucid: LucidEvolution,
  config: SDK.StateQueueFetchConfig,
): Effect.Effect<
  { tail: SDK.StateQueueUTxO; traversedHops: number },
  | SDK.LucidError
  | SDK.DataCoercionError
  | SDK.MissingDatumError
  | SDK.UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const { confirmed, link } =
      yield* SDK.fetchConfirmedStateAndItsLinkByUnitProgram(lucid, config);
    if (link === undefined) {
      return { tail: confirmed, traversedHops: 0 };
    }

    let traversedHops = 1;
    let current = link;

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

      const nextUnit =
        config.stateQueuePolicyId +
        SDK.NODE_ASSET_NAME +
        current.datum.next.Key.key;
      current = yield* SDK.fetchLatestCommittedBlockByUnitProgram(
        lucid,
        config,
        nextUnit,
      );
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
  const { stateQueue } = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: stateQueue.spendingScriptAddress,
    stateQueuePolicyId: stateQueue.policyId,
  };

  yield* lucid.switchToOperatorsBlockCommitmentWallet;
  const { tail: latestStateQueueUTxO, traversedHops } =
    yield* fetchTailByTraversal(lucid.api, fetchConfig);
  const headerHashHex =
    yield* SDK.headerHashFromStateQueueUTxO(latestStateQueueUTxO);

  const walletUTxOs = yield* Effect.tryPromise({
    try: () => lucid.api.wallet().getUtxos(),
    catch: (e) =>
      new SDK.LucidError({
        message: "Failed to fetch wallet UTxOs for BlocksDB seeding",
        cause: e,
      }),
  });
  const serializedWalletUTxOs = yield* serializeUTxOsForStorage(walletUTxOs);
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
  AlwaysSucceedsContract | Database | Lucid
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

  yield* Metric.increment(seedBlocksDbAttemptsCounter);
  const seedStartMs = performance.now();
  const seedResult = yield* Effect.either(seedBlocksDBFromChain);
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
