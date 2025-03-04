import { LucidEvolution, OutRef } from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule } from "effect";
import { parentPort } from "worker_threads";

import { makeConfig, makeUserFn } from "@/config.js";
import { UtilsDB } from "@/database/index.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { StateQueueTx, UtilsTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { logInfo } from "effect/Effect";
import pg from "pg";
let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };

const monitorStateQueue = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  pool: pg.Pool
) =>
  Effect.gen(function* () {
    console.log("monitorStateQueue... :>> ");
    const latestBlock = yield* SDK.Endpoints.fetchLatestCommitedBlockProgram(
      lucid,
      fetchConfig
    );
    const fetchedBlocksOutRef = UtilsTx.utxoToOutRef(latestBlock);
    if (!UtilsTx.outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
      latestBlockOutRef = fetchedBlocksOutRef;
      logInfo("monitorStateQueue: Committing a new block...");
      yield* StateQueueTx.buildAndSubmitCommitmentBlock(
        lucid,
        pool,
        fetchConfig,
        Date.now()
      );
    }
  });

parentPort?.on("message", (_) => {
  console.log("Starting Worker monitorStateQueue");

  const workerProgram = Effect.gen(function* () {
    const nodeConfig = yield* makeConfig;
    const { user } = yield* makeUserFn(nodeConfig);
    const { spendScriptAddress, policyId } =
      yield* makeAlwaysSucceedsServiceFn(nodeConfig);
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };

    const pool = new pg.Pool({
      host: "postgres",
      user: nodeConfig.POSTGRES_USER,
      password: nodeConfig.POSTGRES_PASSWORD,
      database: nodeConfig.POSTGRES_DB,
      max: 20,
      idleTimeoutMillis: 30000,
      connectionTimeoutMillis: 2000,
    });

    const db = yield* Effect.tryPromise({
      try: () => UtilsDB.initializeDb(pool),
      catch: (e) => new Error(`${e}`),
    });

    const policy = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(nodeConfig.POLLING_INTERVAL)
    );
    const action = monitorStateQueue(user, fetchConfig, db).pipe(
      Effect.tap((metrics) => {
        parentPort?.postMessage({ type: "metrics", data: metrics });
      }),
      Effect.catchAll((error) => {
        Effect.log("monitorStateQueue: error occured", error);
        return Effect.void;
      })
    );
    yield* Effect.repeat(action, policy);
  });
  Effect.runPromiseExit(workerProgram);
});
