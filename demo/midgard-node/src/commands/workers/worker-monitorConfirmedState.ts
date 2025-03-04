import { Duration, Effect, Schedule } from "effect";
import { parentPort } from "worker_threads";

import * as SDK from "@al-ft/midgard-sdk";
import { makeConfig, makeUserFn } from "@/config.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { UtilsDB } from "@/database/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import pg from "pg";

parentPort?.on("message", (_) => {
  console.log("Starting Worker monitorConfirmedState");

  const workerProgram = Effect.gen(function* () {
    const nodeConfig = yield* makeConfig;
    const { user: lucid } = yield* makeUserFn(nodeConfig);
    const { spendScriptAddress, policyId, spendScript, mintScript } =
      yield* makeAlwaysSucceedsServiceFn(nodeConfig);
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };

    const pool = new pg.Pool({
      host: "postgres", // service name
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
      Duration.millis(nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL)
    );
    const action = StateQueueTx.buildAndSubmitMergeTx(
      lucid,
      db,
      fetchConfig,
      spendScript,
      mintScript
    ).pipe(
      Effect.tap((metrics) => {
        parentPort?.postMessage({ type: "metrics", data: metrics });
      }),
      Effect.catchAll((error) => {
        Effect.log("monitorConfirmedState: error occured", error);
        return Effect.void;
      })
    );
    yield* Effect.repeat(action, policy);
  });
  Effect.runPromiseExit(workerProgram);
});
