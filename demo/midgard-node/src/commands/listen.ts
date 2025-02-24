import {
  findSpentAndProducedUTxOs,
  isHexString,
  logInfo,
  logWarning,
} from "../utils.js";
import {
  LucidEvolution,
  OutRef,
  UTxO,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import express from "express";
import sqlite3 from "sqlite3";
import * as MempoolDB from "../database/mempool.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import * as LatestLedgerDB from "../database/latestLedger.js";
import * as ConfirmedLedgerDB from "../database/confirmedLedger.js";
import * as BlocksDB from "../database/blocks.js";
import * as ImmutableDB from "../database/immutable.js";
import { Duration, Effect, Option, Schedule, Metric, pipe } from "effect";
import { modifyMultipleTables } from "@/database/utils.js";
import { User, NodeConfig } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { initializeDb } from "@/database.js";
import { outRefsAreEqual, utxoToOutRef } from "@/transactions/utils.js";
import { NodeSdk } from "@effect/opentelemetry";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { stateQueueInit } from "@/transactions/state-queue/init.js";
import { resetStateQueue } from "@/transactions/state-queue/reset.js";
import { logError } from "effect/Effect";

// TODO: Placehoder, must be imported from SDK.
const fetchLatestBlock = (
  _lucid: LucidEvolution,
): Effect.Effect<UTxO, never, never> =>
  Effect.gen(function* () {
    return {
      txHash: "",
      outputIndex: 0,
      address: "",
      assets: {},
    };
  });

export const listen = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  port: number,
): Effect.Effect<void, never, never> =>
  Effect.sync(() => {
    const app = express();
    const txCounter = Metric.counter("tx_count", {
      description: "A counter for tracking transactions",
      bigint: true,
      incremental: true,
    }).pipe(Metric.tagged("environment", lucid.config().network!));
    app.get("/tx", (req, res) => {
      res.type("text/plain");
      const txHash = req.query.tx_hash;
      if (
        typeof txHash === "string" &&
        isHexString(txHash) &&
        txHash.length === 32
      ) {
        MempoolDB.retrieveTxCborByHash(db, txHash).then((ret) => {
          Option.match(ret, {
            onSome: (retrieved) => res.json({ tx: retrieved }),
            onNone: () =>
              ImmutableDB.retrieveTxCborByHash(db, txHash).then((ret) => {
                Option.match(ret, {
                  onSome: (retrieved) => res.json({ tx: retrieved }),
                  onNone: () =>
                    res
                      .status(404)
                      .json({ message: "No matching transactions found" }),
                });
              }),
          });
        });
      } else {
        res
          .status(400)
          .json({ message: `Invalid transaction hash: ${txHash}` });
      }
    });

    app.get("/utxos", (req, res) => {
      res.type("text/plain");
      const addr = req.query.addr;
      if (typeof addr === "string") {
        try {
          const addrDetails = getAddressDetails(addr);
          if (addrDetails.paymentCredential) {
            MempoolLedgerDB.retrieve(db).then((allUTxOs) =>
              res.json({
                utxos: allUTxOs.filter(
                  (a) => a.address === addrDetails.address.bech32,
                ),
              }),
            );
          } else {
            res.status(400).json({ message: `Invalid address: ${addr}` });
          }
        } catch {
          res.status(400).json({ message: `Invalid address: ${addr}` });
        }
      } else {
        res.status(400).json({ message: `Invalid address: ${addr}` });
      }
    });

    app.get("/block", (req, res) => {
      res.type("text/plain");
      const hdrHash = req.query.header_hash;
      if (
        typeof hdrHash === "string" &&
        isHexString(hdrHash) &&
        hdrHash.length === 32
      ) {
        BlocksDB.retrieveTxHashesByBlockHash(db, hdrHash).then((hashes) =>
          res.json({ hashes }),
        );
      } else {
        res
          .status(400)
          .json({ message: `Invalid block header hash: ${hdrHash}` });
      }
    });

    app.get("/init", async (_req, res) => {
      try {
        const program = pipe(
          stateQueueInit,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        const txHash = await Effect.runPromise(program);
        res.json({ message: `Initiation successful: ${txHash}` });
      } catch (e) {
        logError("Error during initialization:", e);

        res.status(500).json({
          message: "Initiation failed",
          error: e instanceof Error ? e.message : `Unknown error: ${e}`,
        });
      }
    });

    app.get("/reset", async (_req, res) => {
      res.type("text/plain");
      try {
        const program = pipe(
          resetStateQueue,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        await Effect.runPromise(program);
        res.json({ message: "Collected all UTxOs successfully!" });
      } catch (_e) {
        res
          .status(400)
          .json({
            message: "Failed to collect one or more UTxOs. Please try again.",
          });
      }
      try {
        await Promise.all([
          MempoolDB.clear(db),
          MempoolLedgerDB.clear(db),
          BlocksDB.clear(db),
          ImmutableDB.clear(db),
          LatestLedgerDB.clear(db),
          ConfirmedLedgerDB.clear(db),
        ]);
        res.json({ message: "Cleared all tables successfully!" });
      } catch (_e) {
        res
          .status(400)
          .json({
            message: "Failed to clear one or more tables. Please try again.",
          });
      }
    });

    app.post("/submit", async (req, res) => {
      res.type("text/plain");
      const txCBOR = req.query.tx_cbor;
      if (typeof txCBOR === "string" && isHexString(txCBOR)) {
        try {
          const tx = lucid.fromTx(txCBOR);
          const spentAndProducedProgram = findSpentAndProducedUTxOs(txCBOR);
          const { spent, produced } = await Effect.runPromise(
            spentAndProducedProgram,
          );
          // TODO: Avoid abstraction, dedicate a SQL command.
          await modifyMultipleTables(
            db,
            [MempoolDB.insert, tx.toHash(), txCBOR],
            [MempoolLedgerDB.clearUTxOs, spent],
            [MempoolLedgerDB.insert, produced],
          );
          Effect.runSync(Metric.increment(txCounter));
          res.json({ message: "Successfully submitted the transaction" });
        } catch (e) {
          res.status(400).json({ message: "Something went wrong" });
        }
      } else {
        res.status(400).json({ message: "Invalid CBOR provided" });
      }
    });

    app.listen(port, () =>
      logInfo(`Server running at http://localhost:${port}`),
    );
  });

const monitorStateQueue = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  pollingInterval: number,
) =>
  Effect.gen(function* () {
    let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };
    const monitor = Effect.gen(function* () {
      logInfo("monitoring state query...");
      const latestBlock = yield* fetchLatestBlock(lucid);
      const fetchedBlocksOutRef = utxoToOutRef(latestBlock);
      if (!outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
        latestBlockOutRef = fetchedBlocksOutRef;
        yield* submitBlock(lucid, latestBlock);
      }
    });
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(pollingInterval),
    );
    yield* Effect.repeat(monitor, schedule);
  });

export const storeTx = async (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  tx: string,
) =>
  Effect.gen(function* () {
    const txHash = lucid.fromTx(tx).toHash();
    yield* Effect.tryPromise(() => MempoolDB.insert(db, txHash, tx));
  });

const submitBlock = (_lucid: LucidEvolution, latestBlock: UTxO) =>
  Effect.gen(function* () {
    logWarning("submitBlock: TODO");
  });

const monitorConfirmedState = (
  _lucid: LucidEvolution,
  pollingInterval: number,
) =>
  Effect.gen(function* () {
    const monitor = Effect.gen(function* () {
      logWarning("monitorConfirmedState: TODO");
    });
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(pollingInterval),
    );
    yield* Effect.repeat(monitor, schedule);
  });

export const runNode = Effect.gen(function* () {
  const { user } = yield* User;
  const nodeConfig = yield* NodeConfig;
  const { spendScriptAddress, mintScript, policyId } =
    yield* AlwaysSucceedsContract;
  const initParams: SDK.Types.InitParams = {
    address: spendScriptAddress,
    policyId: policyId,
    stateQueueMintingScript: mintScript,
  };
  const db = yield* Effect.tryPromise(() =>
    initializeDb(nodeConfig.DATABASE_PATH),
  );
  const MetricsLive = NodeSdk.layer(() => ({
    resource: { serviceName: "midgard-node" },
    spanProcessor: new BatchSpanProcessor(
      new OTLPTraceExporter({
        url: `http://localhost:${nodeConfig.OTLP_PORT}/v1/traces`,
      }),
    ),
    metricReader: new PrometheusExporter({
      port: nodeConfig.PROM_METRICS_PORT,
    }),
  }));

  yield* Effect.all([
    listen(user, db, nodeConfig.PORT),
    monitorStateQueue(user, db, nodeConfig.POLLING_INTERVAL),
    monitorConfirmedState(user, nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL),
  ]).pipe(
    Effect.withSpan("midgard-node"),
    Effect.tap(() => Effect.annotateCurrentSpan("migdard-node", "runner")),
    Effect.provide(MetricsLive),
  );
});
