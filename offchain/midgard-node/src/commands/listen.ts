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
import express from "express";
import sqlite3 from "sqlite3";
import * as MempoolDB from "../database/mempool.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import * as BlocksDB from "../database/blocks.js";
import * as ImmutableDB from "../database/immutable.js";
import { Duration, Effect, Option, Schedule } from "effect";
import { modifyMultipleTables } from "@/database/utils.js";
import { User, NodeConfig } from "@/config.js";
import { initializeDb } from "@/database.js";
import { outRefsAreEqual, utxoToOutRef } from "@/transactions/utils.js";

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

// TODO: Placehoder, must be imported from SDK.
const fetchConfirmedState = (
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

const readEndTimeOfConfirmedState = (
  utxo: UTxO,
): Effect.Effect<number, string, never> => {
  return utxo.datum
    ? Effect.succeed(0) // Replace 0 with the actual extraction logic
    : Effect.fail("Missing datum of the confirmed state.");
};

export const listen = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  port: number,
): Effect.Effect<void, never, never> =>
  Effect.sync(() => {
    const app = express();

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
          await modifyMultipleTables(
            db,
            [MempoolDB.insert, tx.toHash(), txCBOR],
            [MempoolLedgerDB.clearUTxOs, spent],
            [MempoolLedgerDB.insert, produced],
          );
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
  const db = yield* Effect.tryPromise(() =>
    initializeDb(nodeConfig.DATABASE_PATH),
  );

  yield* Effect.all([
    listen(user, db, nodeConfig.PORT),
    monitorStateQueue(user, db, nodeConfig.POLLING_INTERVAL),
    monitorConfirmedState(user, nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL),
  ]);
});
