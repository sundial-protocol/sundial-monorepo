// Build a tx Merkle root with all the mempool txs

import { makeConfig } from "@/config.js";
import { ImmutableDB, LatestLedgerDB, MempoolDB } from "@/database/index.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import { UtilsTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import pg from "pg";
import { handleSignSubmit } from "../utils.js";

const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear included transactions from MempoolDB, and inject them into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: pg.Pool,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  endTime: number,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");
    const mempoolTxs = yield* Effect.tryPromise(() => MempoolDB.retrieve(db)).pipe(
      Effect.withSpan("retrieve mempool transaction"),
    );
    const mempoolTxsCount = BigInt(mempoolTxs.length);

    if (mempoolTxsCount > 0n) {
      yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

      const mempoolTxCbors = mempoolTxs.map((tx) => tx.txCbor);

      yield* Effect.logInfo("🔹 Building MPT root of transactions...");
      const txRoot = yield* SDK.Utils.mptFromList(
        mempoolTxCbors
      ).pipe(Effect.withSpan("build MPT tx root"));
      yield* Effect.logInfo(`🔹 Mempool tx root found: ${txRoot}`);

      const { spent: spentList, produced: producedList } =
        yield* findAllSpentAndProducedUTxOs(
          mempoolTxCbors
        ).pipe(Effect.withSpan("findAllSpentAndProducedUTxOs"));

      const latestLedgerUTxOs = yield* Effect.tryPromise(() =>
        LatestLedgerDB.retrieve(db),
      ).pipe(Effect.withSpan("retrieve latest ledger utxo list"));

      // Remove spent UTxOs from latestLedgerUTxOs
      const filteredUTxOList = latestLedgerUTxOs.filter(
        (utxo) =>
          !spentList.some(
            (spent) => UtilsTx.outRefsAreEqual(utxo, spent),
          ),
      );

      // Merge filtered latestLedgerUTxOs with producedList
      const newLatestLedger = [...filteredUTxOList, ...producedList].map(
        (utxo) => utxo.txHash + utxo.outputIndex,
      );

      yield* Effect.logInfo("🔹 Building MPT root of UTxO set after applying MempoolDB to LatestLedgerDB...");
      const utxoRoot = yield* SDK.Utils.mptFromList(newLatestLedger);
      yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

      const nodeConfig = yield* makeConfig;

      const { policyId, spendScript, mintScript } =
        yield* makeAlwaysSucceedsServiceFn(nodeConfig);

      // Build commitment block
      const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
        newUTxOsRoot: utxoRoot.hash.toString("hex"),
        transactionsRoot: txRoot.hash.toString("hex"),
        endTime: BigInt(endTime),
        stateQueueSpendingScript: spendScript,
        policyId,
        stateQueueMintingScript: mintScript,
      };

      const aoUpdateCommitmentTimeParams = {};

      yield* Effect.logInfo("🔹 Building block commitment transaction...");
      const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
        lucid,
        fetchConfig,
        commitBlockParams,
        aoUpdateCommitmentTimeParams,
      );

      const txSize = txBuilder.toCBOR().length / 2;
      yield* Effect.logInfo(`🔹 Transaction built successfully. Size: ${txSize}`);

      // Using sign and submit helper with confirmation so that databases are
      // only updated after a successful on-chain registration of the block.
      yield* handleSignSubmit(lucid, txBuilder).pipe(
        Effect.withSpan("handleSignSubmit-commit-block"),
      );

      const totalTxSize = mempoolTxCbors.reduce(
        (acc, cbor) => acc + cbor.length / 2,
        0,
      );

      yield* commitBlockTxSizeGauge(Effect.succeed(txSize));
      yield* commitBlockNumTxGauge(Effect.succeed(mempoolTxsCount));
      yield* Metric.increment(commitBlockCounter);
      yield* Metric.incrementBy(commitBlockTxCounter, mempoolTxsCount);
      yield* totalTxSizeGauge(Effect.succeed(totalTxSize));

      const batchSize = 100;

      yield* Effect.logInfo("🔹 Clearing spennt UTxOs from LatestLedgerDB...");
      for (let i = 0; i < spentList.length; i += batchSize) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.clearUTxOs(db, spentList.slice(i, i + batchSize)),
        ).pipe(Effect.withSpan(`latest-ledger-clearUTxOs-${i}`));
      }

      yield* Effect.logInfo("🔹 Inserting produced UTxOs into LatestLedgerDB...");
      for (let i = 0; i < producedList.length; i += batchSize) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.insert(db, producedList.slice(i, i + batchSize)),
        ).pipe(Effect.withSpan(`latest-ledger-insert-${i}`));
      }

      yield* Effect.logInfo("🔹 Inserting included transactions into ImmutableDB...");
      for (let i = 0; i < mempoolTxsCount; i += batchSize) {
        yield* Effect.tryPromise(() =>
          ImmutableDB.insertTxs(db, mempoolTxs.slice(i, i + batchSize))
        ).pipe(Effect.withSpan(`immutable-db-insert-${i}`));
      }

      yield* Effect.logInfo("🔹 Clearing included transactions from MempoolDB...");
      yield* Effect.tryPromise(
        () => MempoolDB.clearTxs(db, mempoolTxs.map((tx) => tx.txHash))
      ).pipe(
        Effect.withSpan("clear mempool"),
      );

      yield* Effect.logInfo("🔹 Block submission terminated successfully.");
    } else {
      yield* Effect.logInfo("🔹 No transactions were found in MempoolDB.");
    }
  });
