import { Effect, Metric } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, fromHex } from "@lucid-evolution/lucid";
import * as ETH_UTILS from "@ethereumjs/util";
import { MidgardMpt, MptError } from "./mpt.js";
import {
  DepositsDB,
  Tx,
  UserEvents,
  Ledger,
  WithdrawalsDB,
  BlocksDB,
  MempoolLedgerDB,
} from "@/database/index.js";
import {
  DatabaseError,
  deserializeUTxOsFromStorage,
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { TxSignError } from "@/transactions/utils.js";
import { breakDownTx, ProcessedTx } from "@/utils.js";

export const CommitmentWorkerMessageType = {
  RunCommitment: "RunCommitment",
  RunCommitmentResult: "RunCommitmentResult",
} as const;

export type WorkerInput = {
  type: typeof CommitmentWorkerMessageType.RunCommitment;
};

export type SuccessfulCommitmentOutput = {
  type: "SuccessfulCommitmentOutput";
  stats: BlocksDB.Stats;
};

export type FailureOutput = {
  type: "FailureOutput";
  error: string;
};

export type SeededOutput = {
  type: "SeededOutput";
};

export type WorkerOutput =
  | SuccessfulCommitmentOutput
  | FailureOutput
  | SeededOutput;

export type WorkerMessage = {
  type: typeof CommitmentWorkerMessageType.RunCommitmentResult;
  output: WorkerOutput;
};

const COMMITMENT_MPT_CHUNK_SIZE = 1000;

const commitmentMptChunksAppliedCounter = Metric.counter(
  "commitment_mpt_chunks_applied_total",
  {
    description:
      "Total number of bounded MPT operation chunks applied during block commitment",
    bigint: true,
    incremental: true,
  },
).register();

const commitmentMptChunkEntriesCounter = Metric.counter(
  "commitment_mpt_chunk_entries_total",
  {
    description:
      "Total number of entries processed across bounded MPT chunks during block commitment",
    bigint: true,
    incremental: true,
  },
).register();

const commitmentMptChunkOpsCounter = Metric.counter(
  "commitment_mpt_chunk_ops_total",
  {
    description:
      "Total number of trie batch operations applied across bounded MPT chunks during block commitment",
    bigint: true,
    incremental: true,
  },
).register();

const getTotalChunks = (totalEntries: number): number =>
  Math.ceil(totalEntries / COMMITMENT_MPT_CHUNK_SIZE);

const logMptChunkProgress = (
  stage: string,
  chunkIndex: number,
  chunkCount: number,
  chunkEntriesCount: number,
  processedEntriesCount: number,
  totalEntriesCount: number,
  chunkOpsCount: number,
) =>
  Effect.gen(function* () {
    yield* Metric.increment(commitmentMptChunksAppliedCounter);
    yield* Metric.incrementBy(
      commitmentMptChunkEntriesCounter,
      BigInt(chunkEntriesCount),
    );
    yield* Metric.incrementBy(
      commitmentMptChunkOpsCounter,
      BigInt(chunkOpsCount),
    );
    yield* Effect.logInfo(
      `commitment-mpt chunk stage=${stage} chunk=${chunkIndex + 1}/${chunkCount} entries=${chunkEntriesCount} processed=${processedEntriesCount}/${totalEntriesCount} ops=${chunkOpsCount}`,
    );
  });

const txEntryToBatchDBOps = (
  txCbor: Buffer,
): Effect.Effect<
  {
    spent: Buffer[];
    produced: Ledger.Entry[];
    delOps: ETH_UTILS.BatchDBOp[];
    putOps: ETH_UTILS.BatchDBOp[];
  },
  SDK.CmlDeserializationError
> =>
  Effect.gen(function* () {
    const { spent, produced } = yield* breakDownTx(txCbor).pipe(
      Effect.withSpan("breakDownTx"),
    );
    const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef: Buffer) => ({
      type: "del",
      key: outRef,
    }));
    const putOps: ETH_UTILS.BatchDBOp[] = produced.map((le: Ledger.Entry) => ({
      type: "put",
      key: le[Ledger.Columns.OUTREF],
      value: le[Ledger.Columns.OUTPUT],
    }));
    return {
      spent,
      produced,
      delOps,
      putOps,
    };
  });

export const applyWithdrawalsToLedger = (
  ledgerTrie: MidgardMpt,
  withdrawalEntries: readonly UserEvents.Entry[],
): Effect.Effect<
  {
    withdrawnOutRefs: Buffer[];
    withdrawalsRoot: string;
    sizeOfWithdrawals: number;
  },
  SDK.CmlDeserializationError | SDK.DataCoercionError | MptError
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${withdrawalEntries.length} withdrawal(s) to the ledgerTrie`,
    );

    const withdrawalsTrie: MidgardMpt = yield* MidgardMpt.create("withdrawals");
    const withdrawnOutRefs: Buffer[] = [];
    let sizeOfWithdrawals = 0;
    const chunkCount = getTotalChunks(withdrawalEntries.length);
    let processedEntriesCount = 0;

    for (
      let startIndex = 0;
      startIndex < withdrawalEntries.length;
      startIndex += COMMITMENT_MPT_CHUNK_SIZE
    ) {
      const endIndex = startIndex + COMMITMENT_MPT_CHUNK_SIZE;
      const chunkIndex = Math.floor(startIndex / COMMITMENT_MPT_CHUNK_SIZE);
      const withdrawalsChunk = withdrawalEntries.slice(startIndex, endIndex);
      const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
      const withdrawalsBatchOps: ETH_UTILS.BatchDBOp[] = [];

      yield* Effect.forEach(withdrawalsChunk, (withdrawalEntry) =>
        Effect.gen(function* () {
          const withdrawalInfo = withdrawalEntry[UserEvents.Columns.INFO];
          const spentOutRef =
            yield* WithdrawalsDB.entryToOutRef(withdrawalEntry);
          ledgerBatchOps.push({
            type: "del",
            key: spentOutRef,
          });
          withdrawalsBatchOps.push({
            type: "put",
            key: withdrawalEntry[UserEvents.Columns.ID],
            value: withdrawalInfo,
          });
          sizeOfWithdrawals += withdrawalInfo.length;
          withdrawnOutRefs.push(spentOutRef);
        }),
      );

      yield* Effect.all(
        [
          ledgerTrie.batch(ledgerBatchOps),
          withdrawalsTrie.batch(withdrawalsBatchOps),
        ],
        { concurrency: "unbounded" },
      );
      processedEntriesCount += withdrawalsChunk.length;
      yield* logMptChunkProgress(
        "withdrawals",
        chunkIndex,
        chunkCount,
        withdrawalsChunk.length,
        processedEntriesCount,
        withdrawalEntries.length,
        ledgerBatchOps.length + withdrawalsBatchOps.length,
      );
    }

    const withdrawalsRoot = yield* withdrawalsTrie.getRootHex();

    return {
      withdrawnOutRefs,
      withdrawalsRoot,
      sizeOfWithdrawals,
    };
  });

export const applyTxOrdersToLedger = (
  ledgerTrie: MidgardMpt,
  txOrders: readonly UserEvents.Entry[],
  concurrency: number,
): Effect.Effect<
  {
    txOrdersCount: number;
    spentByTxOrders: Buffer[];
    producedByTxOrders: Ledger.Entry[];
    txsTrie: MidgardMpt;
    sizeOfTxOrders: number;
  },
  SDK.CmlDeserializationError | MptError | DatabaseError,
  never
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${txOrders.length} tx order(s) to the ledgerTrie`,
    );

    let sizeOfTxOrders = 0;
    const spentByTxOrders: Buffer[] = [];
    const producedByTxOrders: Ledger.Entry[] = [];
    const txsTrie: MidgardMpt = yield* MidgardMpt.create("txs");
    const chunkCount = getTotalChunks(txOrders.length);
    let processedEntriesCount = 0;

    for (
      let startIndex = 0;
      startIndex < txOrders.length;
      startIndex += COMMITMENT_MPT_CHUNK_SIZE
    ) {
      const endIndex = startIndex + COMMITMENT_MPT_CHUNK_SIZE;
      const chunkIndex = Math.floor(startIndex / COMMITMENT_MPT_CHUNK_SIZE);
      const txOrdersChunk = txOrders.slice(startIndex, endIndex);
      const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];

      yield* Effect.forEach(
        txOrdersChunk,
        (txOrder) =>
          Effect.gen(function* () {
            const txCbor = txOrder[UserEvents.Columns.INFO];
            const { delOps, putOps, spent, produced } =
              yield* txEntryToBatchDBOps(txCbor);
            sizeOfTxOrders += txCbor.length;
            ledgerBatchOps.push(...delOps);
            ledgerBatchOps.push(...putOps);
            spentByTxOrders.push(...spent);
            producedByTxOrders.push(...produced);
          }),
        { concurrency },
      );

      yield* ledgerTrie.batch(ledgerBatchOps);
      processedEntriesCount += txOrdersChunk.length;
      yield* logMptChunkProgress(
        "tx_orders",
        chunkIndex,
        chunkCount,
        txOrdersChunk.length,
        processedEntriesCount,
        txOrders.length,
        ledgerBatchOps.length,
      );
    }

    return {
      txOrdersCount: txOrders.length,
      spentByTxOrders,
      producedByTxOrders,
      txsTrie,
      sizeOfTxOrders,
    };
  });

export const applyTxRequestsToLedger = (
  ledgerTrie: MidgardMpt,
  txsTrie: MidgardMpt,
  mempoolTxs: readonly (ProcessedTx | Tx.Entry)[],
  concurrency: number,
): Effect.Effect<
  {
    txRequestsCount: number;
    txsRoot: string;
    sizeOfTxRequests: number;
  },
  SDK.CmlDeserializationError | MptError
> =>
  Effect.gen(function* () {
    let txRequestsCount = 0;
    let sizeOfTxRequests = 0;
    const chunkCount = getTotalChunks(mempoolTxs.length);
    let processedEntriesCount = 0;

    yield* Effect.logInfo(
      `🔹 Going through mempool and processing (${mempoolTxs.length}) transactions...`,
    );

    for (
      let startIndex = 0;
      startIndex < mempoolTxs.length;
      startIndex += COMMITMENT_MPT_CHUNK_SIZE
    ) {
      const endIndex = startIndex + COMMITMENT_MPT_CHUNK_SIZE;
      const chunkIndex = Math.floor(startIndex / COMMITMENT_MPT_CHUNK_SIZE);
      const mempoolTxChunk = mempoolTxs.slice(startIndex, endIndex);
      const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
      const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];

      yield* Effect.forEach(
        mempoolTxChunk,
        (entry) =>
          Effect.gen(function* () {
            const processedTx =
              "spent" in entry && "produced" in entry
                ? entry
                : yield* breakDownTx(entry[Tx.Columns.TX]);
            const txHash = processedTx.txId;
            const txCbor = processedTx.txCbor;
            const delOps: ETH_UTILS.BatchDBOp[] = processedTx.spent.map(
              (outRef) => ({
                type: "del",
                key: outRef,
              }),
            );
            const putOps: ETH_UTILS.BatchDBOp[] = processedTx.produced.map(
              (ledgerEntry) => ({
                type: "put",
                key: ledgerEntry[Ledger.Columns.OUTREF],
                value: ledgerEntry[Ledger.Columns.OUTPUT],
              }),
            );
            txRequestsCount += 1;
            sizeOfTxRequests += txCbor.length;
            mempoolBatchOps.push({
              type: "put",
              key: txHash,
              value: txCbor,
            });
            ledgerBatchOps.push(...delOps);
            ledgerBatchOps.push(...putOps);
          }),
        { concurrency },
      );

      yield* Effect.all(
        [txsTrie.batch(mempoolBatchOps), ledgerTrie.batch(ledgerBatchOps)],
        { concurrency: "unbounded" },
      );

      processedEntriesCount += mempoolTxChunk.length;
      yield* logMptChunkProgress(
        "tx_requests",
        chunkIndex,
        chunkCount,
        mempoolTxChunk.length,
        processedEntriesCount,
        mempoolTxs.length,
        mempoolBatchOps.length + ledgerBatchOps.length,
      );
    }

    const txsRoot = yield* txsTrie.getRootHex();

    return {
      txRequestsCount,
      txsRoot,
      sizeOfTxRequests,
    };
  });

export const applyDepositsToLedger = (
  ledgerTrie: MidgardMpt,
  deposits: readonly UserEvents.Entry[],
): Effect.Effect<
  {
    depositLedgerEntries: Ledger.Entry[];
    depositsRoot: string;
    sizeOfDeposits: number;
  },
  MptError | SDK.CmlDeserializationError,
  NodeConfig | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${deposits.length} deposit(s) to the ledgerTrie`,
    );
    const depositLedgerEntries: Ledger.Entry[] = [];
    const depositsTrie: MidgardMpt = yield* MidgardMpt.create("deposits");
    let sizeOfDeposits = 0;
    const chunkCount = getTotalChunks(deposits.length);
    let processedEntriesCount = 0;

    for (
      let startIndex = 0;
      startIndex < deposits.length;
      startIndex += COMMITMENT_MPT_CHUNK_SIZE
    ) {
      const endIndex = startIndex + COMMITMENT_MPT_CHUNK_SIZE;
      const chunkIndex = Math.floor(startIndex / COMMITMENT_MPT_CHUNK_SIZE);
      const depositsChunk = deposits.slice(startIndex, endIndex);
      const depositsBatchOps: ETH_UTILS.BatchDBOp[] = [];
      const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];

      yield* Effect.forEach(depositsChunk, (depositEntry) =>
        Effect.gen(function* () {
          const ledgerEntry = yield* DepositsDB.entryToLedgerEntry(depositEntry);
          depositLedgerEntries.push(ledgerEntry);
          sizeOfDeposits += depositEntry[UserEvents.Columns.INFO].length;
          ledgerBatchOps.push({
            type: "put",
            key: ledgerEntry[Ledger.Columns.OUTREF],
            value: ledgerEntry[Ledger.Columns.OUTPUT],
          });
          depositsBatchOps.push({
            type: "put",
            key: depositEntry[UserEvents.Columns.ID],
            value: depositEntry[UserEvents.Columns.INFO],
          });
        }),
      );

      yield* Effect.all(
        [ledgerTrie.batch(ledgerBatchOps), depositsTrie.batch(depositsBatchOps)],
        { concurrency: "unbounded" },
      );

      processedEntriesCount += depositsChunk.length;
      yield* logMptChunkProgress(
        "deposits",
        chunkIndex,
        chunkCount,
        depositsChunk.length,
        processedEntriesCount,
        deposits.length,
        ledgerBatchOps.length + depositsBatchOps.length,
      );
    }

    const depositsRoot = yield* depositsTrie.getRootHex();

    return {
      depositLedgerEntries,
      depositsRoot,
      sizeOfDeposits,
    };
  });

export const applyBlockCommitmentLedgerProjection = (
  depositLedgerEntries: readonly Ledger.Entry[],
  producedByTxOrders: readonly Ledger.Entry[],
  withdrawnOutRefs: readonly Buffer[],
  spentByTxOrders: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    // Keep inserts homogeneous by key set: deposits include time_stamp_tz,
    // tx-order-produced entries do not.
    yield* MempoolLedgerDB.insert(depositLedgerEntries);
    yield* MempoolLedgerDB.insert(producedByTxOrders);
    yield* MempoolLedgerDB.clearUTxOs([
      ...withdrawnOutRefs,
      ...spentByTxOrders,
    ]);
  });

const prepareLucidForBlockCommitment = (
  entry: BlocksDB.Entry,
): Effect.Effect<
  {
    lucidPreparation: Effect.Effect<LucidEvolution>;
    appendedUTxO: SDK.StateQueueUTxO;
  },
  SDK.CborDeserializationError | SDK.CmlUnexpectedError | SDK.StateQueueError,
  AlwaysSucceedsContract | Lucid
> =>
  Effect.gen(function* () {
    const newWalletUTxOs = yield* deserializeUTxOsFromStorage(
      entry[BlocksDB.Columns.NEW_WALLET_UTXOS],
    );
    const appendedUTxO =
      yield* BlocksDB.getAppendedStateQueueUTxOFromEntry(entry);
    const lucid = yield* Lucid;
    const lucidPreparation = Effect.gen(function* () {
      yield* lucid.switchToOperatorsBlockCommitmentWallet;
      yield* Effect.sync(() => lucid.api.overrideUTxOs(newWalletUTxOs));
      return lucid.api;
    });
    return {
      lucidPreparation,
      appendedUTxO,
    };
  });

export const buildNewBlockEntry = (
  entry: BlocksDB.Entry,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  endDate: Date,
  stats: BlocksDB.Stats,
): Effect.Effect<
  BlocksDB.EntryNoMeta,
  | SDK.CmlUnexpectedError
  | SDK.CborDeserializationError
  | SDK.CborSerializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxSignError,
  AlwaysSucceedsContract | Lucid
> =>
  Effect.gen(function* () {
    const { lucidPreparation, appendedUTxO } =
      yield* prepareLucidForBlockCommitment(entry);
    const initLucidAPI = yield* lucidPreparation;
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        initLucidAPI,
        appendedUTxO.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        withdrawalsRoot,
        BigInt(endDate.getTime()),
      );
    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);
    const { stateQueue } = yield* AlwaysSucceedsContract;
    const commitBlockParams: SDK.StateQueueCommitBlockParams = {
      anchorUTxO: appendedUTxO,
      updatedAnchorDatum: updatedNodeDatum,
      newHeader: newHeader,
      stateQueueSpendingScript: stateQueue.spendingScript,
      policyId: stateQueue.policyId,
      stateQueueMintingScript: stateQueue.mintingScript,
    };

    yield* Effect.logInfo("🔹 Building block commitment transaction...");
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueue.spendingScriptAddress,
      stateQueuePolicyId: stateQueue.policyId,
    };
    // Rerunning `lucidPreparation` to ensure Lucid API object is in proper state.
    const lucidAPI = yield* lucidPreparation;
    const txBuilder = yield* SDK.incompleteCommitBlockHeaderTxProgram(
      lucidAPI,
      fetchConfig,
      commitBlockParams,
    );
    const [newWalletUTxOs, producedUTxOs, txSignBuilder] = yield* txBuilder
      .chainProgram()
      .pipe(
        Effect.catchAll((_firstErr) =>
          Effect.gen(function* () {
            yield* Effect.logWarning(
              "🔹 chainProgram() failed with stored wallet UTxOs; refreshing wallet from provider and retrying...",
            );
            const operatorWalletAddress = yield* Effect.tryPromise({
              try: () => lucidAPI.wallet().address(),
              catch: (e) =>
                new SDK.LucidError({
                  message:
                    "Failed to resolve block-commitment wallet address for retry",
                  cause: e,
                }),
            });
            const freshUTxOs = yield* Effect.tryPromise({
              // IMPORTANT: Do not use wallet().getUtxos() here. When Lucid
              // has overrideUTxOs set, wallet().getUtxos() can return the
              // overridden snapshot instead of chain state.
              try: () => lucidAPI.utxosAt(operatorWalletAddress),
              catch: (e) =>
                new SDK.LucidError({
                  message:
                    "Failed to fetch fresh wallet UTxOs from provider for retry",
                  cause: e,
                }),
            });
            yield* Effect.logInfo(
              `🔹 Refreshed wallet has ${freshUTxOs.length} UTxO(s); retrying chainProgram()...`,
            );
            yield* Effect.sync(() => lucidAPI.overrideUTxOs(freshUTxOs));
            // Rebuild from scratch: re-calling chainProgram() on the same
            // TxBuilder after a failure causes "Duplicate Mint Asset" because
            // the CML layer processes mintAssets eagerly on the first attempt.
            const freshTxBuilder =
              yield* SDK.incompleteCommitBlockHeaderTxProgram(
                lucidAPI,
                fetchConfig,
                commitBlockParams,
              );
            return yield* freshTxBuilder.chainProgram();
          }),
        ),
        Effect.tapError((e) =>
          Effect.logError(
            `chainProgram() failure detail: ${JSON.stringify(e, Object.getOwnPropertyNames(e))}`,
          ),
        ),
        Effect.mapError(
          (e) =>
            new SDK.LucidError({
              message:
                "Failed to complete (chain method) built block commitment transaction",
              cause: e,
            }),
        ),
      );
    const signedTx = yield* txSignBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: "Failed to sign block commitment transaction",
              cause: e,
              txHash: txSignBuilder.toHash(),
            }),
        ),
      );
    const serializedNewWalletUTxOs =
      yield* serializeUTxOsForStorage(newWalletUTxOs);
    const serializedProducedUTxOs =
      yield* serializeUTxOsForStorage(producedUTxOs);
    const l1CBOR = Buffer.from(signedTx.toTransaction().to_cbor_bytes());
    const newBlockEntry: BlocksDB.EntryNoMeta = {
      ...stats,
      [BlocksDB.Columns.HEADER_HASH]: Buffer.from(fromHex(newHeaderHash)),
      [BlocksDB.Columns.EVENT_START_TIME]:
        entry[BlocksDB.Columns.EVENT_END_TIME],
      [BlocksDB.Columns.EVENT_END_TIME]: endDate,
      [BlocksDB.Columns.NEW_WALLET_UTXOS]: serializedNewWalletUTxOs,
      [BlocksDB.Columns.PRODUCED_UTXOS]: serializedProducedUTxOs,
      [BlocksDB.Columns.L1_CBOR]: l1CBOR,
      [BlocksDB.Columns.STATUS]: BlocksDB.Status.UNSUBMITTED,
    };
    return newBlockEntry;
  });
