import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  deserializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import {
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  ProcessedMempoolDB,
} from "@/database/index.js";
import {
  handleSignSubmitNoConfirmation,
  SubmitError,
} from "@/transactions/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import {
  deleteMempoolMpt,
  makeMpts,
  processMpts,
  withTrieTransaction,
} from "./utils/mpt.js";
import { NodeConfig, User } from "@/config.js";
import { Database } from "@/services/database.js";
import { batchProgram } from "@/utils.js";

const BATCH_SIZE = 100;

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig | User | Database> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { user: lucid } = yield* User;

    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const mempoolTxs = yield* MempoolDB.retrieve();
    const endTime = Date.now();
    const mempoolTxsCount = mempoolTxs.length;

    if (mempoolTxsCount === 0) {
      yield* Effect.logInfo("🔹 No transactions were found in MempoolDB");
      return {
        type: "EmptyMempoolOutput",
      };
    }

    yield* Effect.logInfo(`🔹 ${mempoolTxsCount} retrieved.`);

    const { ledgerTrie, mempoolTrie } = yield* makeMpts();

    return yield* withTrieTransaction(
      ledgerTrie,
      Effect.gen(function* () {
        const { utxoRoot, txRoot, mempoolTxHashes, sizeOfBlocksTxs } =
          yield* processMpts(ledgerTrie, mempoolTrie, mempoolTxs);
        const { policyId, spendScript, spendScriptAddress, mintScript } =
          yield* makeAlwaysSucceedsServiceFn(nodeConfig);

        // yield* Effect.forEach(
        //   batchIndices,
        //   (startIndex) => {

        if (workerInput.data.availableConfirmedBlock === "") {
          // The tx confirmation worker has not yet confirmed a previously
          // submitted tx, so the root we have found can not be used yet.
          // However, it is stored on disk in our LevelDB mempool. Therefore,
          // the processed txs must be transferred to `ProccessedMempoolDB` from
          // `MempoolDB`.
          //
          // TODO: Handle failures properly.
          yield* batchProgram(
            BATCH_SIZE,
            mempoolTxsCount,
            "skipped-submission-db-transfer",
            (startIndex: number, endIndex: number) => {
              const batchTxs = mempoolTxs.slice(startIndex, endIndex);
              const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
              return Effect.all(
                [
                  ProcessedMempoolDB.insertTxs(batchTxs).pipe(
                    Effect.withSpan(
                      `processed-mempool-db-insert-${startIndex}`,
                    ),
                  ),
                  MempoolDB.clearTxs(batchHashes).pipe(
                    Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
                  ),
                ],
                { concurrency: "unbounded" },
              );
            },
          );
          return {
            type: "SkippedSubmissionOutput",
            mempoolTxsCount,
          };
        }

        // const retryPolicy = Schedule.exponential("100 millis").pipe(
        //   Schedule.compose(Schedule.recurs(4)),
        // );
        // yield* Effect.logInfo("🔹 Fetching latest commited block...");
        yield* Effect.logInfo(
          "🔹 Previous submitted block is now confirmed, deserializing...",
        );
        const latestBlock = yield* deserializeStateQueueUTxO(
          workerInput.data.availableConfirmedBlock,
        );
        //  yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
        //    lucid,
        //    fetchConfig,
        //  ).pipe(
        //    Effect.retry(retryPolicy),
        //    Effect.withSpan("fetchLatestCommittedBlockProgram"),
        //  );
        yield* Effect.logInfo(
          "🔹 Finding updated block datum and new header...",
        );
        const { nodeDatum: updatedNodeDatum, header: newHeader } =
          yield* SDK.Utils.updateLatestBlocksDatumAndGetTheNewHeader(
            lucid,
            latestBlock.datum,
            utxoRoot,
            txRoot,
            BigInt(endTime),
          );

        const newHeaderHash = yield* SDK.Utils.hashHeader(newHeader);
        yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

        // Build commitment block
        const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
          anchorUTxO: latestBlock,
          updatedAnchorDatum: updatedNodeDatum,
          newHeader: newHeader,
          stateQueueSpendingScript: spendScript,
          policyId,
          stateQueueMintingScript: mintScript,
        };

        const aoUpdateCommitmentTimeParams = {};

        yield* Effect.logInfo("🔹 Building block commitment transaction...");
        const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
          stateQueueAddress: spendScriptAddress,
          stateQueuePolicyId: policyId,
        };
        lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE);
        const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
          lucid,
          fetchConfig,
          commitBlockParams,
          aoUpdateCommitmentTimeParams,
        );
        const txSize = txBuilder.toCBOR().length / 2;
        yield* Effect.logInfo(
          `🔹 Transaction built successfully. Size: ${txSize}`,
        );

        let flushMempoolTrie: boolean = true;

        // Using sign and submit helper with confirmation so that databases are
        // only updated after a successful on-chain registration of the block.
        const onSubmitFailure = (_err: SubmitError) =>
          Effect.gen(function* () {
            yield* Effect.logError(
              "🔹 ⚠️  Tx submit failed. Mempool trie will be preserved, but db will be cleared.",
            );
            yield* Effect.logInfo("🔹 Mempool Trie stats:");
            console.dir(mempoolTrie.database()._stats, { depth: null });
            flushMempoolTrie = false;
            // yield* Effect.fail(err.err);
          });

        yield* handleSignSubmitNoConfirmation(
          lucid,
          txBuilder,
          onSubmitFailure,
        ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

        const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

        yield* Effect.logInfo(
          "🔹 Inserting included transactions into ImmutableDB and BlocksDB...",
        );
        yield* Effect.logInfo(
          "🔹 Clearing included transactions from MempoolDB...",
        );

        yield* batchProgram(
          BATCH_SIZE,
          mempoolTxsCount,
          "successful-commit",
          (startIndex: number, endIndex: number) => {
            const batchTxs = mempoolTxs.slice(startIndex, endIndex);
            const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
            return Effect.all(
              [
                ImmutableDB.insertTxs(batchTxs).pipe(
                  Effect.withSpan(`immutable-db-insert-${startIndex}`),
                ),
                BlocksDB.insert(newHeaderHashBuffer, batchHashes).pipe(
                  Effect.withSpan(`blocks-db-insert-${startIndex}`),
                ),
                MempoolDB.clearTxs(batchHashes).pipe(
                  Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
                ),
              ],
              { concurrency: "unbounded" },
            );
          },
        );

        if (flushMempoolTrie) {
          yield* Effect.logInfo("🔹 Wiping mempool trie...");
          yield* deleteMempoolMpt;
        }

        const output: WorkerOutput = {
          txSize,
          mempoolTxsCount,
          sizeOfBlocksTxs,
        };
        return output;
      }),
    );
  });

if (parentPort === null)
  throw new Error("MPT computation must be run as a worker");

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(Database.layer),
  Effect.provide(User.layer),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAll((e) =>
      Effect.succeed({
        type: "FailureOutput",
        error: e instanceof Error ? e.message : "Unknown error from MPT worker",
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(`👷 Work completed (${JSON.stringify(output)}).`),
  );
  parentPort?.postMessage(output);
});
