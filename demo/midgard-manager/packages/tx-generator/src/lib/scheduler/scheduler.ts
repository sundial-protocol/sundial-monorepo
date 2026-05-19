import { randomBytes } from 'node:crypto';
import { appendFile, mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { UTxO } from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import type { SubmitTransactionResult } from '../client/node-client.js';
import { MidgardNodeClient } from '../client/node-client.js';
import { SerializedMidgardTransaction } from '../client/types.js';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../generators/index.js';
import { LucidPool } from '../generators/lucid-pool.js';
import {
  DEFAULT_CONFIG,
  TRANSACTION_CONSTANTS,
  TransactionGeneratorConfig,
  validateGeneratorConfig,
} from '../types.js';
import {
  GeneratorManifest,
  toEvidenceEntry,
  TransactionEvidenceEntry,
} from './artifact-metadata.js';
import { createSeededRandom, randomHex, randomInt } from './deterministic-random.js';
import {
  createEmptySubmissionAggregate,
  recordAttemptedSubmission,
  recordGeneratedTransactions,
  recordSubmissionObservation,
  REQUEST_EVENTS_SAMPLE_RATE,
  type RequestEventsMode,
  type SubmissionOutcome,
  toSubmissionAggregateWithPercentiles,
} from './submission-evidence.js';
import { inspectGeneratedTransaction } from './transaction-inspector.js';

const GENERATED_TX_PREFIX_ONE_TO_ONE = 'one-to-one';
const GENERATED_TX_PREFIX_MULTI_OUTPUT = 'multi-output';
const GENERATED_TX_PREFIX_REPLAY = 'replay';
const OUTPUT_INDEX_UPPER_EXCLUSIVE = 1001;
const PROJECT_ROOT_RELATIVE_PATH = '../../../..';
const SUBMISSION_AGGREGATE_FILE = 'submission-aggregates.json';
const REQUEST_EVENTS_FILE = 'request-events.jsonl';

// Get the directory path for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, PROJECT_ROOT_RELATIVE_PATH);

/**
 * Generator State Manager
 * Encapsulates all state for the transaction generator
 */
class TxGeneratorState {
  private static instance: TxGeneratorState;

  private _shouldStop = false;
  private _currentPromise: Promise<void> | null = null;
  private _stats = {
    transactionsGenerated: 0,
    transactionsSubmitted: 0,
    transactionsFailed: 0,
    lastError: null as string | null,
    startTime: null as Date | null,
    submissionAggregate: createEmptySubmissionAggregate(),
  };

  private constructor() {}

  static getInstance(): TxGeneratorState {
    if (!TxGeneratorState.instance) {
      TxGeneratorState.instance = new TxGeneratorState();
    }
    return TxGeneratorState.instance;
  }

  get shouldStop(): boolean {
    return this._shouldStop;
  }

  set shouldStop(value: boolean) {
    this._shouldStop = value;
  }

  get currentPromise(): Promise<void> | null {
    return this._currentPromise;
  }

  set currentPromise(value: Promise<void> | null) {
    this._currentPromise = value;
  }

  get stats() {
    return this._stats;
  }

  resetStats() {
    this._stats = {
      transactionsGenerated: 0,
      transactionsSubmitted: 0,
      transactionsFailed: 0,
      lastError: null,
      startTime: new Date(),
      submissionAggregate: createEmptySubmissionAggregate(),
    };
  }

  isRunning(): boolean {
    return !this._shouldStop && this._currentPromise !== null;
  }
}

type GenerationMode = 'generated' | 'replay';

export interface TaskPlan {
  initialUTxO: UTxO;
  useOneToOne: boolean;
}

export interface DeterministicTaskPlanConfig {
  initialUTxO: UTxO;
  batchSize: number;
  transactionType: TransactionGeneratorConfig['transactionType'];
  oneToOneRatio?: number;
  generationSeed: string;
}

export interface DeterministicTaskPlanResult {
  generationSeed: string;
  deterministicStartMs: number;
  taskPlans: TaskPlan[];
}

interface ReplayCorpusFile {
  transactions: SerializedMidgardTransaction[];
}

// Get the shared instance
const state = TxGeneratorState.getInstance();

const buildFilenamePrefix = (useOneToOne: boolean, mode: GenerationMode): string => {
  if (mode === 'replay') {
    return GENERATED_TX_PREFIX_REPLAY;
  }
  return useOneToOne ? GENERATED_TX_PREFIX_ONE_TO_ONE : GENERATED_TX_PREFIX_MULTI_OUTPUT;
};

const getNormalizedSeed = (seed: string | undefined): string => {
  if (seed !== undefined) {
    return seed.trim();
  }
  return randomBytes(16).toString('hex');
};

const getDeterministicStartMs = (seed: string): number => {
  const seedPrefix = seed.slice(0, 12).padEnd(12, '0');
  return Number.parseInt(seedPrefix, 16);
};

const generateUniqueUTxOs = (baseUTxO: UTxO, count: number, random: () => number): UTxO[] =>
  Array.from({ length: count }, () => ({
    ...baseUTxO,
    txHash: randomHex(random, 64).toUpperCase(),
    outputIndex: randomInt(random, OUTPUT_INDEX_UPPER_EXCLUSIVE),
  }));

const buildTaskPlans = ({
  initialUTxO,
  batchSize,
  transactionType,
  oneToOneRatio,
  random,
}: {
  initialUTxO: UTxO;
  batchSize: number;
  transactionType: TransactionGeneratorConfig['transactionType'];
  oneToOneRatio: number;
  random: () => number;
}): TaskPlan[] => {
  const uniqueUTxOs = generateUniqueUTxOs(initialUTxO, batchSize, random);
  return uniqueUTxOs.map((taskUTxO) => ({
    initialUTxO: taskUTxO,
    useOneToOne:
      transactionType === 'one-to-one' ||
      (transactionType === 'mixed' && random() * 100 < oneToOneRatio),
  }));
};

export const createDeterministicTaskPlan = (
  config: DeterministicTaskPlanConfig
): DeterministicTaskPlanResult => {
  const generationSeed = getNormalizedSeed(config.generationSeed);
  const random = createSeededRandom(generationSeed);
  const deterministicStartMs = getDeterministicStartMs(generationSeed);
  const taskPlans = buildTaskPlans({
    initialUTxO: config.initialUTxO,
    batchSize: config.batchSize,
    transactionType: config.transactionType,
    oneToOneRatio: config.oneToOneRatio ?? DEFAULT_CONFIG.oneToOneRatio ?? 70,
    random,
  });

  return {
    generationSeed,
    deterministicStartMs,
    taskPlans,
  };
};

const resolveOutputDir = (outputDir: string): string =>
  isAbsolute(outputDir) ? outputDir : join(projectRoot, outputDir);

const shouldEmitRequestEvent = (mode: RequestEventsMode, sampleRandom: () => number): boolean => {
  if (mode === 'all') {
    return true;
  }
  if (mode === 'sampled') {
    return sampleRandom() < REQUEST_EVENTS_SAMPLE_RATE;
  }
  return false;
};

const submissionOutcomeFromResult = (result: SubmitTransactionResult): SubmissionOutcome => {
  if (result.status === 'SUBMITTED') {
    return 'submitted';
  }
  if (result.status === 'NODE_UNAVAILABLE') {
    return 'node_unavailable';
  }
  if (result.responseClass === 'timed_out') {
    return 'timed_out';
  }
  return 'error';
};

const writeSubmissionAggregates = async (outputDir: string): Promise<void> => {
  const outputPath = resolveOutputDir(outputDir);
  const aggregatePath = join(outputPath, SUBMISSION_AGGREGATE_FILE);
  await writeFile(
    aggregatePath,
    JSON.stringify(toSubmissionAggregateWithPercentiles(state.stats.submissionAggregate), null, 2)
  );
};

const parseReplayCorpusContent = (
  replayCorpusPath: string,
  rawContent: string
): SerializedMidgardTransaction[] => {
  const parsed = JSON.parse(rawContent) as SerializedMidgardTransaction[] | ReplayCorpusFile;
  const transactions = Array.isArray(parsed) ? parsed : parsed.transactions;

  if (!Array.isArray(transactions)) {
    throw new Error(`Replay corpus "${replayCorpusPath}" must be an array or { transactions: [] }`);
  }

  for (const [index, tx] of transactions.entries()) {
    if (
      tx === null ||
      typeof tx !== 'object' ||
      typeof tx.txId !== 'string' ||
      typeof tx.cborHex !== 'string' ||
      typeof tx.description !== 'string' ||
      typeof tx.type !== 'string'
    ) {
      throw new Error(
        `Replay corpus "${replayCorpusPath}" has invalid transaction at index ${index}`
      );
    }
  }

  return transactions;
};

const loadReplayCorpus = async (
  replayCorpusPath: string | undefined
): Promise<SerializedMidgardTransaction[] | null> => {
  if (replayCorpusPath === undefined) {
    return null;
  }

  const resolvedPath = isAbsolute(replayCorpusPath)
    ? replayCorpusPath
    : join(projectRoot, replayCorpusPath);
  const raw = await readFile(resolvedPath, 'utf8');
  return parseReplayCorpusContent(replayCorpusPath, raw);
};

const writeTransactionsWithManifest = async ({
  outputDir,
  filenamePrefix,
  transactions,
  manifestTransactions,
  generationSeed,
  fullConfig,
  mode,
  replayCorpusPath,
}: {
  outputDir: string;
  filenamePrefix: string;
  transactions: SerializedMidgardTransaction[];
  manifestTransactions: TransactionEvidenceEntry[];
  generationSeed: string;
  fullConfig: TransactionGeneratorConfig;
  mode: GenerationMode;
  replayCorpusPath: string | undefined;
}): Promise<string> => {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const filename = `${filenamePrefix}-${timestamp}.json`;
  const transactionsPath = join(resolveOutputDir(outputDir), filename);
  const manifestPath = `${transactionsPath}.manifest.json`;

  await writeFile(transactionsPath, JSON.stringify(transactions, null, 2));
  const manifest: GeneratorManifest = {
    mode,
    generationSeed,
    replayCorpusPath,
    profile: {
      transactionType: fullConfig.transactionType,
      oneToOneRatio: fullConfig.oneToOneRatio,
      batchSize: fullConfig.batchSize,
      concurrency: fullConfig.concurrency,
      intervalSeconds: fullConfig.interval,
    },
    generatedAt: new Date().toISOString(),
    transactionCount: transactions.length,
    transactions: manifestTransactions,
  };

  await writeFile(manifestPath, JSON.stringify(manifest, null, 2));

  return transactionsPath;
};

const submitTransactions = async ({
  txs,
  nodeClient,
  outputDir,
  filenamePrefix,
  generationSeed,
  fullConfig,
  mode,
  replayCorpusPath,
  requestEventsMode,
  requestEventRandom,
}: {
  txs: SerializedMidgardTransaction[];
  nodeClient: MidgardNodeClient;
  outputDir?: string;
  filenamePrefix: string;
  generationSeed: string;
  fullConfig: TransactionGeneratorConfig;
  mode: GenerationMode;
  replayCorpusPath: string | undefined;
  requestEventsMode: RequestEventsMode;
  requestEventRandom: () => number;
}): Promise<void> => {
  const manifestTransactions = txs.map((tx) =>
    toEvidenceEntry(inspectGeneratedTransaction(tx, fullConfig.network), mode)
  );
  recordGeneratedTransactions(state.stats.submissionAggregate, txs.length);

  const txById = new Map(txs.map((tx) => [tx.txId, tx]));
  const manifestById = new Map(manifestTransactions.map((tx) => [tx.txId, tx]));
  const validTransactions = manifestTransactions.filter(
    (entry) => entry.validation.status === 'accepted'
  );
  const rejectedCount = manifestTransactions.length - validTransactions.length;

  const appendRequestEvent = async ({
    txId,
    outcome,
    latencyMs,
    retryCount,
    httpStatusCode,
    responseClass,
    errorClass,
    error,
  }: {
    txId: string;
    outcome: SubmissionOutcome;
    latencyMs: number | null;
    retryCount: number;
    httpStatusCode: number | null;
    responseClass: string;
    errorClass: string | null;
    error?: string;
  }): Promise<void> => {
    if (outputDir === undefined) {
      return;
    }
    if (!shouldEmitRequestEvent(requestEventsMode, requestEventRandom)) {
      return;
    }
    const manifestTx = manifestById.get(txId);
    const sourceTx = txById.get(txId);

    const eventPath = join(resolveOutputDir(outputDir), REQUEST_EVENTS_FILE);
    await appendFile(
      eventPath,
      JSON.stringify({
        ts: new Date().toISOString(),
        txId,
        finalOutcome: outcome,
        outcome,
        responseClass,
        httpStatusCode,
        errorClass,
        latencyMs,
        retryCount,
        transactionType: sourceTx?.type ?? 'unknown',
        transactionProfile: manifestTx?.profile ?? 'unknown',
        cborByteSize: manifestTx?.cborByteSize ?? null,
        midgardByteSize: manifestTx?.midgardByteSize ?? null,
        ...(error !== undefined ? { error } : {}),
      }) + '\n'
    );
  };

  for (const tx of manifestTransactions) {
    if (tx.validation.status === 'rejected') {
      recordSubmissionObservation(state.stats.submissionAggregate, 'rejected', null, 0);
      await appendRequestEvent({
        txId: tx.txId,
        outcome: 'rejected',
        latencyMs: null,
        retryCount: 0,
        httpStatusCode: null,
        responseClass: 'validation_rejected',
        errorClass: 'validation_rejected',
        error: tx.validation.detail ?? tx.validation.rejectCode,
      });
    }
  }

  const nodeAvailable = await nodeClient.isAvailable();

  if (!nodeAvailable) {
    for (const tx of validTransactions) {
      tx.submission.status = 'NODE_UNAVAILABLE';
      recordSubmissionObservation(state.stats.submissionAggregate, 'node_unavailable', null, 0);
      await appendRequestEvent({
        txId: tx.txId,
        outcome: 'node_unavailable',
        latencyMs: null,
        retryCount: 0,
        httpStatusCode: null,
        responseClass: 'node_unavailable',
        errorClass: 'node_unavailable',
      });
    }
    if (outputDir) {
      const outputPath = await writeTransactionsWithManifest({
        outputDir,
        filenamePrefix,
        transactions: txs,
        manifestTransactions,
        generationSeed,
        fullConfig,
        mode,
        replayCorpusPath,
      });
      await writeSubmissionAggregates(outputDir);
      console.log(`Node unavailable - transactions written to ${outputPath}`);
    }
    state.stats.transactionsGenerated += txs.length;
    state.stats.transactionsFailed += rejectedCount;
    return;
  }

  try {
    const submissionStart = Date.now();
    let submitted = 0;
    let failed = rejectedCount;
    let nodeUnavailable = false;
    let nodeUnavailableAnnounced = false;

    for (const tx of validTransactions) {
      const sourceTx = txById.get(tx.txId);
      if (sourceTx === undefined) {
        tx.submission.status = 'ERROR';
        tx.submission.error = 'transaction evidence entry not found in source list';
        recordSubmissionObservation(state.stats.submissionAggregate, 'error', null, 0);
        await appendRequestEvent({
          txId: tx.txId,
          outcome: 'error',
          latencyMs: null,
          retryCount: 0,
          httpStatusCode: null,
          responseClass: 'unknown_error',
          errorClass: 'unknown_error',
          error: tx.submission.error,
        });
        failed++;
        continue;
      }

      recordAttemptedSubmission(state.stats.submissionAggregate);
      const result = await nodeClient.submitTransaction(sourceTx.cborHex);
      const submissionOutcome = submissionOutcomeFromResult(result);
      recordSubmissionObservation(
        state.stats.submissionAggregate,
        submissionOutcome,
        result.latencyMs,
        result.retriesUsed
      );
      await appendRequestEvent({
        txId: tx.txId,
        outcome: submissionOutcome,
        latencyMs: result.latencyMs,
        retryCount: result.retriesUsed,
        httpStatusCode: result.httpStatusCode ?? null,
        responseClass: result.responseClass,
        errorClass: result.errorClass ?? null,
        error: result.error,
      });

      if (result.status === 'NODE_UNAVAILABLE') {
        tx.submission.status = 'NODE_UNAVAILABLE';
        nodeUnavailable = true;
        if (!nodeUnavailableAnnounced) {
          nodeUnavailableAnnounced = true;
          console.log('Node became unavailable during submission.');
        }
        break;
      } else if (result.status === 'ERROR') {
        tx.submission.status = 'ERROR';
        tx.submission.error = result.error;
        failed++;
      } else {
        tx.submission.status = 'SUBMITTED';
        submitted++;
      }
    }

    if (nodeUnavailable) {
      for (const tx of validTransactions) {
        if (tx.submission.status === 'NOT_ATTEMPTED') {
          tx.submission.status = 'NODE_UNAVAILABLE';
          recordSubmissionObservation(state.stats.submissionAggregate, 'node_unavailable', null, 0);
          await appendRequestEvent({
            txId: tx.txId,
            outcome: 'node_unavailable',
            latencyMs: null,
            retryCount: 0,
            httpStatusCode: null,
            responseClass: 'node_unavailable',
            errorClass: 'node_unavailable',
          });
        }
      }
    }

    const submissionEnd = Date.now();
    state.stats.transactionsGenerated += txs.length;
    state.stats.transactionsSubmitted += submitted;
    state.stats.transactionsFailed += failed;

    if (outputDir) {
      const outputPath = await writeTransactionsWithManifest({
        outputDir,
        filenamePrefix,
        transactions: txs,
        manifestTransactions,
        generationSeed,
        fullConfig,
        mode,
        replayCorpusPath,
      });
      await writeSubmissionAggregates(outputDir);
      console.log(`Transactions and manifest written to ${outputPath}`);
    }

    if (!nodeUnavailable) {
      console.log(
        `Submitted ${submitted} transactions (${failed} failed) in ${submissionEnd - submissionStart}ms`
      );
    }
  } catch (submitError) {
    console.error('Failed to submit transactions:', submitError);

    if (outputDir) {
      const outputPath = await writeTransactionsWithManifest({
        outputDir,
        filenamePrefix,
        transactions: txs,
        manifestTransactions,
        generationSeed,
        fullConfig,
        mode,
        replayCorpusPath,
      });
      await writeSubmissionAggregates(outputDir);
      console.log(`Failed submission - transactions written to ${outputPath}`);
    }

    state.stats.transactionsGenerated += txs.length;
    state.stats.transactionsFailed += rejectedCount;
  }
};

/**
 * Starts a transaction generator with the given configuration
 */
export const startGenerator = async (
  config: Partial<TransactionGeneratorConfig> = {}
): Promise<void> => {
  // Stop any existing generator
  if (state.currentPromise) {
    stopGenerator();
  }

  // Reset the stop flag
  state.shouldStop = false;

  // Merge with default config
  const fullConfig: TransactionGeneratorConfig = {
    ...DEFAULT_CONFIG,
    ...config,
  };

  // Validate the configuration
  validateGeneratorConfig(fullConfig);

  const generationSeed = getNormalizedSeed(fullConfig.generationSeed);
  const deterministicStartMs = getDeterministicStartMs(generationSeed);
  const random = createSeededRandom(generationSeed);
  const requestEventsMode: RequestEventsMode = fullConfig.requestEvents ?? 'off';
  const requestEventRandom = createSeededRandom(`${generationSeed}:request-events`);
  const replayCorpus = await loadReplayCorpus(fullConfig.replayCorpusPath);

  // Set up node client with the new configuration structure
  const nodeClient = new MidgardNodeClient({
    baseUrl: fullConfig.nodeEndpoint,
    retryAttempts: fullConfig.nodeRetryAttempts,
    retryDelay: fullConfig.nodeRetryDelay,
    enableLogs: fullConfig.nodeEnableLogs,
  });

  // Create a limiter for concurrent transaction generation
  const concurrencyLimiter = pLimit(fullConfig.concurrency);

  // Pre-initialize a pool of Lucid instances for one-to-one generation.
  // Pool size matches concurrency so every running task gets an instance
  // immediately without contention. Skipped for pure multi-output workloads
  // where Lucid is constructed once per large batch anyway.
  const needsPool =
    fullConfig.transactionType === 'one-to-one' || fullConfig.transactionType === 'mixed';
  const lucidPool = needsPool
    ? await LucidPool.create(
        fullConfig.concurrency,
        fullConfig.walletSeedOrPrivateKey,
        fullConfig.initialUTxO.address,
        fullConfig.initialUTxO.assets,
        fullConfig.network
      )
    : null;

  // Reset stats
  state.resetStats();

  // Create output directory if needed
  if (fullConfig.outputDir) {
    const outputPath = resolveOutputDir(fullConfig.outputDir);
    await mkdir(outputPath, { recursive: true });
  }

  // Log start with more configuration details
  console.log('\nStarting transaction generator with configuration:');
  console.log(`• Type: ${fullConfig.transactionType}`);
  if (fullConfig.transactionType === 'mixed') {
    console.log(`• One-to-One Ratio: ${fullConfig.oneToOneRatio}%`);
  }
  console.log(`• Batch Size: ${fullConfig.batchSize}`);
  console.log(`• Interval: ${fullConfig.interval}s`);
  console.log(`• Concurrency: ${fullConfig.concurrency}`);
  console.log(`• Node Endpoint: ${fullConfig.nodeEndpoint}`);
  console.log(`• Generation Seed: ${generationSeed}`);
  console.log(`• Request Events: ${requestEventsMode}`);
  if (requestEventsMode === 'sampled') {
    console.log(`• Request Event Sample Rate: ${REQUEST_EVENTS_SAMPLE_RATE}`);
  }
  if (fullConfig.replayCorpusPath) {
    console.log(`• Replay Corpus Path: ${fullConfig.replayCorpusPath}`);
  }
  if (fullConfig.autoStopAfterBatch) {
    console.log('• Auto-stop: Enabled (will stop after one batch)');
  }
  console.log();

  // Define the transaction generation function
  const generateTransactions = async () => {
    try {
      if (replayCorpus !== null) {
        await submitTransactions({
          txs: replayCorpus,
          nodeClient,
          outputDir: fullConfig.outputDir,
          filenamePrefix: GENERATED_TX_PREFIX_REPLAY,
          generationSeed,
          fullConfig,
          mode: 'replay',
          replayCorpusPath: fullConfig.replayCorpusPath,
          requestEventsMode,
          requestEventRandom,
        });
        return;
      }

      const taskPlans = buildTaskPlans({
        initialUTxO: fullConfig.initialUTxO,
        batchSize: fullConfig.batchSize,
        transactionType: fullConfig.transactionType,
        oneToOneRatio: fullConfig.oneToOneRatio ?? DEFAULT_CONFIG.oneToOneRatio ?? 70,
        random,
      });

      const tasks = taskPlans.map(async (taskPlan) => {
        return concurrencyLimiter(async () => {
          let txs: SerializedMidgardTransaction[];

          if (taskPlan.useOneToOne) {
            // Acquire a pre-initialized Lucid instance from the pool.
            // Release it immediately after generation so the next task can
            // start building its tx while this task is still submitting.
            const pooledLucid = lucidPool !== null ? await lucidPool.acquire() : undefined;
            try {
              txs = await generateOneToOneTransactions({
                network: fullConfig.network,
                initialUTxO: taskPlan.initialUTxO,
                txsCount: 1,
                walletSeedOrPrivateKey: fullConfig.walletSeedOrPrivateKey,
                nodeClient,
                random,
                deterministicStartMs,
                lucid: pooledLucid,
              });
            } finally {
              if (pooledLucid !== undefined && lucidPool !== null) {
                lucidPool.release(pooledLucid);
              }
            }
          } else {
            txs = await generateMultiOutputTransactions({
              network: fullConfig.network,
              initialUTxO: taskPlan.initialUTxO,
              utxosCount: TRANSACTION_CONSTANTS.OUTPUTS_PER_DISTRIBUTION,
              finalUtxosCount: 1,
              walletSeedOrPrivateKey: fullConfig.walletSeedOrPrivateKey,
              nodeClient,
              random,
            });
          }

          if (!txs || !Array.isArray(txs)) {
            throw new Error('Failed to generate transactions');
          }

          await submitTransactions({
            txs,
            nodeClient,
            outputDir: fullConfig.outputDir,
            filenamePrefix: buildFilenamePrefix(taskPlan.useOneToOne, 'generated'),
            generationSeed,
            fullConfig,
            mode: 'generated',
            replayCorpusPath: undefined,
            requestEventsMode,
            requestEventRandom,
          });
        });
      });

      await Promise.all(tasks);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      state.stats.lastError = errorMessage;
      console.error('Error in transaction generation loop:', errorMessage);
      throw error;
    }
  };

  // Create a function to run the generator loop
  const runGenerator = async () => {
    // Generate at least one batch
    await generateTransactions();

    // If auto-stop is enabled, stop here (for scheduled jobs)
    if (fullConfig.autoStopAfterBatch) {
      console.log('Auto-stop enabled - stopping after one batch');
      state.shouldStop = true;
      return;
    }

    // Otherwise, continue in a loop until stopped manually
    while (!state.shouldStop) {
      // Wait for the specified interval
      await new Promise((resolve) => setTimeout(resolve, fullConfig.interval * 1000));

      // Check if we should stop before generating more transactions
      if (state.shouldStop) break;

      await generateTransactions();
    }
    console.log('Transaction generator stopped');
  };

  // Start the generator
  state.currentPromise = runGenerator()
    .catch((error) => {
      const errorMessage = error instanceof Error ? error.message : String(error);
      state.stats.lastError = errorMessage;
      console.error('Generator failed:', errorMessage);
    })
    .finally(() => {
      state.currentPromise = null;
    });
};

/**
 * Stops the currently running transaction generator
 */
export const stopGenerator = (): Promise<void> => {
  state.shouldStop = true;
  return Promise.resolve();
};

export const waitForGeneratorStop = async (maxMs = 2000): Promise<void> => {
  const runningPromise = state.currentPromise;
  if (runningPromise === null) {
    return;
  }

  let timeoutHandle: ReturnType<typeof setTimeout> | null = null;
  try {
    await Promise.race([
      runningPromise,
      new Promise<void>((_, reject) => {
        timeoutHandle = setTimeout(() => {
          reject(new Error(`Timed out waiting for generator to stop after ${maxMs}ms`));
        }, maxMs);
      }),
    ]);
  } finally {
    if (timeoutHandle !== null) {
      clearTimeout(timeoutHandle);
    }
  }

  if (state.stats.lastError !== null) {
    throw new Error(`Generator stopped with error: ${state.stats.lastError}`);
  }
};

/**
 * Gets the current status of the transaction generator
 */
export const getGeneratorStatus = (): {
  running: boolean;
  transactionsGenerated: number;
  transactionsSubmitted: number;
  transactionsFailed: number;
  lastError: string | null;
  uptime: number | null;
} => {
  return {
    running: state.isRunning(),
    transactionsGenerated: state.stats.transactionsGenerated,
    transactionsSubmitted: state.stats.transactionsSubmitted,
    transactionsFailed: state.stats.transactionsFailed,
    lastError: state.stats.lastError,
    uptime: state.stats.startTime
      ? Math.floor((new Date().getTime() - state.stats.startTime.getTime()) / 1000)
      : null,
  };
};
