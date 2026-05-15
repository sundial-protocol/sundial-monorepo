import { randomBytes } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { UTxO } from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import { MidgardNodeClient } from '../client/node-client.js';
import { SerializedMidgardTransaction } from '../client/types.js';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../generators/index.js';
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
import { inspectGeneratedTransaction } from './transaction-inspector.js';

const GENERATED_TX_PREFIX_ONE_TO_ONE = 'one-to-one';
const GENERATED_TX_PREFIX_MULTI_OUTPUT = 'multi-output';
const GENERATED_TX_PREFIX_REPLAY = 'replay';
const OUTPUT_INDEX_UPPER_EXCLUSIVE = 1001;
const PROJECT_ROOT_RELATIVE_PATH = '../../../..';

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
    };
  }

  isRunning(): boolean {
    return !this._shouldStop && this._currentPromise !== null;
  }
}

type GenerationMode = 'generated' | 'replay';

interface TaskPlan {
  initialUTxO: UTxO;
  useOneToOne: boolean;
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
  const transactionsPath = join(projectRoot, outputDir, filename);
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
}: {
  txs: SerializedMidgardTransaction[];
  nodeClient: MidgardNodeClient;
  outputDir?: string;
  filenamePrefix: string;
  generationSeed: string;
  fullConfig: TransactionGeneratorConfig;
  mode: GenerationMode;
  replayCorpusPath: string | undefined;
}): Promise<void> => {
  const manifestTransactions = txs.map((tx) =>
    toEvidenceEntry(inspectGeneratedTransaction(tx, fullConfig.network), mode)
  );
  const validTransactions = manifestTransactions.filter(
    (entry) => entry.validation.status === 'accepted'
  );
  const rejectedCount = manifestTransactions.length - validTransactions.length;
  const nodeAvailable = await nodeClient.isAvailable();

  if (!nodeAvailable) {
    for (const tx of validTransactions) {
      tx.submission.status = 'NODE_UNAVAILABLE';
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
      const sourceTx = txs.find((item) => item.txId === tx.txId);
      if (sourceTx === undefined) {
        tx.submission.status = 'ERROR';
        tx.submission.error = 'transaction evidence entry not found in source list';
        failed++;
        continue;
      }

      const result = await nodeClient.submitTransaction(sourceTx.cborHex);

      if (result && result.status === 'NODE_UNAVAILABLE') {
        tx.submission.status = 'NODE_UNAVAILABLE';
        nodeUnavailable = true;
        if (!nodeUnavailableAnnounced) {
          nodeUnavailableAnnounced = true;
          console.log('Node became unavailable during submission.');
        }
        break;
      } else if (result && result.status === 'ERROR') {
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

  // Reset stats
  state.resetStats();

  // Create output directory if needed
  if (fullConfig.outputDir) {
    const outputPath = join(projectRoot, fullConfig.outputDir);
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
        });
        return;
      }

      const uniqueUTxOs = generateUniqueUTxOs(fullConfig.initialUTxO, fullConfig.batchSize, random);
      const taskPlans: TaskPlan[] = uniqueUTxOs.map((taskUTxO) => ({
        initialUTxO: taskUTxO,
        useOneToOne:
          fullConfig.transactionType === 'one-to-one' ||
          (fullConfig.transactionType === 'mixed' &&
            random() * 100 < (fullConfig.oneToOneRatio ?? 70)),
      }));

      const tasks = taskPlans.map(async (taskPlan) => {
        return concurrencyLimiter(async () => {
          const txs = taskPlan.useOneToOne
            ? await generateOneToOneTransactions({
                network: fullConfig.network,
                initialUTxO: taskPlan.initialUTxO,
                txsCount: 1,
                walletSeedOrPrivateKey: fullConfig.walletSeedOrPrivateKey,
                nodeClient,
                random,
                deterministicStartMs,
              })
            : await generateMultiOutputTransactions({
                network: fullConfig.network,
                initialUTxO: taskPlan.initialUTxO,
                utxosCount: TRANSACTION_CONSTANTS.OUTPUTS_PER_DISTRIBUTION,
                finalUtxosCount: 1,
                walletSeedOrPrivateKey: fullConfig.walletSeedOrPrivateKey,
                nodeClient,
                random,
              });

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
  state.currentPromise = runGenerator().catch((error) => {
    const errorMessage = error instanceof Error ? error.message : String(error);
    state.stats.lastError = errorMessage;
    console.error('Generator failed:', errorMessage);
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
