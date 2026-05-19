import { randomBytes } from 'node:crypto';
import { createReadStream } from 'node:fs';
import { appendFile, mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, isAbsolute, join } from 'node:path';
import { createInterface } from 'node:readline';
import { fileURLToPath } from 'node:url';

import { UTxO } from '@lucid-evolution/lucid';
import type { queueAsPromised } from 'fastq';
import * as fastq from 'fastq';
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
import { toEvidenceEntry, TransactionEvidenceEntry } from './artifact-metadata.js';
import { createSeededRandom, randomHex, randomInt } from './deterministic-random.js';
import {
  createEmptySubmissionAggregate,
  recordAttemptedSubmission,
  recordGeneratedTransactions,
  recordGenerationLatency,
  recordInFlightSubmits,
  recordLucidPoolWaitLatency,
  recordPreparedQueueDepth,
  recordQueueBackpressureWaitLatency,
  recordSubmissionObservation,
  recordSubmitLatency,
  recordTokenLate,
  recordTokenWaitLatency,
  REQUEST_EVENTS_SAMPLE_RATE,
  type RequestEventsMode,
  type SubmissionOutcome,
  toSubmissionAggregateWithPercentiles,
} from './submission-evidence.js';
import { inspectGeneratedTransaction } from './transaction-inspector.js';

const OUTPUT_INDEX_UPPER_EXCLUSIVE = 1001;
const PROJECT_ROOT_RELATIVE_PATH = '../../../..';
const SUBMISSION_AGGREGATE_FILE = 'submission-aggregates.json';
const REQUEST_EVENTS_FILE = 'request-events.jsonl';
const SUBMISSION_AGGREGATE_FLUSH_INTERVAL_MS = 1_000;
const QUEUE_BACKPRESSURE_SLEEP_MS = 1;
const TOKEN_LATE_THRESHOLD_MS = 10;
const RUNTIME_TELEMETRY_INTERVAL_MS = 10_000;

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
  private _stopHook: (() => void) | null = null;
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

  get stopHook(): (() => void) | null {
    return this._stopHook;
  }

  set stopHook(value: (() => void) | null) {
    this._stopHook = value;
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

interface PreparedSubmission {
  sourceTx: SerializedMidgardTransaction;
  evidenceEntry: TransactionEvidenceEntry;
  mode: GenerationMode;
}

interface ReplayCorpusFile {
  transactions: SerializedMidgardTransaction[];
}

type ReplayCorpus =
  | { type: 'array'; transactions: SerializedMidgardTransaction[] }
  | { type: 'jsonl'; resolvedPath: string; sourcePath: string };

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

class TokenBucket {
  private tokens = 1;
  private lastRefillMs = Date.now();
  private readonly ratePerSecond: number;
  private readonly maxTokens: number;
  private readonly onLateToken: (() => void) | undefined;

  constructor(ratePerSecond: number, maxTokens = 1, onLateToken?: () => void) {
    this.ratePerSecond = ratePerSecond;
    this.maxTokens = Math.max(1, maxTokens);
    this.onLateToken = onLateToken;
  }

  async waitForToken(shouldStop: () => boolean): Promise<{ acquired: boolean; waitMs: number }> {
    const startedAtMs = Date.now();
    while (true) {
      if (shouldStop()) {
        return { acquired: false, waitMs: Date.now() - startedAtMs };
      }

      this.refill();
      if (this.tokens >= 1) {
        this.tokens -= 1;
        return { acquired: true, waitMs: Date.now() - startedAtMs };
      }

      const waitMs = Math.max(1, Math.ceil(((1 - this.tokens) / this.ratePerSecond) * 1000));
      if (waitMs >= TOKEN_LATE_THRESHOLD_MS) {
        this.onLateToken?.();
      }
      await sleep(waitMs);
    }
  }

  private refill(): void {
    const now = Date.now();
    const elapsedSeconds = Math.max(0, (now - this.lastRefillMs) / 1000);
    if (elapsedSeconds <= 0) {
      return;
    }

    this.tokens = Math.min(this.maxTokens, this.tokens + elapsedSeconds * this.ratePerSecond);
    this.lastRefillMs = now;
  }
}

// Get the shared instance
const state = TxGeneratorState.getInstance();

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    setTimeout(resolve, ms);
  });

const formatRate = (countDelta: number, elapsedMs: number): string => {
  if (elapsedMs <= 0) {
    return '0.0';
  }
  return ((countDelta * 1000) / elapsedMs).toFixed(1);
};

const formatMeanMs = (sumMs: number, count: number): string => {
  if (count <= 0) {
    return 'n/a';
  }
  return (sumMs / count).toFixed(1);
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

const parseReplayCorpusLine = (
  replayCorpusPath: string,
  lineNumber: number,
  line: string
): SerializedMidgardTransaction => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(line);
  } catch {
    throw new Error(`Replay corpus "${replayCorpusPath}" has invalid JSON at line ${lineNumber}`);
  }

  if (
    parsed === null ||
    typeof parsed !== 'object' ||
    !('txId' in parsed) ||
    !('cborHex' in parsed) ||
    !('description' in parsed) ||
    !('type' in parsed)
  ) {
    throw new Error(
      `Replay corpus "${replayCorpusPath}" has invalid transaction at line ${lineNumber}`
    );
  }

  const tx = parsed as SerializedMidgardTransaction;
  if (
    typeof tx.txId !== 'string' ||
    typeof tx.cborHex !== 'string' ||
    typeof tx.description !== 'string' ||
    typeof tx.type !== 'string'
  ) {
    throw new Error(
      `Replay corpus "${replayCorpusPath}" has invalid transaction at line ${lineNumber}`
    );
  }

  return tx;
};

const loadReplayCorpus = async (
  replayCorpusPath: string | undefined
): Promise<ReplayCorpus | null> => {
  if (replayCorpusPath === undefined) {
    return null;
  }

  const resolvedPath = isAbsolute(replayCorpusPath)
    ? replayCorpusPath
    : join(projectRoot, replayCorpusPath);

  if (resolvedPath.endsWith('.jsonl')) {
    return {
      type: 'jsonl',
      resolvedPath,
      sourcePath: replayCorpusPath,
    };
  }

  const raw = await readFile(resolvedPath, 'utf8');
  return {
    type: 'array',
    transactions: parseReplayCorpusContent(replayCorpusPath, raw),
  };
};

async function appendRequestEvent(
  outputDir: string | undefined,
  requestEventsMode: RequestEventsMode,
  requestEventRandom: () => number,
  input: {
    sourceTx: SerializedMidgardTransaction;
    evidenceEntry: TransactionEvidenceEntry;
    outcome: SubmissionOutcome;
    latencyMs: number | null;
    retryCount: number;
    httpStatusCode: number | null;
    responseClass: string;
    errorClass: string | null;
    error?: string;
  }
): Promise<void> {
  if (outputDir === undefined) {
    return;
  }
  if (!shouldEmitRequestEvent(requestEventsMode, requestEventRandom)) {
    return;
  }

  const eventPath = join(resolveOutputDir(outputDir), REQUEST_EVENTS_FILE);
  await appendFile(
    eventPath,
    JSON.stringify({
      ts: new Date().toISOString(),
      txId: input.sourceTx.txId,
      finalOutcome: input.outcome,
      outcome: input.outcome,
      responseClass: input.responseClass,
      httpStatusCode: input.httpStatusCode,
      errorClass: input.errorClass,
      latencyMs: input.latencyMs,
      retryCount: input.retryCount,
      transactionType: input.sourceTx.type,
      transactionProfile: input.evidenceEntry.profile,
      cborByteSize: input.evidenceEntry.cborByteSize,
      midgardByteSize: input.evidenceEntry.midgardByteSize,
      ...(input.error !== undefined ? { error: input.error } : {}),
    }) + '\n'
  );
}

function resolveTargetTps(config: TransactionGeneratorConfig): number | null {
  if (config.targetTps !== undefined) {
    return config.targetTps > 0 ? config.targetTps : null;
  }
  if (config.interval <= 0) {
    return null;
  }
  return config.batchSize / config.interval;
}

async function withBackpressureCapacity(
  queue: queueAsPromised<PreparedSubmission>,
  queueCapacity: number,
  getActiveWorkers: () => number
): Promise<number> {
  const startedAtMs = Date.now();
  while (!state.shouldStop) {
    const queued = queue.length();
    const inFlight = getActiveWorkers();
    recordPreparedQueueDepth(state.stats.submissionAggregate, queued + inFlight);
    if (queued + inFlight < queueCapacity) {
      return Date.now() - startedAtMs;
    }
    await sleep(QUEUE_BACKPRESSURE_SLEEP_MS);
  }
  return Date.now() - startedAtMs;
}

async function processPreparedSubmission(params: {
  prepared: PreparedSubmission;
  nodeClient: MidgardNodeClient;
  outputDir: string | undefined;
  requestEventsMode: RequestEventsMode;
  requestEventRandom: () => number;
  rateLimiter: TokenBucket | null;
}): Promise<void> {
  const { prepared, nodeClient, outputDir, requestEventsMode, requestEventRandom, rateLimiter } =
    params;

  if (prepared.evidenceEntry.validation.status === 'rejected') {
    prepared.evidenceEntry.submission.status = 'VALIDATION_REJECTED';
    state.stats.transactionsFailed += 1;
    recordSubmissionObservation(state.stats.submissionAggregate, 'rejected', null, 0);
    await appendRequestEvent(outputDir, requestEventsMode, requestEventRandom, {
      sourceTx: prepared.sourceTx,
      evidenceEntry: prepared.evidenceEntry,
      outcome: 'rejected',
      latencyMs: null,
      retryCount: 0,
      httpStatusCode: null,
      responseClass: 'validation_rejected',
      errorClass: 'validation_rejected',
      error:
        prepared.evidenceEntry.validation.detail ??
        prepared.evidenceEntry.validation.rejectCode ??
        'validation_rejected',
    });
    return;
  }

  if (rateLimiter !== null) {
    const tokenResult = await rateLimiter.waitForToken(() => state.shouldStop);
    recordTokenWaitLatency(state.stats.submissionAggregate, tokenResult.waitMs);
    if (!tokenResult.acquired) {
      return;
    }
  }

  recordAttemptedSubmission(state.stats.submissionAggregate);

  const result = await nodeClient.submitTransaction(prepared.sourceTx.cborHex);
  recordSubmitLatency(state.stats.submissionAggregate, result.latencyMs);

  const submissionOutcome = submissionOutcomeFromResult(result);
  recordSubmissionObservation(
    state.stats.submissionAggregate,
    submissionOutcome,
    result.latencyMs,
    result.retriesUsed
  );

  await appendRequestEvent(outputDir, requestEventsMode, requestEventRandom, {
    sourceTx: prepared.sourceTx,
    evidenceEntry: prepared.evidenceEntry,
    outcome: submissionOutcome,
    latencyMs: result.latencyMs,
    retryCount: result.retriesUsed,
    httpStatusCode: result.httpStatusCode ?? null,
    responseClass: result.responseClass,
    errorClass: result.errorClass ?? null,
    error: result.error,
  });

  if (result.status === 'NODE_UNAVAILABLE') {
    prepared.evidenceEntry.submission.status = 'NODE_UNAVAILABLE';
    return;
  }

  if (result.status === 'ERROR') {
    prepared.evidenceEntry.submission.status = 'ERROR';
    prepared.evidenceEntry.submission.error = result.error;
    state.stats.transactionsFailed += 1;
    return;
  }

  prepared.evidenceEntry.submission.status = 'SUBMITTED';
  state.stats.transactionsSubmitted += 1;
}

async function prepareAndEnqueueTaskPlan(params: {
  taskPlan: TaskPlan;
  mode: GenerationMode;
  fullConfig: TransactionGeneratorConfig;
  random: () => number;
  deterministicStartMs: number;
  lucidPool: LucidPool | null;
  submissionQueue: queueAsPromised<PreparedSubmission>;
  queueCapacity: number;
  getActiveWorkers: () => number;
}): Promise<void> {
  const {
    taskPlan,
    mode,
    fullConfig,
    random,
    deterministicStartMs,
    lucidPool,
    submissionQueue,
    queueCapacity,
    getActiveWorkers,
  } = params;

  let txs: SerializedMidgardTransaction[];
  const generationStartedAtMs = Date.now();

  if (taskPlan.useOneToOne) {
    const poolWaitStartedAtMs = Date.now();
    const pooledLucid = lucidPool !== null ? await lucidPool.acquire() : undefined;
    recordLucidPoolWaitLatency(state.stats.submissionAggregate, Date.now() - poolWaitStartedAtMs);
    try {
      txs = await generateOneToOneTransactions({
        network: fullConfig.network,
        initialUTxO: taskPlan.initialUTxO,
        txsCount: 1,
        walletSeedOrPrivateKey: fullConfig.walletSeedOrPrivateKey,
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
      random,
    });
  }

  recordGeneratedTransactions(state.stats.submissionAggregate, txs.length);
  state.stats.transactionsGenerated += txs.length;
  const generationLatencyPerTxMs = Math.max(
    1,
    Math.floor((Date.now() - generationStartedAtMs) / Math.max(1, txs.length))
  );

  for (const sourceTx of txs) {
    recordGenerationLatency(state.stats.submissionAggregate, generationLatencyPerTxMs);
    const evidenceEntry = toEvidenceEntry(
      inspectGeneratedTransaction(sourceTx, fullConfig.network),
      mode
    );
    if (state.shouldStop) {
      return;
    }

    const queueWaitMs = await withBackpressureCapacity(
      submissionQueue,
      queueCapacity,
      getActiveWorkers
    );
    recordQueueBackpressureWaitLatency(state.stats.submissionAggregate, queueWaitMs);
    if (state.shouldStop) {
      return;
    }

    void submissionQueue.push({ sourceTx, evidenceEntry, mode }).catch((error: unknown) => {
      if (state.stats.lastError === null && error instanceof Error) {
        state.stats.lastError = error.message;
      }
      state.shouldStop = true;
    });
    recordPreparedQueueDepth(
      state.stats.submissionAggregate,
      submissionQueue.length() + getActiveWorkers()
    );
  }
}

async function prepareAndEnqueueReplay(params: {
  replayCorpus: ReplayCorpus;
  fullConfig: TransactionGeneratorConfig;
  submissionQueue: queueAsPromised<PreparedSubmission>;
  queueCapacity: number;
  getActiveWorkers: () => number;
}): Promise<void> {
  const { replayCorpus, fullConfig, submissionQueue, queueCapacity, getActiveWorkers } = params;

  const processSourceTx = async (sourceTx: SerializedMidgardTransaction): Promise<void> => {
    if (state.shouldStop) {
      return;
    }

    recordGenerationLatency(state.stats.submissionAggregate, 1);
    recordGeneratedTransactions(state.stats.submissionAggregate, 1);
    state.stats.transactionsGenerated += 1;

    const evidenceEntry = toEvidenceEntry(
      inspectGeneratedTransaction(sourceTx, fullConfig.network),
      'replay'
    );

    const queueWaitMs = await withBackpressureCapacity(
      submissionQueue,
      queueCapacity,
      getActiveWorkers
    );
    recordQueueBackpressureWaitLatency(state.stats.submissionAggregate, queueWaitMs);
    if (state.shouldStop) {
      return;
    }

    void submissionQueue
      .push({ sourceTx, evidenceEntry, mode: 'replay' })
      .catch((error: unknown) => {
        if (state.stats.lastError === null && error instanceof Error) {
          state.stats.lastError = error.message;
        }
        state.shouldStop = true;
      });
    recordPreparedQueueDepth(
      state.stats.submissionAggregate,
      submissionQueue.length() + getActiveWorkers()
    );
  };

  if (replayCorpus.type === 'array') {
    for (const sourceTx of replayCorpus.transactions) {
      await processSourceTx(sourceTx);
    }
    return;
  }

  const rl = createInterface({
    input: createReadStream(replayCorpus.resolvedPath, { encoding: 'utf8' }),
    crlfDelay: Infinity,
  });
  let lineNumber = 0;
  let sawAtLeastOneLine = false;

  for await (const rawLine of rl) {
    if (state.shouldStop) {
      rl.close();
      return;
    }
    const line = rawLine.trim();
    if (line.length === 0) {
      continue;
    }
    sawAtLeastOneLine = true;
    lineNumber += 1;
    const sourceTx = parseReplayCorpusLine(replayCorpus.sourcePath, lineNumber, line);
    await processSourceTx(sourceTx);
  }

  if (!sawAtLeastOneLine) {
    throw new Error(`Replay corpus "${replayCorpus.sourcePath}" is empty`);
  }
}

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

/**
 * Starts a transaction generator with the given configuration
 */
export const startGenerator = async (
  config: Partial<TransactionGeneratorConfig> = {}
): Promise<void> => {
  // Stop any existing generator
  if (state.currentPromise) {
    await stopGenerator();
    await waitForGeneratorStop();
  }

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

  const resolvedTargetTps = resolveTargetTps(fullConfig);
  const maxInFlight = fullConfig.maxInFlight ?? fullConfig.concurrency;
  const generationConcurrency =
    fullConfig.generationConcurrency ?? Math.max(1, Math.min(maxInFlight, fullConfig.batchSize));
  const preparedQueueCapacity =
    fullConfig.preparedQueueCapacity ?? Math.max(fullConfig.batchSize * 2, maxInFlight * 2, 1);

  // Set up node client with the new configuration structure
  const nodeClient = new MidgardNodeClient({
    baseUrl: fullConfig.nodeEndpoint,
    retryAttempts: fullConfig.nodeRetryAttempts,
    retryDelay: fullConfig.nodeRetryDelay,
    submitTimeoutMs: fullConfig.nodeSubmitTimeoutMs,
    enableLogs: fullConfig.nodeEnableLogs,
    skipAvailabilityCheck: true,
  });
  // Pre-initialize a pool of Lucid instances for one-to-one generation.
  const needsPool =
    fullConfig.transactionType === 'one-to-one' || fullConfig.transactionType === 'mixed';
  const lucidPool = needsPool
    ? await LucidPool.create(
        generationConcurrency,
        fullConfig.walletSeedOrPrivateKey,
        fullConfig.initialUTxO.address,
        fullConfig.initialUTxO.assets,
        fullConfig.network
      )
    : null;

  let activeSubmissionWorkers = 0;
  const rateLimiter =
    resolvedTargetTps !== null
      ? new TokenBucket(resolvedTargetTps, Math.max(1, maxInFlight), () => {
          recordTokenLate(state.stats.submissionAggregate);
        })
      : null;

  const submissionQueue: queueAsPromised<PreparedSubmission> = fastq.promise(
    async (prepared) => {
      activeSubmissionWorkers += 1;
      recordInFlightSubmits(state.stats.submissionAggregate, activeSubmissionWorkers);
      recordPreparedQueueDepth(
        state.stats.submissionAggregate,
        submissionQueue.length() + activeSubmissionWorkers
      );
      try {
        await processPreparedSubmission({
          prepared,
          nodeClient,
          outputDir: fullConfig.outputDir,
          requestEventsMode,
          requestEventRandom,
          rateLimiter,
        });
      } finally {
        activeSubmissionWorkers = Math.max(0, activeSubmissionWorkers - 1);
        recordInFlightSubmits(state.stats.submissionAggregate, activeSubmissionWorkers);
        recordPreparedQueueDepth(
          state.stats.submissionAggregate,
          submissionQueue.length() + activeSubmissionWorkers
        );
      }
    },
    Math.max(1, maxInFlight)
  );

  // Reset stats
  state.resetStats();

  // Create output directory if needed
  let aggregateFlushTimer: ReturnType<typeof setInterval> | null = null;
  let runtimeTelemetryTimer: ReturnType<typeof setInterval> | null = null;
  if (fullConfig.outputDir) {
    const outputPath = resolveOutputDir(fullConfig.outputDir);
    await mkdir(outputPath, { recursive: true });

    aggregateFlushTimer = setInterval(() => {
      void writeSubmissionAggregates(fullConfig.outputDir as string).catch(() => {
        // best effort periodic flush
      });
    }, SUBMISSION_AGGREGATE_FLUSH_INTERVAL_MS);
    aggregateFlushTimer.unref?.();
  }

  let previousTelemetrySnapshot = {
    capturedAtMs: Date.now(),
    generated: 0,
    attempted: 0,
    submitted: 0,
    timedOut: 0,
    errors: 0,
  };

  runtimeTelemetryTimer = setInterval(() => {
    const capturedAtMs = Date.now();
    const elapsedMs = Math.max(1, capturedAtMs - previousTelemetrySnapshot.capturedAtMs);
    const aggregate = state.stats.submissionAggregate;
    const submissionPercentiles = toSubmissionAggregateWithPercentiles(aggregate).percentilesMs;

    const generatedDelta = aggregate.counters.generated - previousTelemetrySnapshot.generated;
    const attemptedDelta = aggregate.counters.attempted - previousTelemetrySnapshot.attempted;
    const submittedDelta = aggregate.counters.submitted - previousTelemetrySnapshot.submitted;
    const timedOutDelta = aggregate.counters.timed_out - previousTelemetrySnapshot.timedOut;
    const errorDelta = aggregate.counters.error - previousTelemetrySnapshot.errors;

    const queueDepth = aggregate.schedulerMetrics.prepared_queue_depth.current;
    const inFlight = aggregate.schedulerMetrics.in_flight_submits.current;
    const queueCapacityUtilization = (
      (queueDepth / Math.max(1, preparedQueueCapacity)) *
      100
    ).toFixed(1);

    const submitP95Ms = submissionPercentiles.schedulerMetrics.submit_latency.p95 ?? 0;
    const generationP95Ms = submissionPercentiles.schedulerMetrics.generation_latency.p95 ?? 0;
    const queueWaitP95Ms =
      submissionPercentiles.schedulerMetrics.queue_backpressure_wait_latency.p95 ?? 0;
    const tokenWaitP95Ms = submissionPercentiles.schedulerMetrics.token_wait_latency.p95 ?? 0;
    const poolWaitP95Ms = submissionPercentiles.schedulerMetrics.lucid_pool_wait_latency.p95 ?? 0;

    const submitMeanMs = formatMeanMs(
      aggregate.schedulerMetrics.submit_latency.sumMs,
      aggregate.schedulerMetrics.submit_latency.count
    );

    const queueWaitMeanMs = formatMeanMs(
      aggregate.schedulerMetrics.queue_backpressure_wait_latency.sumMs,
      aggregate.schedulerMetrics.queue_backpressure_wait_latency.count
    );

    const tokenWaitMeanMs = formatMeanMs(
      aggregate.schedulerMetrics.token_wait_latency.sumMs,
      aggregate.schedulerMetrics.token_wait_latency.count
    );

    const poolWaitMeanMs = formatMeanMs(
      aggregate.schedulerMetrics.lucid_pool_wait_latency.sumMs,
      aggregate.schedulerMetrics.lucid_pool_wait_latency.count
    );

    console.log(
      `[telemetry] gen=${formatRate(generatedDelta, elapsedMs)}/s attempt=${formatRate(attemptedDelta, elapsedMs)}/s submit=${formatRate(submittedDelta, elapsedMs)}/s timeout=${formatRate(timedOutDelta, elapsedMs)}/s error=${formatRate(errorDelta, elapsedMs)}/s inflight=${inFlight}/${maxInFlight} queue=${queueDepth}/${preparedQueueCapacity} (${queueCapacityUtilization}%) p95ms{submit=${submitP95Ms},gen=${generationP95Ms},qwait=${queueWaitP95Ms},token=${tokenWaitP95Ms},pool=${poolWaitP95Ms}} meanms{submit=${submitMeanMs},qwait=${queueWaitMeanMs},token=${tokenWaitMeanMs},pool=${poolWaitMeanMs}}`
    );

    const submitSaturationThresholdMs = Math.floor(
      (fullConfig.nodeSubmitTimeoutMs ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.SUBMIT_TIMEOUT_MS) *
        0.8
    );
    if (
      submitP95Ms >= submitSaturationThresholdMs &&
      queueDepth >= Math.floor(preparedQueueCapacity * 0.8)
    ) {
      console.warn(
        `[telemetry][bottleneck] submit path saturated: submit p95=${submitP95Ms}ms (threshold=${submitSaturationThresholdMs}ms) with queue utilization ${queueCapacityUtilization}%`
      );
    } else if (generationP95Ms >= 1_000 && queueDepth < Math.floor(preparedQueueCapacity * 0.2)) {
      console.warn(
        `[telemetry][bottleneck] generation path saturated: generation p95=${generationP95Ms}ms with low prepared queue depth`
      );
    }

    previousTelemetrySnapshot = {
      capturedAtMs,
      generated: aggregate.counters.generated,
      attempted: aggregate.counters.attempted,
      submitted: aggregate.counters.submitted,
      timedOut: aggregate.counters.timed_out,
      errors: aggregate.counters.error,
    };
  }, RUNTIME_TELEMETRY_INTERVAL_MS);
  runtimeTelemetryTimer.unref?.();

  state.stopHook = () => {
    submissionQueue.kill();
  };

  // Log start with more configuration details
  console.log('\nStarting transaction generator with configuration:');
  console.log(`• Type: ${fullConfig.transactionType}`);
  if (fullConfig.transactionType === 'mixed') {
    console.log(`• One-to-One Ratio: ${fullConfig.oneToOneRatio}%`);
  }
  console.log(`• Legacy Batch Size: ${fullConfig.batchSize}`);
  console.log(`• Legacy Interval: ${fullConfig.interval}s`);
  console.log(`• Max In Flight: ${maxInFlight}`);
  console.log(`• Generation Concurrency: ${generationConcurrency}`);
  console.log(`• Prepared Queue Capacity: ${preparedQueueCapacity}`);
  console.log(`• Target TPS: ${resolvedTargetTps === null ? 'unbounded' : resolvedTargetTps}`);
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
    console.log('• Auto-stop: Enabled (bounded prefill then drain)');
  }
  console.log();

  const producer = async (): Promise<void> => {
    if (replayCorpus !== null) {
      if (fullConfig.autoStopAfterBatch) {
        await prepareAndEnqueueReplay({
          replayCorpus,
          fullConfig,
          submissionQueue,
          queueCapacity: preparedQueueCapacity,
          getActiveWorkers: () => activeSubmissionWorkers,
        });
        return;
      }

      while (!state.shouldStop) {
        await prepareAndEnqueueReplay({
          replayCorpus,
          fullConfig,
          submissionQueue,
          queueCapacity: preparedQueueCapacity,
          getActiveWorkers: () => activeSubmissionWorkers,
        });
      }
      return;
    }

    const generationLimiter = pLimit(generationConcurrency);
    const generationWorkers: Promise<void>[] = [];
    const oneToOneRatio = fullConfig.oneToOneRatio ?? DEFAULT_CONFIG.oneToOneRatio ?? 70;

    const taskBudget = fullConfig.autoStopAfterBatch
      ? fullConfig.batchSize
      : Number.POSITIVE_INFINITY;
    let reservedTasks = 0;

    const reserveTask = (): boolean => {
      if (reservedTasks >= taskBudget) {
        return false;
      }
      reservedTasks += 1;
      return true;
    };

    const runWorker = async (): Promise<void> => {
      while (!state.shouldStop) {
        if (!reserveTask()) {
          return;
        }

        await generationLimiter(async () => {
          const taskPlan = buildTaskPlans({
            initialUTxO: fullConfig.initialUTxO,
            batchSize: 1,
            transactionType: fullConfig.transactionType,
            oneToOneRatio,
            random,
          })[0];

          await prepareAndEnqueueTaskPlan({
            taskPlan,
            mode: 'generated',
            fullConfig,
            random,
            deterministicStartMs,
            lucidPool,
            submissionQueue,
            queueCapacity: preparedQueueCapacity,
            getActiveWorkers: () => activeSubmissionWorkers,
          });
        });

        // Prevent microtask starvation when mocked generators resolve instantly.
        await sleep(0);
      }
    };

    for (let workerIndex = 0; workerIndex < generationConcurrency; workerIndex += 1) {
      generationWorkers.push(runWorker());
    }

    await Promise.all(generationWorkers);
  };

  const runGenerator = async () => {
    await producer();

    if (fullConfig.autoStopAfterBatch) {
      await submissionQueue.drained();
    } else {
      while (!state.shouldStop) {
        await sleep(25);
      }
    }

    if (fullConfig.outputDir) {
      await writeSubmissionAggregates(fullConfig.outputDir);
    }

    console.log('Transaction generator stopped');
  };

  state.currentPromise = runGenerator()
    .catch((error) => {
      const errorMessage = error instanceof Error ? error.message : String(error);
      state.stats.lastError = errorMessage;
      console.error('Generator failed:', errorMessage);
    })
    .finally(async () => {
      state.currentPromise = null;
      state.stopHook = null;
      if (aggregateFlushTimer !== null) {
        clearInterval(aggregateFlushTimer);
      }
      if (runtimeTelemetryTimer !== null) {
        clearInterval(runtimeTelemetryTimer);
      }
      if (fullConfig.outputDir) {
        await writeSubmissionAggregates(fullConfig.outputDir).catch(() => {
          // best effort
        });
      }
    });
};

/**
 * Stops the currently running transaction generator
 */
export const stopGenerator = (): Promise<void> => {
  state.shouldStop = true;
  state.stopHook?.();
  if (state.currentPromise === null) {
    return Promise.resolve();
  }
  return Promise.race([state.currentPromise, sleep(5_000)]).then(() => undefined);
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
