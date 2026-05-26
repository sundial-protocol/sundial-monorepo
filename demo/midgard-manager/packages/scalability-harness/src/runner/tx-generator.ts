import { type ChildProcess, spawn, type SpawnOptions } from 'node:child_process';
import { readFile } from 'node:fs/promises';
import path, { isAbsolute, resolve } from 'node:path';
import { createInterface } from 'node:readline';

import type { ScalabilityScenario } from '../config/scenario.js';
import type { LoadTier } from '../config/tiers.js';
import type { ArtifactWriter } from '../evidence/artifacts.js';
import type {
  TxGeneratorStartedEvent,
  TxGeneratorStoppedEvent,
  TxGeneratorSubmissionAggregateEvent,
} from '../evidence/load-events.js';
import { makeEvent } from '../evidence/load-events.js';

export const SIGINT_GRACE_MS = 5_000;
export const SIGTERM_GRACE_MS = 5_000;
export const SUBMISSION_AGGREGATES_FILE = 'submission-aggregates.json';
const TX_GENERATOR_CLI_PATH = 'dist/bin/index.js';
const MAX_TAIL_LINES = 40;

export const REQUEST_EVENT_MODES = ['off', 'sampled', 'all'] as const;
export type RequestEventsMode = (typeof REQUEST_EVENT_MODES)[number];

export interface SubmissionLatencyHistogram {
  boundsMs: number[];
  counts: number[];
  overflowCount: number;
  count: number;
  sumMs: number;
  minMs: number | null;
  maxMs: number | null;
}

export interface SubmissionAggregate {
  counters: {
    generated: number;
    attempted: number;
    submitted: number;
    rejected: number;
    node_unavailable: number;
    timed_out: number;
    error: number;
  };
  retries: {
    totalRetries: number;
    submissionsRetried: number;
    maxRetryCount: number;
  };
  latencyMs: {
    submitted: SubmissionLatencyHistogram;
    rejected: SubmissionLatencyHistogram;
    node_unavailable: SubmissionLatencyHistogram;
    timed_out: SubmissionLatencyHistogram;
    error: SubmissionLatencyHistogram;
  };
  percentilesMs: {
    submitted: { p50: number | null; p95: number | null; p99: number | null };
    rejected: { p50: number | null; p95: number | null; p99: number | null };
    node_unavailable: { p50: number | null; p95: number | null; p99: number | null };
    timed_out: { p50: number | null; p95: number | null; p99: number | null };
    error: { p50: number | null; p95: number | null; p99: number | null };
    schedulerMetrics?: {
      generation_latency: { p50: number | null; p95: number | null; p99: number | null };
      submit_latency: { p50: number | null; p95: number | null; p99: number | null };
    };
  };
  schedulerMetrics?: {
    prepared_queue_depth: {
      current: number;
      max: number;
    };
    in_flight_submits: {
      current: number;
      max: number;
    };
    send_tokens_late_total: number;
    generation_latency: SubmissionLatencyHistogram;
    submit_latency: SubmissionLatencyHistogram;
  };
}

// Generator settings are fully derived from targetTps and txCostSeconds (or submitTimeoutMs).
//
// effectiveTps = targetTps * CLIENT_OVERSEND_RATIO
//
// Standard mode (no submitTimeoutMs):
//   concurrency  = ceil(effectiveTps * txCostSeconds * SUBMISSION_PARALLELISM_HEADROOM)
//   interval     = batchSize / effectiveTps  ≈ HEADROOM * txCostSeconds / OVERSEND_RATIO
//
// Fast-fail mode (submitTimeoutMs provided):
//   effectiveTaskCostSec = (submitTimeoutMs * retryAttempts + retryDelayMs * max(0, retryAttempts-1)) / 1000
//   concurrency  = ceil(targetTps * effectiveTaskCostSec * FAIL_FAST_CONCURRENCY_HEADROOM)
//   interval     = batchSize / effectiveTps
//   Uses targetTps (not effectiveTps) so concurrency is proportional to what the server
//   actually needs to handle, not the oversend rate. For 800 TPS, 500ms timeout:
//   ceil(800 × 0.5 × 1.5) = 600 connections — enough to sustain 800 TPS with headroom
//   but not so many that the OS accept queue or event loop saturates.
//
// CLIENT_OVERSEND_RATIO drives the token bucket above target TPS so scheduling jitter
// does not drop below the configured rate. It is intentionally NOT applied to the
// concurrency calculation in fast-fail mode.
const SUBMISSION_PARALLELISM_HEADROOM = 12;
const FAIL_FAST_CONCURRENCY_HEADROOM = 1.5;
const CLIENT_OVERSEND_RATIO = 1.05;
// Keep generation concurrency well below submission concurrency. Lucid's
// WASM transaction-signing is synchronous-from-Node's-perspective and
// saturates the event loop when too many workers run in parallel. At high
// counts the AbortController timers for the 500ms submit timeout starve and
// fire late, causing all submissions to appear in-flight indefinitely.
// When a pre-generated corpus is used, generation runs before the load phase
// so this constant only matters for ad-hoc (non-corpus) runs.
const GENERATION_TO_SUBMISSION_CONCURRENCY_RATIO = 0.25;
const MAX_GENERATION_CONCURRENCY = 32;
const PREPARED_QUEUE_HEADROOM = 4;

export interface GeneratorSettings {
  intervalSeconds: number;
  batchSize: number;
  concurrency: number;
  maxInFlight: number;
  generationConcurrency: number;
  preparedQueueCapacity: number;
  targetTps: number;
  actualTpsEstimate: number;
}

export interface TxGeneratorResult {
  pid: number | undefined;
  startedAt: string;
  stoppedAt: string;
  exitCode: number | null;
  signal: string | null;
  stderrTail: string | null;
  settings: GeneratorSettings;
  submissionAggregate: SubmissionAggregate | null;
}

export interface TxGeneratorHandle {
  readonly pid: number | undefined;
  readonly settings: GeneratorSettings;
  readonly startedAt: string;
  // Resolves with the exit code when the process exits for any reason.
  // null = exited via signal (e.g. SIGINT from stop()); non-zero = crash.
  readonly processExited: Promise<number | null>;
  stop(): Promise<TxGeneratorResult>;
}

export interface ProcessSpawner {
  spawn(command: string, args: string[], options: SpawnOptions): ChildProcess;
}

export const defaultSpawner: ProcessSpawner = {
  spawn: (cmd, args, opts) => spawn(cmd, args, opts),
};

export interface RunnerOptions {
  spawner?: ProcessSpawner;
  cwd?: string;
  sigintGraceMs?: number;
  sigTermGraceMs?: number;
  requestEvents?: RequestEventsMode;
}

export interface ComputeSettingsSubmitOptions {
  submitTimeoutMs: number;
  retryAttempts: number;
  retryDelayMs: number;
}

export function computeSettings(
  targetTps: number,
  txCostSeconds: number,
  submitOptions?: ComputeSettingsSubmitOptions
): GeneratorSettings {
  const effectiveTps = targetTps * CLIENT_OVERSEND_RATIO;

  let concurrency: number;
  if (submitOptions !== undefined) {
    // Fast-fail mode: derive task cost from the submit timeout chain so that
    // maxInFlight stays proportional to the actual max blocking time per worker,
    // not the (often mismatched) txCostSeconds estimate.
    const { submitTimeoutMs, retryAttempts, retryDelayMs } = submitOptions;
    const maxAttempts = Math.max(1, retryAttempts);
    const effectiveTaskCostMs =
      submitTimeoutMs * maxAttempts + retryDelayMs * Math.max(0, maxAttempts - 1);
    const effectiveTaskCostSec = effectiveTaskCostMs / 1000;
    // Use targetTps (not effectiveTps) so that concurrent connections stay
    // proportional to what the server needs to handle. effectiveTps already
    // has an oversend ratio built in; applying it to the concurrency formula
    // would inflate the connection count without benefit.
    concurrency = Math.max(
      1,
      Math.ceil(targetTps * effectiveTaskCostSec * FAIL_FAST_CONCURRENCY_HEADROOM)
    );
  } else {
    concurrency = Math.max(
      1,
      Math.ceil(effectiveTps * txCostSeconds * SUBMISSION_PARALLELISM_HEADROOM)
    );
  }

  const batchSize = concurrency;
  const maxInFlight = concurrency;
  const generationConcurrency = Math.max(
    1,
    Math.min(
      MAX_GENERATION_CONCURRENCY,
      Math.ceil(maxInFlight * GENERATION_TO_SUBMISSION_CONCURRENCY_RATIO)
    )
  );
  const preparedQueueCapacity = Math.max(1, maxInFlight * PREPARED_QUEUE_HEADROOM);
  const intervalSeconds = batchSize / effectiveTps;
  const actualTpsEstimate = effectiveTps;
  return {
    intervalSeconds,
    batchSize,
    concurrency,
    maxInFlight,
    generationConcurrency,
    preparedQueueCapacity,
    targetTps,
    actualTpsEstimate,
  };
}

function toGeneratorSeed(seed: string): string {
  const normalized = seed.trim();
  if (normalized.length === 0) {
    return '00';
  }
  return Buffer.from(normalized, 'utf8').toString('hex');
}

function buildArgs(
  scenario: ScalabilityScenario,
  tier: LoadTier,
  settings: GeneratorSettings,
  tierArtifactDir: string,
  requestEvents: RequestEventsMode,
  cwd: string
): string[] {
  const generatorSeed = toGeneratorSeed(tier.seed);
  const walletMode = scenario.walletMode ?? 'test-wallet';
  const args = [
    '--filter',
    '@midgard-manager/tx-generator',
    'exec',
    'node',
    TX_GENERATOR_CLI_PATH,
    'start',
    '--endpoint',
    scenario.nodeEndpoint,
    '--type',
    scenario.transactionType,
    '--batch-size',
    String(settings.batchSize),
    '--interval',
    String(settings.intervalSeconds),
    '--concurrency',
    String(settings.concurrency),
    '--max-in-flight',
    String(settings.maxInFlight),
    '--generation-concurrency',
    String(settings.generationConcurrency),
    '--prepared-queue-capacity',
    String(settings.preparedQueueCapacity),
    '--target-tps',
    String(settings.actualTpsEstimate),
    '--retry-attempts',
    String(scenario.retryAttempts),
    '--retry-delay-ms',
    String(scenario.retryDelayMs),
    '--submit-timeout-ms',
    String(scenario.submitTimeoutMs ?? 5000),
    '--request-events',
    requestEvents,
    '--seed',
    generatorSeed,
    '--output-dir',
    tierArtifactDir,
  ];

  if (walletMode === 'test-wallet') {
    args.push('--test-wallet');
  }

  if (scenario.transactionType === 'mixed' && scenario.oneToOneRatio !== undefined) {
    args.push('--ratio', String(scenario.oneToOneRatio));
  }

  if (scenario.localValidation !== undefined) {
    args.push('--local-validation', scenario.localValidation);
  }

  const replayCorpusPath = resolveReplayCorpusPath(scenario.replayCorpusPath, cwd);
  if (replayCorpusPath !== undefined) {
    args.push('--replay-corpus-path', replayCorpusPath);
    if (tier.replayStartIndex !== undefined) {
      args.push('--replay-start-index', String(tier.replayStartIndex));
    }
    if (tier.replayCount !== undefined) {
      args.push('--replay-count', String(tier.replayCount));
    }
  }

  return args;
}

function resolveReplayCorpusPath(
  replayCorpusPath: string | undefined,
  cwd: string
): string | undefined {
  if (replayCorpusPath === undefined) {
    return undefined;
  }
  if (isAbsolute(replayCorpusPath)) {
    return replayCorpusPath;
  }
  return resolve(cwd, replayCorpusPath);
}

async function readSubmissionAggregate(
  tierArtifactDir: string
): Promise<SubmissionAggregate | null> {
  const aggregatePath = path.join(tierArtifactDir, SUBMISSION_AGGREGATES_FILE);
  try {
    const raw = await readFile(aggregatePath, 'utf8');
    return JSON.parse(raw) as SubmissionAggregate;
  } catch {
    return null;
  }
}

function waitForExit(proc: ChildProcess, timeoutMs: number): Promise<boolean> {
  return new Promise((resolve) => {
    let settled = false;

    const finish = (exited: boolean) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      proc.off('exit', onExit);
      proc.off('close', onExit);
      resolve(exited);
    };

    const onExit = () => finish(true);

    const timer = setTimeout(() => finish(false), timeoutMs);

    proc.once('exit', onExit);
    proc.once('close', onExit);
  });
}

export async function stopProcess(
  proc: ChildProcess,
  sigintGraceMs: number = SIGINT_GRACE_MS,
  sigTermGraceMs: number = SIGTERM_GRACE_MS
): Promise<{ exitCode: number | null; signal: string | null }> {
  if (proc.exitCode !== null || proc.killed) {
    return { exitCode: proc.exitCode, signal: proc.signalCode ?? null };
  }

  proc.kill('SIGINT');
  if (await waitForExit(proc, sigintGraceMs)) {
    return { exitCode: proc.exitCode, signal: proc.signalCode ?? null };
  }

  proc.kill('SIGTERM');
  if (await waitForExit(proc, sigTermGraceMs)) {
    return { exitCode: proc.exitCode, signal: proc.signalCode ?? null };
  }

  proc.kill('SIGKILL');
  await waitForExit(proc, sigTermGraceMs);
  return { exitCode: proc.exitCode, signal: proc.signalCode ?? null };
}

export async function startTxGenerator(
  scenario: ScalabilityScenario,
  tier: LoadTier,
  writer: ArtifactWriter,
  tierArtifactDir: string,
  options: RunnerOptions = {}
): Promise<TxGeneratorHandle> {
  const {
    spawner = defaultSpawner,
    cwd = process.cwd(),
    sigintGraceMs = SIGINT_GRACE_MS,
    sigTermGraceMs = SIGTERM_GRACE_MS,
    requestEvents = 'off',
  } = options;

  const settings = computeSettings(
    tier.targetTps,
    scenario.txGeneratorTaskCostSeconds,
    scenario.submitTimeoutMs !== undefined
      ? {
          submitTimeoutMs: scenario.submitTimeoutMs,
          retryAttempts: scenario.retryAttempts,
          retryDelayMs: scenario.retryDelayMs,
        }
      : undefined
  );
  const args = buildArgs(scenario, tier, settings, tierArtifactDir, requestEvents, cwd);
  const startedAt = new Date().toISOString();

  const proc = spawner.spawn('pnpm', args, {
    stdio: ['ignore', 'pipe', 'pipe'],
    cwd,
  });
  const stderrTailLines: string[] = [];
  const appendTailLine = (line: string): void => {
    stderrTailLines.push(line);
    if (stderrTailLines.length > MAX_TAIL_LINES) {
      stderrTailLines.shift();
    }
  };

  if (proc.stdout) {
    const rl = createInterface({ input: proc.stdout, crlfDelay: Infinity });
    rl.on('line', (line) => {
      void writer.logStdout(line);
    });
  }

  if (proc.stderr) {
    const rl = createInterface({ input: proc.stderr, crlfDelay: Infinity });
    rl.on('line', (line) => {
      appendTailLine(line);
      void writer.logStderr(line);
    });
  }

  await writer.appendLoadEvent(
    makeEvent<TxGeneratorStartedEvent>({
      event: 'tx_generator_started',
      runId: scenario.runId,
      tierIndex: tier.tierIndex,
      targetTps: tier.targetTps,
      pid: proc.pid,
    })
  );

  const processExited = new Promise<number | null>((resolve) => {
    proc.once('exit', (code) => resolve(code));
  });

  return {
    pid: proc.pid,
    settings,
    startedAt,
    processExited,

    async stop(): Promise<TxGeneratorResult> {
      const { exitCode, signal } = await stopProcess(proc, sigintGraceMs, sigTermGraceMs);
      const stoppedAt = new Date().toISOString();
      const submissionAggregate = await readSubmissionAggregate(tierArtifactDir);
      const stderrTail = stderrTailLines.length > 0 ? stderrTailLines.join('\n') : null;

      await writer.appendLoadEvent(
        makeEvent<TxGeneratorStoppedEvent>({
          event: 'tx_generator_stopped',
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          exitCode,
          signal,
          ...(exitCode !== null && exitCode !== 0 && stderrTail !== null
            ? { errorSnippet: stderrTail }
            : {}),
        })
      );

      if (submissionAggregate !== null) {
        await writer.appendLoadEvent(
          makeEvent<TxGeneratorSubmissionAggregateEvent>({
            event: 'tx_generator_submission_aggregate',
            runId: scenario.runId,
            tierIndex: tier.tierIndex,
            targetTps: tier.targetTps,
            counters: submissionAggregate.counters,
            retries: submissionAggregate.retries,
            submittedLatencyP95Ms: submissionAggregate.percentilesMs.submitted.p95,
          })
        );
      }

      return {
        pid: proc.pid,
        startedAt,
        stoppedAt,
        exitCode,
        signal,
        stderrTail,
        settings,
        submissionAggregate,
      };
    },
  };
}
