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
  };
}

// Generator settings are fully derived from targetTps and txCostSeconds.
//
// effectiveTps = targetTps * CLIENT_OVERSEND_RATIO
// concurrency  = ceil(effectiveTps * txCostSeconds * PARALLELISM_HEADROOM)
// batchSize    = concurrency   (one parallel wave per batch)
// interval     = batchSize / effectiveTps
//
// With PARALLELISM_HEADROOM=2 the interval is always 2*txCostSeconds regardless
// of effectiveTps, so batch execution time (~txCostSeconds, all tasks in
// parallel) is well inside the interval and the wall-clock scheduler fires
// exactly on time. Scaling TPS only changes how many concurrent workers run
// per batch.
//
// CLIENT_OVERSEND_RATIO drives the client slightly above the scenario target so
// that measurement overhead, startup latency, and scheduling jitter do not
// cause the effective send rate to fall below the target.
const PARALLELISM_HEADROOM = 2;
const CLIENT_OVERSEND_RATIO = 1.1;

export interface GeneratorSettings {
  intervalSeconds: number;
  batchSize: number;
  concurrency: number;
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

export function computeSettings(targetTps: number, txCostSeconds: number): GeneratorSettings {
  const effectiveTps = targetTps * CLIENT_OVERSEND_RATIO;
  const concurrency = Math.max(1, Math.ceil(effectiveTps * txCostSeconds * PARALLELISM_HEADROOM));
  const batchSize = concurrency;
  const intervalSeconds = batchSize / effectiveTps;
  const actualTpsEstimate = effectiveTps;
  return { intervalSeconds, batchSize, concurrency, targetTps, actualTpsEstimate };
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
    '--retry-attempts',
    String(scenario.retryAttempts),
    '--retry-delay-ms',
    String(scenario.retryDelayMs),
    '--request-events',
    requestEvents,
    '--seed',
    generatorSeed,
    '--output-dir',
    tierArtifactDir,
    '--test-wallet',
  ];

  if (scenario.transactionType === 'mixed' && scenario.oneToOneRatio !== undefined) {
    args.push('--ratio', String(scenario.oneToOneRatio));
  }

  const replayCorpusPath = resolveReplayCorpusPath(scenario.replayCorpusPath, cwd);
  if (replayCorpusPath !== undefined) {
    args.push('--replay-corpus-path', replayCorpusPath);
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

  const settings = computeSettings(tier.targetTps, scenario.txGeneratorTaskCostSeconds);
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
