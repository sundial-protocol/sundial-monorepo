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
const SUBMISSION_AGGREGATES_FILE = 'submission-aggregates.json';

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

// Deliberately coarse: the tx-generator is batch/interval based, not exact-TPS based.
// intervalSeconds is derived from batchSize, concurrency, and targetTps but is a floor
// approximation that may under-drive at high TPS targets.
export interface GeneratorSettings {
  intervalSeconds: number;
  batchSize: number;
  concurrency: number;
  targetTps: number;
  // What the generator will actually deliver at this interval — may differ from targetTps.
  actualTpsEstimate: number;
}

export interface TxGeneratorResult {
  pid: number | undefined;
  startedAt: string;
  stoppedAt: string;
  exitCode: number | null;
  signal: string | null;
  settings: GeneratorSettings;
  submissionAggregate: SubmissionAggregate | null;
}

export interface TxGeneratorHandle {
  readonly pid: number | undefined;
  readonly settings: GeneratorSettings;
  readonly startedAt: string;
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

export function computeSettings(
  targetTps: number,
  batchSize: number,
  concurrency: number
): GeneratorSettings {
  const intervalSeconds = Math.max(1, Math.floor((batchSize * concurrency) / targetTps));
  const actualTpsEstimate = (batchSize * concurrency) / intervalSeconds;
  return { intervalSeconds, batchSize, concurrency, targetTps, actualTpsEstimate };
}

function buildArgs(
  scenario: ScalabilityScenario,
  tier: LoadTier,
  settings: GeneratorSettings,
  tierArtifactDir: string,
  requestEvents: RequestEventsMode,
  cwd: string
): string[] {
  const args = [
    '--filter',
    '@midgard-manager/tx-generator',
    'exec',
    'tsx',
    'src/bin/index.ts',
    '--',
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
    tier.seed,
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

  const settings = computeSettings(tier.targetTps, scenario.batchSize, scenario.concurrency);
  const args = buildArgs(scenario, tier, settings, tierArtifactDir, requestEvents, cwd);
  const startedAt = new Date().toISOString();

  const proc = spawner.spawn('pnpm', args, {
    stdio: ['ignore', 'pipe', 'pipe'],
    cwd,
  });

  if (proc.stdout) {
    const rl = createInterface({ input: proc.stdout, crlfDelay: Infinity });
    rl.on('line', (line) => {
      void writer.logStdout(line);
    });
  }

  if (proc.stderr) {
    const rl = createInterface({ input: proc.stderr, crlfDelay: Infinity });
    rl.on('line', (line) => {
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

  return {
    pid: proc.pid,
    settings,
    startedAt,

    async stop(): Promise<TxGeneratorResult> {
      const { exitCode, signal } = await stopProcess(proc, sigintGraceMs, sigTermGraceMs);
      const stoppedAt = new Date().toISOString();
      const submissionAggregate = await readSubmissionAggregate(tierArtifactDir);

      await writer.appendLoadEvent(
        makeEvent<TxGeneratorStoppedEvent>({
          event: 'tx_generator_stopped',
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          exitCode,
          signal,
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
        settings,
        submissionAggregate,
      };
    },
  };
}
