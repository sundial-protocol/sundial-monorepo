import type { ChildProcess, SpawnOptions } from 'node:child_process';
import { EventEmitter } from 'node:events';
import { PassThrough } from 'node:stream';

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import type { LoadTier } from '../../src/config/tiers.js';
import type { ArtifactWriter } from '../../src/evidence/artifacts.js';
import { computeSettings, startTxGenerator, stopProcess } from '../../src/runner/tx-generator.js';

// ---------------------------------------------------------------------------
// Mock ChildProcess
// ---------------------------------------------------------------------------

class MockProcess extends EventEmitter {
  pid = 42000;
  exitCode: number | null = null;
  signalCode: string | null = null;
  killed = false;
  stdout = new PassThrough();
  stderr = new PassThrough();
  readonly killCalls: string[] = [];

  kill(signal?: string): boolean {
    this.killCalls.push(signal ?? 'SIGTERM');
    return true;
  }

  simulateExit(code: number | null, signal: string | null = null): void {
    this.exitCode = code;
    this.signalCode = signal;
    this.killed = signal !== null;
    this.emit('exit', code, signal);
    this.emit('close', code, signal);
  }
}

function makeMockSpawner(proc: MockProcess) {
  const spawnFn = vi.fn((_cmd: string, _args: string[], _opts: SpawnOptions) => {
    return proc as unknown as ChildProcess;
  });
  return { spawn: spawnFn, spawnFn };
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const BASE_SCENARIO: ScalabilityScenario = {
  runId: 'test-run',
  nodeEndpoint: 'http://localhost:3000',
  prometheusEndpoint: 'http://localhost:9090',
  outputDir: 'benchmark-runs',
  seed: 'test-seed',
  transactionType: 'one-to-one',
  tierDurationSeconds: 60,
  recoverySeconds: 30,
  startTps: 100,
  maxTps: 800,
  stepMultiplier: 2,
  batchSize: 50,
  concurrency: 4,
  retryAttempts: 3,
  retryDelayMs: 500,
  stopConditions: {
    maxConsecutiveNodeProbeFailures: 5,
    stopOnPrometheusDown: false,
    stopOnCommitmentFailure: true,
    stopOnMergeFailure: false,
  },
};

const BASE_TIER: LoadTier = {
  tierIndex: 0,
  targetTps: 100,
  durationSeconds: 60,
  recoverySeconds: 30,
  seed: 'test-seed:tier:0:tps:100',
};

function makeWriter() {
  return {
    appendLoadEvent: vi.fn().mockResolvedValue(undefined),
    logStdout: vi.fn().mockResolvedValue(undefined),
    logStderr: vi.fn().mockResolvedValue(undefined),
  } as unknown as ArtifactWriter;
}

// ---------------------------------------------------------------------------
// computeSettings
// ---------------------------------------------------------------------------

describe('computeSettings', () => {
  it('computes intervalSeconds as batchSize / targetTps', () => {
    // 50 / 100 = 0.5
    expect(computeSettings(100, 50, 4).intervalSeconds).toBe(0.5);
  });

  it('clamps intervalSeconds to a minimum of 0.1 s (100 ms floor)', () => {
    // 50 / 5000 = 0.01 → clamp → 0.1
    expect(computeSettings(5000, 50, 4).intervalSeconds).toBe(0.1);
  });

  it('computes actualTpsEstimate as batchSize / intervalSeconds', () => {
    // interval=0.5, so estimate = 50 / 0.5 = 100
    expect(computeSettings(100, 50, 4).actualTpsEstimate).toBe(100);
  });

  it('records actual TPS estimate that may differ from targetTps when the 100 ms floor binds', () => {
    // targetTps=5000, interval clamped to 0.1, so estimate = 50/0.1 = 500 ≠ 5000
    const s = computeSettings(5000, 50, 4);
    expect(s.targetTps).toBe(5000);
    expect(s.actualTpsEstimate).toBe(500);
    expect(s.actualTpsEstimate).not.toBe(s.targetTps);
  });

  it('concurrency does not affect intervalSeconds or actualTpsEstimate', () => {
    // interval is batchSize/targetTps regardless of concurrency
    const s4 = computeSettings(100, 50, 4);
    const s8 = computeSettings(100, 50, 8);
    expect(s4.intervalSeconds).toBe(s8.intervalSeconds);
    expect(s4.actualTpsEstimate).toBe(s8.actualTpsEstimate);
  });

  it('preserves batchSize, concurrency, and targetTps verbatim', () => {
    const s = computeSettings(300, 25, 6);
    expect(s.batchSize).toBe(25);
    expect(s.concurrency).toBe(6);
    expect(s.targetTps).toBe(300);
  });
});

// ---------------------------------------------------------------------------
// startTxGenerator
// ---------------------------------------------------------------------------

describe('startTxGenerator', () => {
  let proc: MockProcess;
  let writer: ArtifactWriter;

  beforeEach(() => {
    proc = new MockProcess();
    writer = makeWriter();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('spawns tx-generator built CLI through pnpm exec node with the node endpoint', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    expect(spawnFn).toHaveBeenCalledOnce();
    const [cmd, args] = spawnFn.mock.calls[0];
    expect(cmd).toBe('pnpm');
    expect(args).toContain('--filter');
    expect(args).toContain('@midgard-manager/tx-generator');
    expect(args).toContain('exec');
    expect(args).toContain('node');
    expect(args).toContain('dist/bin/index.js');
    expect(args).toContain('--endpoint');
    expect(args).toContain('http://localhost:3000');
  });

  it('passes the Commander start subcommand as the next positional after dist CLI path', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    const [, args] = spawnFn.mock.calls[0];
    const distIndex = args.indexOf('dist/bin/index.js');
    expect(distIndex).toBeGreaterThan(-1);
    expect(args[distIndex + 1]).toBe('start');
  });

  it('passes --type, --batch-size, --interval, --concurrency, retry options, --seed, --output-dir, --test-wallet', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    const [, args] = spawnFn.mock.calls[0];
    expect(args).toContain('--type');
    expect(args).toContain('--batch-size');
    expect(args).toContain('--interval');
    expect(args).toContain('--concurrency');
    expect(args).toContain('--retry-attempts');
    expect(args).toContain(String(BASE_SCENARIO.retryAttempts));
    expect(args).toContain('--retry-delay-ms');
    expect(args).toContain(String(BASE_SCENARIO.retryDelayMs));
    expect(args).toContain('--request-events');
    expect(args).toContain('off');
    expect(args).toContain('--seed');
    expect(args).toContain(Buffer.from(BASE_TIER.seed, 'utf8').toString('hex'));
    expect(args).toContain('--output-dir');
    expect(args).toContain('/tmp/tier-0');
    expect(args).toContain('--test-wallet');
  });

  it('passes --ratio when transactionType is mixed', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    const scenario = { ...BASE_SCENARIO, transactionType: 'mixed' as const, oneToOneRatio: 70 };
    await startTxGenerator(scenario, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    const [, args] = spawnFn.mock.calls[0];
    expect(args).toContain('--ratio');
    expect(args).toContain('70');
  });

  it('passes --replay-corpus-path when replayCorpusPath is configured', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    const scenario = {
      ...BASE_SCENARIO,
      replayCorpusPath: 'scenarios/corpora/replay-baseline.json',
    };
    await startTxGenerator(scenario, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
      cwd: '/workdir/demo/midgard-manager/packages/scalability-harness',
    });

    const [, args] = spawnFn.mock.calls[0];
    const replayFlagIndex = args.indexOf('--replay-corpus-path');
    expect(replayFlagIndex).toBeGreaterThan(-1);
    expect(args[replayFlagIndex + 1]).toBe(
      '/workdir/demo/midgard-manager/packages/scalability-harness/scenarios/corpora/replay-baseline.json'
    );
  });

  it('omits --ratio when transactionType is not mixed', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    const [, args] = spawnFn.mock.calls[0];
    expect(args).not.toContain('--ratio');
  });

  it('emits tx_generator_started event with process PID', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    const calls = vi.mocked(writer.appendLoadEvent).mock.calls;
    const startedEvent = calls.find(([e]) => e.event === 'tx_generator_started')?.[0];
    expect(startedEvent).toBeDefined();
    expect(startedEvent?.event).toBe('tx_generator_started');
    if (startedEvent?.event === 'tx_generator_started') {
      expect(startedEvent.pid).toBe(42000);
      expect(startedEvent.tierIndex).toBe(0);
      expect(startedEvent.targetTps).toBe(100);
    }
  });

  it('returns a handle with pid, settings, and startedAt', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
    });

    expect(handle.pid).toBe(42000);
    expect(handle.settings.targetTps).toBe(100);
    expect(handle.startedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });

  it('captures stdout lines via writer.logStdout', async () => {
    const { ...spawner } = makeMockSpawner(proc);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    proc.stdout.push('hello from generator\n');
    proc.stdout.push(null);

    // let readline drain
    await new Promise((r) => setImmediate(r));

    expect(vi.mocked(writer.logStdout)).toHaveBeenCalledWith('hello from generator');
  });

  it('captures stderr lines via writer.logStderr', async () => {
    const { ...spawner } = makeMockSpawner(proc);

    await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', { spawner });

    proc.stderr.push('error output\n');
    proc.stderr.push(null);

    await new Promise((r) => setImmediate(r));

    expect(vi.mocked(writer.logStderr)).toHaveBeenCalledWith('error output');
  });
});

// ---------------------------------------------------------------------------
// stop() on the handle
// ---------------------------------------------------------------------------

describe('TxGeneratorHandle.stop', () => {
  let proc: MockProcess;
  let writer: ArtifactWriter;

  beforeEach(() => {
    proc = new MockProcess();
    writer = makeWriter();
  });

  it('emits tx_generator_stopped event with exit code', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
      sigintGraceMs: 50,
      sigTermGraceMs: 50,
    });

    proc.simulateExit(0);
    await handle.stop();

    const calls = vi.mocked(writer.appendLoadEvent).mock.calls;
    const stoppedEvent = calls.find(([e]) => e.event === 'tx_generator_stopped')?.[0];
    expect(stoppedEvent).toBeDefined();
    if (stoppedEvent?.event === 'tx_generator_stopped') {
      expect(stoppedEvent.exitCode).toBe(0);
      expect(stoppedEvent.tierIndex).toBe(0);
    }
  });

  it('records non-zero exit code in the stopped event without throwing', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
      sigintGraceMs: 50,
      sigTermGraceMs: 50,
    });

    proc.simulateExit(1);
    const result = await handle.stop();

    expect(result.exitCode).toBe(1);

    const calls = vi.mocked(writer.appendLoadEvent).mock.calls;
    const stoppedEvent = calls.find(([e]) => e.event === 'tx_generator_stopped')?.[0];
    if (stoppedEvent?.event === 'tx_generator_stopped') {
      expect(stoppedEvent.exitCode).toBe(1);
    }
  });

  it('returns a result with pid, startedAt, stoppedAt, exitCode, signal, and settings', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
      sigintGraceMs: 50,
      sigTermGraceMs: 50,
    });

    proc.simulateExit(0);
    const result = await handle.stop();

    expect(result.pid).toBe(42000);
    expect(result.startedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    expect(result.stoppedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    expect(result.exitCode).toBe(0);
    expect(result.settings.targetTps).toBe(100);
    expect(result.submissionAggregate).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// stopProcess signal escalation
// ---------------------------------------------------------------------------

describe('stopProcess', () => {
  it('sends SIGINT and resolves immediately when process exits within grace period', async () => {
    const proc = new MockProcess();

    const stopPromise = stopProcess(proc as unknown as ChildProcess, 200, 200);
    proc.simulateExit(0);
    const result = await stopPromise;

    expect(proc.killCalls).toEqual(['SIGINT']);
    expect(result.exitCode).toBe(0);
  });

  it('escalates to SIGTERM when process does not exit within SIGINT grace period', async () => {
    const proc = new MockProcess();

    const stopPromise = stopProcess(proc as unknown as ChildProcess, 50, 200);

    // Let SIGINT grace expire, then exit after SIGTERM
    await new Promise((r) => setTimeout(r, 80));
    proc.simulateExit(null, 'SIGTERM');

    await stopPromise;

    expect(proc.killCalls).toContain('SIGINT');
    expect(proc.killCalls).toContain('SIGTERM');
  });

  it('escalates to SIGKILL when process does not exit within SIGTERM grace period', async () => {
    const proc = new MockProcess();

    const stopPromise = stopProcess(proc as unknown as ChildProcess, 50, 50);

    // Let both grace periods expire, then exit on SIGKILL
    await new Promise((r) => setTimeout(r, 130));
    proc.simulateExit(null, 'SIGKILL');

    await stopPromise;

    expect(proc.killCalls).toContain('SIGINT');
    expect(proc.killCalls).toContain('SIGTERM');
    expect(proc.killCalls).toContain('SIGKILL');
  });

  it('returns without sending any signal if the process has already exited', async () => {
    const proc = new MockProcess();
    proc.exitCode = 0;

    const result = await stopProcess(proc as unknown as ChildProcess, 50, 50);

    expect(proc.killCalls).toHaveLength(0);
    expect(result.exitCode).toBe(0);
  });

  it('returns without sending any signal if the process is already killed', async () => {
    const proc = new MockProcess();
    proc.killed = true;
    proc.signalCode = 'SIGKILL';

    const result = await stopProcess(proc as unknown as ChildProcess, 50, 50);

    expect(proc.killCalls).toHaveLength(0);
    expect(result.signal).toBe('SIGKILL');
  });

  it('records the exit code from the process after SIGKILL', async () => {
    const proc = new MockProcess();

    const stopPromise = stopProcess(proc as unknown as ChildProcess, 50, 50);

    await new Promise((r) => setTimeout(r, 130));
    proc.exitCode = null;
    proc.signalCode = 'SIGKILL';
    proc.simulateExit(null, 'SIGKILL');

    const result = await stopPromise;
    expect(result.signal).toBe('SIGKILL');
  });
});
