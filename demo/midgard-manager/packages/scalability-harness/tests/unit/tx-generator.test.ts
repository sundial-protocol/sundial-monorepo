import type { ChildProcess, SpawnOptions } from 'node:child_process';
import { EventEmitter } from 'node:events';
import { PassThrough } from 'node:stream';

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import type { LoadTier } from '../../src/config/tiers.js';
import type { ArtifactWriter } from '../../src/evidence/artifacts.js';
import type { ComputeSettingsSubmitOptions } from '../../src/runner/tx-generator.js';
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
  outputDir: 'benchmark-runs/tmp',
  seed: 'test-seed',
  transactionType: 'one-to-one',
  tierDurationSeconds: 60,
  recoverySeconds: 30,
  startTps: 100,
  maxTps: 800,
  stepMultiplier: 2,
  txGeneratorTaskCostSeconds: 0.2,
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
  // CLIENT_OVERSEND_RATIO = 1.3: the client targets 130% of scenario TPS to
  // absorb scheduling jitter without falling below the configured target.
  // effectiveTps = targetTps * 1.3
  // concurrency  = ceil(effectiveTps * txCostSeconds * 12)
  // interval     = concurrency / effectiveTps  (≈ 6 * txCostSeconds)
  //
  // Note: IEEE-754 floating-point means ceil(targetTps * 2 * cost * 12) may
  // round up by 1 from the mathematical value.

  it('sets batchSize equal to concurrency', () => {
    const s = computeSettings(100, 0.2);
    expect(s.batchSize).toBe(s.concurrency);
  });

  it('sets concurrency proportional to effectiveTps (targetTps * 1.3)', () => {
    // effectiveTps = 130; concurrency = ceil(130 * 0.2 * 12) = 312
    const s = computeSettings(100, 0.2);
    expect(s.concurrency).toBe(312);
  });

  it('sets actualTpsEstimate to effectiveTps (targetTps * 1.3)', () => {
    expect(computeSettings(100, 0.2).actualTpsEstimate).toBeCloseTo(130);
    expect(computeSettings(800, 0.2).actualTpsEstimate).toBeCloseTo(1040);
  });

  it('preserves targetTps as the scenario-configured value, not the effective rate', () => {
    expect(computeSettings(100, 0.2).targetTps).toBe(100);
    expect(computeSettings(800, 0.2).targetTps).toBe(800);
  });

  it('scales concurrency approximately 4x when targetTps goes from 200 to 800', () => {
    const s200 = computeSettings(200, 0.2);
    const s800 = computeSettings(800, 0.2);
    // Ratio must be close to 4 (exact match not guaranteed due to FP ceiling rounding)
    expect(s800.concurrency / s200.concurrency).toBeCloseTo(4, 1);
  });

  it('interval stays close to 6 * txCostSeconds regardless of targetTps', () => {
    // interval = concurrency / effectiveTps ≈ 6 * txCostSeconds; FP ceiling may
    // add at most 1 to concurrency, so allow ±5% tolerance.
    const s100 = computeSettings(100, 0.2);
    const s800 = computeSettings(800, 0.2);
    expect(s100.intervalSeconds).toBeGreaterThan(2.39);
    expect(s100.intervalSeconds).toBeLessThan(2.41);
    expect(s800.intervalSeconds).toBeGreaterThan(2.39);
    expect(s800.intervalSeconds).toBeLessThan(2.41);
  });

  it('derives maxInFlight, generationConcurrency, and preparedQueueCapacity', () => {
    // Standard mode: concurrency = ceil(1040 * 0.2 * 12) = 2496
    // generationConcurrency = min(32, ceil(2496 * 0.25)) = min(32, 624) = 32
    // preparedQueueCapacity = 2496 * 4 = 9984
    const s = computeSettings(800, 0.2);
    expect(s.maxInFlight).toBe(2496);
    expect(s.generationConcurrency).toBe(32);
    expect(s.preparedQueueCapacity).toBe(9984);
  });

  it('clamps concurrency to at least 1', () => {
    expect(computeSettings(1, 0.001).concurrency).toBe(1);
  });

  describe('fast-fail mode (submitOptions provided)', () => {
    // Fast-fail mode replaces txCostSeconds with the actual max task cost derived from
    // the submit timeout chain and uses FAIL_FAST_CONCURRENCY_HEADROOM=1.5.
    // Uses targetTps (not effectiveTps) so connections scale to server need only.
    // effectiveTaskCostMs = submitTimeoutMs * maxAttempts + retryDelayMs * (maxAttempts-1)
    // concurrency = ceil(targetTps * effectiveTaskCostMs/1000 * 1.5)

    it('uses submitTimeoutMs and retryAttempts to compute concurrency with headroom=1.5', () => {
      // targetTps=800, submitTimeoutMs=500, retryAttempts=1 (no retries):
      // taskCostMs=500, taskCostSec=0.5
      // concurrency = ceil(800 * 0.5 * 1.5) = ceil(600) = 600
      const opts: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 1,
        retryDelayMs: 0,
      };
      const s = computeSettings(800, 0.2, opts);
      expect(s.concurrency).toBe(600);
      expect(s.maxInFlight).toBe(600);
    });

    it('fast-fail concurrency is substantially lower than standard for the same cost', () => {
      // Standard for txCostSeconds=0.5: ceil(1600 * 0.5 * 12) = 9600
      // Fast-fail for submitTimeoutMs=500, retryAttempts=1: ceil(800 * 0.5 * 1.5) = 600
      const standard = computeSettings(800, 0.5);
      const opts: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 1,
        retryDelayMs: 0,
      };
      const fastFail = computeSettings(800, 0.2, opts);
      expect(standard.concurrency).toBeGreaterThan(fastFail.concurrency);
    });

    it('factors in retryAttempts and retryDelayMs in the task cost', () => {
      // submitTimeoutMs=500, retryAttempts=3, retryDelayMs=200:
      // taskCostMs = 500*3 + 200*2 = 1900ms = 1.9s
      // concurrency = ceil(800 * 1.9 * 1.5) = ceil(2280) = 2280
      const opts: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 3,
        retryDelayMs: 200,
      };
      const s = computeSettings(800, 0.2, opts);
      expect(s.concurrency).toBe(2280);
    });

    it('clamps retryAttempts to at least 1 (retryAttempts=0 treated as 1)', () => {
      // retryAttempts=0 → maxAttempts=1 → same as retryAttempts=1
      const opts0: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 0,
        retryDelayMs: 0,
      };
      const opts1: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 1,
        retryDelayMs: 0,
      };
      expect(computeSettings(800, 0.2, opts0).concurrency).toBe(
        computeSettings(800, 0.2, opts1).concurrency
      );
    });

    it('ignores txCostSeconds when submitOptions is provided', () => {
      const opts: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 1,
        retryDelayMs: 0,
      };
      // Different txCostSeconds values should produce the same result
      expect(computeSettings(800, 0.2, opts).concurrency).toBe(
        computeSettings(800, 99, opts).concurrency
      );
    });

    it('preserves targetTps and actualTpsEstimate in fast-fail mode', () => {
      const opts: ComputeSettingsSubmitOptions = {
        submitTimeoutMs: 500,
        retryAttempts: 1,
        retryDelayMs: 0,
      };
      const s = computeSettings(800, 0.2, opts);
      expect(s.targetTps).toBe(800);
      expect(s.actualTpsEstimate).toBeCloseTo(1040);
    });
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
    expect(args).toContain('--max-in-flight');
    expect(args).toContain('--generation-concurrency');
    expect(args).toContain('--prepared-queue-capacity');
    expect(args).toContain('--target-tps');
    expect(args).toContain('--retry-attempts');
    expect(args).toContain(String(BASE_SCENARIO.retryAttempts));
    expect(args).toContain('--retry-delay-ms');
    expect(args).toContain(String(BASE_SCENARIO.retryDelayMs));
    expect(args).toContain('--submit-timeout-ms');
    expect(args).toContain('--request-events');
    expect(args).toContain('off');
    expect(args).toContain('--seed');
    expect(args).toContain(Buffer.from(BASE_TIER.seed, 'utf8').toString('hex'));
    expect(args).toContain('--output-dir');
    expect(args).toContain('/tmp/tier-0');
    expect(args).toContain('--test-wallet');
  });

  it('omits --test-wallet when scenario walletMode is external-key', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(
      {
        ...BASE_SCENARIO,
        walletMode: 'external-key',
      },
      BASE_TIER,
      writer,
      '/tmp/tier-0',
      { spawner }
    );

    const [, args] = spawnFn.mock.calls[0];
    expect(args).not.toContain('--test-wallet');
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

  it('passes --local-validation when configured by the scenario', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    await startTxGenerator(
      { ...BASE_SCENARIO, localValidation: 'warn' },
      BASE_TIER,
      writer,
      '/tmp/tier-0',
      { spawner }
    );

    const [, args] = spawnFn.mock.calls[0];
    const flagIndex = args.indexOf('--local-validation');
    expect(flagIndex).toBeGreaterThan(-1);
    expect(args[flagIndex + 1]).toBe('warn');
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

  it('passes replay window args when tier replay slice is configured', async () => {
    const { spawnFn, ...spawner } = makeMockSpawner(proc);
    proc.simulateExit(0);

    const scenario = {
      ...BASE_SCENARIO,
      replayCorpusPath: 'scenarios/corpora/replay-baseline.json',
    };
    const tierWithSlice: LoadTier = {
      ...BASE_TIER,
      replayStartIndex: 375_000,
      replayCount: 375_000,
    };
    await startTxGenerator(scenario, tierWithSlice, writer, '/tmp/tier-0', {
      spawner,
      cwd: '/workdir/demo/midgard-manager/packages/scalability-harness',
    });

    const [, args] = spawnFn.mock.calls[0];
    expect(args).toContain('--replay-start-index');
    expect(args).toContain('375000');
    expect(args).toContain('--replay-count');
    expect(args).toContain('375000');
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
// TxGeneratorHandle.processExited
// ---------------------------------------------------------------------------

describe('TxGeneratorHandle.processExited', () => {
  let proc: MockProcess;
  let writer: ArtifactWriter;

  beforeEach(() => {
    proc = new MockProcess();
    writer = makeWriter();
  });

  it('resolves with exit code 0 on clean exit', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
    });

    proc.simulateExit(0);
    await expect(handle.processExited).resolves.toBe(0);
  });

  it('resolves with non-zero code when generator crashes', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
    });

    proc.simulateExit(1);
    await expect(handle.processExited).resolves.toBe(1);
  });

  it('resolves with null when process exits via signal', async () => {
    const { ...spawner } = makeMockSpawner(proc);
    const handle = await startTxGenerator(BASE_SCENARIO, BASE_TIER, writer, '/tmp/tier-0', {
      spawner,
    });

    proc.simulateExit(null, 'SIGINT');
    await expect(handle.processExited).resolves.toBeNull();
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
