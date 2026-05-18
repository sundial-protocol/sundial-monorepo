import type { ChildProcess, SpawnOptions } from 'node:child_process';
import { EventEmitter } from 'node:events';
import { PassThrough } from 'node:stream';

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import type { LoadTier } from '../../src/config/tiers.js';
import type { ArtifactWriter } from '../../src/evidence/artifacts.js';
import type { PrometheusClient } from '../../src/metrics/prometheus.js';
import { NODE_METRICS } from '../../src/metrics/prometheus.js';
import type { TierMetricWindow, TierWindowSummary } from '../../src/metrics/window.js';
import type {
  HostResourceCollector,
  HostResourceSnapshot,
} from '../../src/runner/host-resources.js';
import type { TierRunOptions } from '../../src/runner/load-runner.js';
import { checkMetricStopConditions, runTier } from '../../src/runner/load-runner.js';

// ---------------------------------------------------------------------------
// Module mock: silence real mkdir calls
// ---------------------------------------------------------------------------

vi.mock('node:fs/promises', () => ({
  mkdir: vi.fn().mockResolvedValue(undefined),
}));

// ---------------------------------------------------------------------------
// MockProcess: auto-exits when kill() is called so stop() doesn't hang
// ---------------------------------------------------------------------------

class MockProcess extends EventEmitter {
  pid = 99001;
  exitCode: number | null = null;
  signalCode: string | null = null;
  killed = false;
  stdout = new PassThrough();
  stderr = new PassThrough();

  kill(signal?: string): boolean {
    if (this.exitCode !== null || this.killed) return false;
    this.exitCode = 0;
    this.signalCode = signal ?? 'SIGTERM';
    this.killed = true;
    setImmediate(() => {
      this.emit('exit', this.exitCode, this.signalCode);
      this.emit('close', this.exitCode, this.signalCode);
    });
    return true;
  }
}

function makeMockSpawner() {
  const proc = new MockProcess();
  const spawner = {
    spawn: vi.fn((_cmd: string, _args: string[], _opts: SpawnOptions) => {
      return proc as unknown as ChildProcess;
    }),
  };
  return { proc, spawner };
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const BASE_SCENARIO: ScalabilityScenario = {
  runId: 'test-run',
  nodeEndpoint: 'http://localhost:3000',
  prometheusEndpoint: 'http://localhost:9090',
  outputDir: '/tmp/bench',
  seed: 'test-seed',
  transactionType: 'one-to-one',
  tierDurationSeconds: 60,
  recoverySeconds: 30,
  startTps: 100,
  maxTps: 200,
  stepMultiplier: 2,
  batchSize: 50,
  concurrency: 4,
  retryAttempts: 3,
  retryDelayMs: 500,
  stopConditions: {
    maxConsecutiveNodeProbeFailures: 3,
    stopOnPrometheusDown: false,
    stopOnCommitmentFailure: false,
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

function makeWriter(): ArtifactWriter {
  return {
    runDir: '/test-run',
    appendLoadEvent: vi.fn().mockResolvedValue(undefined),
    appendTierSummary: vi.fn().mockResolvedValue(undefined),
    logStdout: vi.fn().mockResolvedValue(undefined),
    logStderr: vi.fn().mockResolvedValue(undefined),
  } as unknown as ArtifactWriter;
}

function makePrometheusClient(): PrometheusClient {
  return {
    queryInstant: vi.fn().mockResolvedValue([]),
    queryRange: vi.fn().mockResolvedValue([]),
  } as unknown as PrometheusClient;
}

function makeEmptyWindow(overrides?: Partial<TierMetricWindow>): TierMetricWindow {
  return {
    tierIndex: 0,
    targetTps: 100,
    startedAt: new Date().toISOString(),
    stoppedAt: new Date().toISOString(),
    recoveryStartedAt: new Date().toISOString(),
    recoveryStoppedAt: new Date().toISOString(),
    before: {},
    afterLoad: {},
    afterRecovery: {},
    ranges: {},
    ...overrides,
  };
}

function makeWindowSummary(overrides?: Partial<TierWindowSummary>): TierWindowSummary {
  return {
    counterDeltas: [],
    gaugeSummaries: [],
    ...overrides,
  };
}

function makePrimaryMetricSnapshot(value: number): Record<string, number | null> {
  const snapshot: Record<string, number | null> = {};
  for (const metric of NODE_METRICS) {
    snapshot[metric] = value;
  }
  return snapshot;
}

function makeHostSnapshot(capturedAtMs: number): HostResourceSnapshot {
  return {
    capturedAt: new Date(capturedAtMs).toISOString(),
    capturedAtMs,
    systemCpuTotalMs: capturedAtMs + 1_000,
    systemCpuIdleMs: capturedAtMs + 600,
    processCpuUserMicros: capturedAtMs * 10,
    processCpuSystemMicros: capturedAtMs * 3,
    processCpuTotalMicros: capturedAtMs * 13,
    processRssBytes: 128_000_000,
    processHeapUsedBytes: 64_000_000,
    processHeapTotalBytes: 96_000_000,
    systemTotalMemoryBytes: 1_000_000_000,
    systemFreeMemoryBytes: 500_000_000,
    eventLoopLagP95Ms: 5,
    eventLoopLagMaxMs: 10,
    eventLoopLagMeanMs: 4,
    networkRxBytes: 1_000_000 + capturedAtMs,
    networkTxBytes: 2_000_000 + capturedAtMs,
  };
}

function makeHostCollector(): HostResourceCollector {
  const snapshots = [makeHostSnapshot(1_000), makeHostSnapshot(61_000), makeHostSnapshot(91_000)];
  let index = 0;
  return {
    captureSnapshot: vi.fn(async () => {
      const current = snapshots[Math.min(index, snapshots.length - 1)];
      index += 1;
      return current;
    }),
    resetEventLoopLag: vi.fn(),
    close: vi.fn(),
  };
}

// Fast options for runTier — short durations and fast probe intervals
function makeOptions(
  overrides: Partial<TierRunOptions> = {}
): TierRunOptions & { spawner: ReturnType<typeof makeMockSpawner>['spawner'] } {
  const { spawner } = makeMockSpawner();
  const collector = makeHostCollector();
  return {
    probeFetcher: vi.fn().mockResolvedValue({ status: 404 } as Response),
    probeIntervalMs: 10,
    probeTimeoutMs: 100,
    tierDurationMs: 50,
    recoveryDurationMs: 50,
    collectWindowFn: vi.fn().mockResolvedValue(makeEmptyWindow()),
    hostResourceCollectorFactory: () => collector,
    runnerOptions: { spawner, sigintGraceMs: 50, sigTermGraceMs: 50 },
    spawner,
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// checkMetricStopConditions — pure function tests
// ---------------------------------------------------------------------------

describe('checkMetricStopConditions', () => {
  const BASE_SC = BASE_SCENARIO.stopConditions;
  const emptyWindow = makeEmptyWindow();
  const emptyWindowSummary = makeWindowSummary();

  // ---- stopOnPrometheusDown ------------------------------------------------

  it('returns null when stopOnPrometheusDown is false even when up metric is 0', () => {
    const window = makeEmptyWindow({ afterLoad: { 'up{job="midgard_nodes"}': 0 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnPrometheusDown: false },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns prometheus_down when up metric is 0 and stopOnPrometheusDown is true', () => {
    const window = makeEmptyWindow({ afterLoad: { 'up{job="midgard_nodes"}': 0 } });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnPrometheusDown: true },
      window,
      emptyWindowSummary,
      60,
      100
    );
    expect(result?.reason).toBe('prometheus_down');
  });

  it('returns prometheus_down when up metric is null (Prometheus unreachable)', () => {
    const window = makeEmptyWindow({ afterLoad: {} });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnPrometheusDown: true },
      window,
      emptyWindowSummary,
      60,
      100
    );
    expect(result?.reason).toBe('prometheus_down');
  });

  it('returns null when up metric is 1 and stopOnPrometheusDown is true', () => {
    const window = makeEmptyWindow({ afterLoad: { 'up{job="midgard_nodes"}': 1 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnPrometheusDown: true },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  it('includes the up metric value in metricValues for prometheus_down', () => {
    const window = makeEmptyWindow({ afterLoad: { 'up{job="midgard_nodes"}': 0 } });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnPrometheusDown: true },
      window,
      emptyWindowSummary,
      60,
      100
    );
    expect(result?.metricValues).toMatchObject({ 'up{job="midgard_nodes"}': 0 });
  });

  // ---- stopOnCommitmentFailure ---------------------------------------------

  it('returns null when stopOnCommitmentFailure is false even with positive delta', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'commit_block_commitment_failures_total', deltaLoad: 2, deltaRecovery: 2 },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnCommitmentFailure: false },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns commitment_failure when deltaLoad > 0', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'commit_block_commitment_failures_total', deltaLoad: 3, deltaRecovery: 3 },
      ],
    });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnCommitmentFailure: true },
      emptyWindow,
      summary,
      60,
      100
    );
    expect(result?.reason).toBe('commitment_failure');
    expect(result?.metricValues?.commit_block_commitment_failures_total).toBe(3);
  });

  it('returns null when commitment failure deltaLoad is 0', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'commit_block_commitment_failures_total', deltaLoad: 0, deltaRecovery: 0 },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnCommitmentFailure: true },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns null when commitment failure deltaLoad is null', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'commit_block_commitment_failures_total', deltaLoad: null, deltaRecovery: null },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnCommitmentFailure: true },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  // ---- stopOnMergeFailure --------------------------------------------------

  it('returns null when stopOnMergeFailure is false even with positive delta', () => {
    const summary = makeWindowSummary({
      counterDeltas: [{ query: 'merge_block_failures_total', deltaLoad: 1, deltaRecovery: 1 }],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnMergeFailure: false },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns merge_failure when deltaLoad > 0', () => {
    const summary = makeWindowSummary({
      counterDeltas: [{ query: 'merge_block_failures_total', deltaLoad: 1, deltaRecovery: 1 }],
    });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnMergeFailure: true },
      emptyWindow,
      summary,
      60,
      100
    );
    expect(result?.reason).toBe('merge_failure');
  });

  it('returns null when merge failure deltaLoad is 0', () => {
    const summary = makeWindowSummary({
      counterDeltas: [{ query: 'merge_block_failures_total', deltaLoad: 0, deltaRecovery: 0 }],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnMergeFailure: true },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns null when merge failure deltaLoad is null', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'merge_block_failures_total', deltaLoad: null, deltaRecovery: null },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, stopOnMergeFailure: true },
        emptyWindow,
        summary,
        60,
        100
      )
    ).toBeNull();
  });

  // ---- maxRecoveryQueueSize ------------------------------------------------

  it('returns null when maxRecoveryQueueSize is not set', () => {
    const window = makeEmptyWindow({ afterRecovery: { tx_queue_size: 99999 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, maxRecoveryQueueSize: undefined },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns recovery_queue_exceeded when tx_queue_size exceeds threshold', () => {
    const window = makeEmptyWindow({ afterRecovery: { tx_queue_size: 15000 } });
    const result = checkMetricStopConditions(
      { ...BASE_SC, maxRecoveryQueueSize: 10000 },
      window,
      emptyWindowSummary,
      60,
      100
    );
    expect(result?.reason).toBe('recovery_queue_exceeded');
    expect(result?.metricValues?.tx_queue_size).toBe(15000);
  });

  it('returns null when tx_queue_size is at or below threshold', () => {
    const window = makeEmptyWindow({ afterRecovery: { tx_queue_size: 10000 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, maxRecoveryQueueSize: 10000 },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns null when tx_queue_size is absent (null)', () => {
    const window = makeEmptyWindow({ afterRecovery: {} });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, maxRecoveryQueueSize: 10000 },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  // ---- maxRecoveryMempoolSize ----------------------------------------------

  it('returns null when maxRecoveryMempoolSize is not set', () => {
    const window = makeEmptyWindow({ afterRecovery: { mempool_tx_count: 99999 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, maxRecoveryMempoolSize: undefined },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  it('returns recovery_mempool_exceeded when mempool_tx_count exceeds threshold', () => {
    const window = makeEmptyWindow({ afterRecovery: { mempool_tx_count: 6000 } });
    const result = checkMetricStopConditions(
      { ...BASE_SC, maxRecoveryMempoolSize: 5000 },
      window,
      emptyWindowSummary,
      60,
      100
    );
    expect(result?.reason).toBe('recovery_mempool_exceeded');
    expect(result?.metricValues?.mempool_tx_count).toBe(6000);
  });

  it('returns null when mempool_tx_count is at threshold', () => {
    const window = makeEmptyWindow({ afterRecovery: { mempool_tx_count: 5000 } });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, maxRecoveryMempoolSize: 5000 },
        window,
        emptyWindowSummary,
        60,
        100
      )
    ).toBeNull();
  });

  // ---- minUsefulThroughputRatio --------------------------------------------

  it('returns null when minUsefulThroughputRatio is not set', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: 100, deltaRecovery: 100 },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, minUsefulThroughputRatio: undefined },
        emptyWindow,
        summary,
        60,
        1
      )
    ).toBeNull();
  });

  it('returns throughput_below_minimum when ratio is below threshold', () => {
    // observedTps = 18/60 = 0.3, targetTps = 1, usefulThroughputRatio = 0.3 < 0.5
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: 18, deltaRecovery: 18 },
      ],
    });
    const result = checkMetricStopConditions(
      { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
      emptyWindow,
      summary,
      60,
      1
    );
    expect(result?.reason).toBe('throughput_below_minimum');
    expect(result?.metricValues?.usefulThroughputRatio as number).toBeCloseTo(0.3);
  });

  it('returns null when ratio meets or exceeds threshold', () => {
    // observedTps = 36/60 = 0.6, targetTps = 1, usefulThroughputRatio = 0.6 >= 0.5
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: 36, deltaRecovery: 36 },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
        emptyWindow,
        summary,
        60,
        1
      )
    ).toBeNull();
  });

  it('returns null when mempoolAcceptedDelta is null (no data)', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: null, deltaRecovery: null },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
        emptyWindow,
        summary,
        60,
        1
      )
    ).toBeNull();
  });

  it('returns null when tierDurationSeconds is 0 (avoid division by zero)', () => {
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: 0, deltaRecovery: 0 },
      ],
    });
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
        emptyWindow,
        summary,
        0,
        1
      )
    ).toBeNull();
  });

  it('returns null when tx_submissions_mempool_accepted_total is absent from deltas', () => {
    expect(
      checkMetricStopConditions(
        { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
        emptyWindow,
        emptyWindowSummary,
        60,
        1
      )
    ).toBeNull();
  });

  it('includes all throughput supporting values in metricValues', () => {
    // observedTps = 18/60 = 0.3, targetTps = 1, ratio = 0.3 < 0.5
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'tx_submissions_mempool_accepted_total', deltaLoad: 18, deltaRecovery: 18 },
      ],
    });
    const result = checkMetricStopConditions(
      { ...BASE_SC, minUsefulThroughputRatio: 0.5 },
      emptyWindow,
      summary,
      60,
      1
    );
    expect(result?.metricValues?.mempoolAcceptedDelta).toBe(18);
    expect(result?.metricValues?.tierDurationSeconds).toBe(60);
    expect(result?.metricValues?.observedMempoolAcceptedTps as number).toBeCloseTo(0.3);
    expect(result?.metricValues?.targetTps).toBe(1);
    expect(result?.metricValues?.minUsefulThroughputRatio).toBe(0.5);
  });

  // ---- priority ordering ---------------------------------------------------

  it('returns prometheus_down before checking commitment_failure', () => {
    const window = makeEmptyWindow({ afterLoad: { 'up{job="midgard_nodes"}': 0 } });
    const summary = makeWindowSummary({
      counterDeltas: [
        { query: 'commit_block_commitment_failures_total', deltaLoad: 5, deltaRecovery: 5 },
      ],
    });
    const result = checkMetricStopConditions(
      { ...BASE_SC, stopOnPrometheusDown: true, stopOnCommitmentFailure: true },
      window,
      summary,
      60,
      100
    );
    expect(result?.reason).toBe('prometheus_down');
  });
});

// ---------------------------------------------------------------------------
// runTier — orchestration tests
// ---------------------------------------------------------------------------

describe('runTier', () => {
  let writer: ArtifactWriter;
  let prometheusClient: PrometheusClient;

  beforeEach(() => {
    writer = makeWriter();
    prometheusClient = makePrometheusClient();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('emits tier_started event with correct fields', async () => {
    const { runnerOptions } = makeOptions();
    const opts = makeOptions({ runnerOptions });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const started = events.find((e) => e.event === 'tier_started');
    expect(started).toBeDefined();
    if (started?.event === 'tier_started') {
      expect(started.tierIndex).toBe(0);
      expect(started.targetTps).toBe(100);
      expect(started.runId).toBe('test-run');
      expect(started.durationSeconds).toBe(60);
      expect(started.seed).toBe(BASE_TIER.seed);
    }
  });

  it('emits tier_stopped event after the load phase', async () => {
    const opts = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const stopped = events.find((e) => e.event === 'tier_stopped');
    expect(stopped).toBeDefined();
    if (stopped?.event === 'tier_stopped') {
      expect(stopped.tierIndex).toBe(0);
      expect(stopped.targetTps).toBe(100);
      expect(stopped.elapsedMs).toBeGreaterThanOrEqual(0);
    }
  });

  it('tier_stopped reason is completed when no probe stop condition fires', async () => {
    const opts = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const stopped = events.find((e) => e.event === 'tier_stopped');
    if (stopped?.event === 'tier_stopped') {
      expect(stopped.reason).toBe('completed');
    }
  });

  it('tier_stopped reason is stop_condition when load probe fires', async () => {
    const opts = makeOptions({
      probeFetcher: vi.fn().mockResolvedValue({ status: 503 } as Response),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const stopped = events.find((e) => e.event === 'tier_stopped');
    if (stopped?.event === 'tier_stopped') {
      expect(stopped.reason).toBe('stop_condition');
    }
  });

  it('spawns the tx-generator and stops it', async () => {
    const { spawner, ...rest } = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, {
      ...rest,
      runnerOptions: { spawner, sigintGraceMs: 50, sigTermGraceMs: 50 },
    });

    expect(spawner.spawn).toHaveBeenCalledOnce();
    // Generator was stopped (emitted tx_generator_stopped event)
    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    expect(events.some((e) => e.event === 'tx_generator_stopped')).toBe(true);
  });

  it('runs probe loop during load phase', async () => {
    const probeFetcher = vi.fn().mockResolvedValue({ status: 404 } as Response);
    const opts = makeOptions({ probeFetcher });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const probeEvents = events.filter((e) => e.event === 'node_probe');
    expect(probeEvents.length).toBeGreaterThanOrEqual(1);
  });

  it('runs recovery probe loop after the load phase', async () => {
    // Probe is healthy — load phase completes normally, then recovery runs
    const probeFetcher = vi.fn().mockResolvedValue({ status: 404 } as Response);
    const opts = makeOptions({ probeFetcher, tierDurationMs: 30, recoveryDurationMs: 50 });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    // Both load and recovery produce probe events
    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const probeEvents = events.filter((e) => e.event === 'node_probe');
    // We expect probes from both phases (at 10ms interval across 30ms + 50ms)
    expect(probeEvents.length).toBeGreaterThanOrEqual(2);
  });

  it('calls collectWindowFn after recovery completes', async () => {
    const collectWindowFn = vi.fn().mockResolvedValue(makeEmptyWindow());
    const opts = makeOptions({ collectWindowFn });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(collectWindowFn).toHaveBeenCalledOnce();
    const [, tierIdx, targetTps] = collectWindowFn.mock.calls[0];
    expect(tierIdx).toBe(0);
    expect(targetTps).toBe(100);
  });

  it('emits one prometheus_snapshot event per capture point', async () => {
    const window = makeEmptyWindow({
      before: makePrimaryMetricSnapshot(1),
      afterLoad: makePrimaryMetricSnapshot(2),
      afterRecovery: makePrimaryMetricSnapshot(3),
    });
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockResolvedValue(window),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const snapshotEvents = events.filter((e) => e.event === 'prometheus_snapshot');
    expect(snapshotEvents).toHaveLength(3);
    if (
      snapshotEvents[0]?.event === 'prometheus_snapshot' &&
      snapshotEvents[1]?.event === 'prometheus_snapshot' &&
      snapshotEvents[2]?.event === 'prometheus_snapshot'
    ) {
      expect(snapshotEvents[0].capture).toBe('before');
      expect(snapshotEvents[1].capture).toBe('after_load');
      expect(snapshotEvents[2].capture).toBe('after_recovery');
      expect(snapshotEvents[0].ok).toBe(true);
      expect(snapshotEvents[1].ok).toBe(true);
      expect(snapshotEvents[2].ok).toBe(true);
    }
  });

  it('marks prometheus_snapshot capture as failed when primary metrics are missing', async () => {
    const window = makeEmptyWindow({
      before: makePrimaryMetricSnapshot(1),
      afterLoad: {
        tx_queue_size: 42,
      },
      afterRecovery: makePrimaryMetricSnapshot(3),
    });
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockResolvedValue(window),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const afterLoadSnapshot = events.find(
      (e) => e.event === 'prometheus_snapshot' && e.capture === 'after_load'
    );
    expect(afterLoadSnapshot).toBeDefined();
    if (afterLoadSnapshot?.event === 'prometheus_snapshot') {
      expect(afterLoadSnapshot.ok).toBe(false);
      expect(afterLoadSnapshot.metrics.tx_queue_size).toBe(42);
      expect(afterLoadSnapshot.errorMessage).toContain('Missing primary metrics for after_load');
    }
  });

  it('writes tier summary via appendTierSummary', async () => {
    const opts = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(vi.mocked(writer.appendTierSummary)).toHaveBeenCalledOnce();
    const [summary] = vi.mocked(writer.appendTierSummary).mock.calls[0];
    expect(summary.tierIndex).toBe(0);
    expect(summary.targetTps).toBe(100);
  });

  it('includes load-driver resource evidence in tier summary', async () => {
    const opts = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const [summary] = vi.mocked(writer.appendTierSummary).mock.calls[0];
    expect(summary.loadDriverResourceEvidence).toBeDefined();
    expect(summary.loadDriverResourceEvidence).not.toBeNull();
  });

  it('writes tier summary even when probe stop condition fires', async () => {
    const opts = makeOptions({
      probeFetcher: vi.fn().mockResolvedValue({ status: 503 } as Response),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(vi.mocked(writer.appendTierSummary)).toHaveBeenCalledOnce();
  });

  it('returns shouldContinue: true when no stop conditions fire', async () => {
    const opts = makeOptions();

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.shouldContinue).toBe(true);
    expect(result.stopConditionTriggered).toBe(false);
    expect(result.stopReason).toBe('completed');
  });

  it('returns shouldContinue: false when load probe fires stop condition', async () => {
    const opts = makeOptions({
      probeFetcher: vi.fn().mockResolvedValue({ status: 503 } as Response),
    });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.shouldContinue).toBe(false);
    expect(result.stopConditionTriggered).toBe(true);
    expect(result.probeResult.stopConditionTriggered).toBe(true);
  });

  it('returns shouldContinue: false when recovery probe fires stop condition', async () => {
    let calls = 0;
    // Healthy during load phase, fails during recovery
    const probeFetcher = vi.fn().mockImplementation(async () => {
      calls++;
      // First few calls are during load phase (healthy), rest fail
      if (calls <= 3) return { status: 404 } as Response;
      return { status: 503 } as Response;
    });

    const opts = makeOptions({
      probeFetcher,
      tierDurationMs: 40,
      recoveryDurationMs: 100,
    });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.shouldContinue).toBe(false);
    expect(result.recoveryProbeResult.stopConditionTriggered).toBe(true);
  });

  it('returns shouldContinue: false when metric stop condition fires', async () => {
    const window = makeEmptyWindow({
      afterRecovery: { mempool_tx_count: 9000 },
    });
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockResolvedValue(window),
    });
    const scenario = {
      ...BASE_SCENARIO,
      stopConditions: {
        ...BASE_SCENARIO.stopConditions,
        maxRecoveryMempoolSize: 5000,
      },
    };

    const result = await runTier(scenario, BASE_TIER, writer, prometheusClient, opts);

    expect(result.shouldContinue).toBe(false);
    expect(result.metricStopCondition?.reason).toBe('recovery_mempool_exceeded');
  });

  it('emits stop_condition event when metric stop condition fires', async () => {
    const window = makeEmptyWindow({
      afterRecovery: { tx_queue_size: 20000 },
    });
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockResolvedValue(window),
    });
    const scenario = {
      ...BASE_SCENARIO,
      stopConditions: {
        ...BASE_SCENARIO.stopConditions,
        maxRecoveryQueueSize: 10000,
      },
    };

    await runTier(scenario, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const stopEvent = events.find((e) => e.event === 'stop_condition');
    expect(stopEvent).toBeDefined();
    if (stopEvent?.event === 'stop_condition') {
      expect(stopEvent.reason).toBe('recovery_queue_exceeded');
      expect(stopEvent.tierIndex).toBe(0);
    }
  });

  it('does not emit stop_condition event when no metric stop condition fires', async () => {
    const opts = makeOptions();

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    // Only stop_condition events from the probe loop or metric checks
    const metricStopEvents = events.filter((e) => e.event === 'stop_condition');
    expect(metricStopEvents).toHaveLength(0);
  });

  it('emits harness_error when collectWindowFn throws', async () => {
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockRejectedValue(new Error('prometheus network error')),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const errEvent = events.find((e) => e.event === 'harness_error');
    expect(errEvent).toBeDefined();
    if (errEvent?.event === 'harness_error') {
      expect(errEvent.errorMessage).toContain('prometheus network error');
    }
  });

  it('sets evidenceIncomplete true when collectWindowFn throws', async () => {
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockRejectedValue(new Error('fail')),
    });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.evidenceIncomplete).toBe(true);
  });

  it('still writes tier summary when collectWindowFn throws', async () => {
    const opts = makeOptions({
      collectWindowFn: vi.fn().mockRejectedValue(new Error('fail')),
    });

    await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(vi.mocked(writer.appendTierSummary)).toHaveBeenCalledOnce();
  });

  it('sets evidenceIncomplete true when node metrics are absent in afterLoad', async () => {
    // afterLoad has no entries → all NODE_METRICS are null → evidenceIncomplete
    const window = makeEmptyWindow({ afterLoad: {} });
    const opts = makeOptions({ collectWindowFn: vi.fn().mockResolvedValue(window) });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.evidenceIncomplete).toBe(true);
  });

  it('returns accurate elapsedMs and totalElapsedMs', async () => {
    const opts = makeOptions({ tierDurationMs: 60, recoveryDurationMs: 60 });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.elapsedMs).toBeGreaterThanOrEqual(0);
    expect(result.totalElapsedMs).toBeGreaterThanOrEqual(result.elapsedMs);
  });

  it('returns correct tierIndex and targetTps', async () => {
    const tier = { ...BASE_TIER, tierIndex: 2, targetTps: 400 };
    const opts = makeOptions();

    const result = await runTier(BASE_SCENARIO, tier, writer, prometheusClient, opts);

    expect(result.tierIndex).toBe(2);
    expect(result.targetTps).toBe(400);
  });

  it('returns txGeneratorExitCode: 0 when generator exits cleanly on stop', async () => {
    const opts = makeOptions();

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    // MockProcess exits with code 0 when killed via SIGINT
    expect(result.txGeneratorExitCode).toBe(0);
  });

  it('returns load-driver resource evidence and saturation flags', async () => {
    const opts = makeOptions();

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.loadDriverResourceEvidence).not.toBeNull();
    expect(result.loadDriverResourceEvidence?.saturationFlags).toBeDefined();
    expect(typeof result.loadDriverResourceEvidence?.saturationFlags.anySaturation).toBe('boolean');
  });

  it('returns txGeneratorExitCode from generator when process already exited with error', async () => {
    const proc = new MockProcess();
    proc.exitCode = 1;
    proc.killed = true;
    const spawner = { spawn: vi.fn(() => proc as unknown as ChildProcess) };
    const opts = makeOptions({ runnerOptions: { spawner, sigintGraceMs: 50, sigTermGraceMs: 50 } });

    const result = await runTier(BASE_SCENARIO, BASE_TIER, writer, prometheusClient, opts);

    expect(result.txGeneratorExitCode).toBe(1);
  });

  // ---- multi-tier acceptance tests ----------------------------------------

  it('multi-tier: second tier runs when first tier has shouldContinue = true', async () => {
    const tier0 = { ...BASE_TIER, tierIndex: 0, targetTps: 100 };
    const tier1 = { ...BASE_TIER, tierIndex: 1, targetTps: 200, seed: 'test-seed:tier:1:tps:200' };
    const opts0 = makeOptions();
    const opts1 = makeOptions();

    const result0 = await runTier(BASE_SCENARIO, tier0, writer, prometheusClient, opts0);
    expect(result0.shouldContinue).toBe(true);

    const result1 = await runTier(BASE_SCENARIO, tier1, writer, prometheusClient, opts1);
    expect(result1.shouldContinue).toBe(true);

    expect(vi.mocked(writer.appendTierSummary)).toHaveBeenCalledTimes(2);
    const summaries = vi.mocked(writer.appendTierSummary).mock.calls.map(([s]) => s);
    expect(summaries[0].tierIndex).toBe(0);
    expect(summaries[1].tierIndex).toBe(1);
  });

  it('multi-tier: stop condition on first tier signals caller to stop', async () => {
    const tier0 = { ...BASE_TIER, tierIndex: 0, targetTps: 100 };
    const opts = makeOptions({
      probeFetcher: vi.fn().mockResolvedValue({ status: 503 } as Response),
    });

    const result0 = await runTier(BASE_SCENARIO, tier0, writer, prometheusClient, opts);

    expect(result0.shouldContinue).toBe(false);
    // Caller would not run tier 1 — only one summary written
    expect(vi.mocked(writer.appendTierSummary)).toHaveBeenCalledTimes(1);
  });
});
