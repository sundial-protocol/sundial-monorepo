import { mkdir, readFile } from 'node:fs/promises';
import path from 'node:path';

import type { ScalabilityScenario, StopConditions } from '../config/scenario.js';
import type { LoadTier } from '../config/tiers.js';
import type { ArtifactWriter, TierSummary } from '../evidence/artifacts.js';
import type {
  HarnessErrorEvent,
  PrometheusSnapshotEvent,
  StopConditionEvent,
  TierStartedEvent,
  TierStoppedEvent,
} from '../evidence/load-events.js';
import { makeEvent } from '../evidence/load-events.js';
import type { LokiClient, LokiTierCapture } from '../evidence/loki.js';
import type { TempoClient, TempoTierCapture } from '../evidence/tempo.js';
import { CADVISOR_METRICS, NODE_METRICS, PrometheusClient } from '../metrics/prometheus.js';
import type { TierMetricWindow, TierWindowSummary } from '../metrics/window.js';
import { collectTierWindow, RANGE_STEP_SECONDS, summarizeTierWindow } from '../metrics/window.js';
import type { HostResourceCollector, LoadDriverResourceEvidence } from './host-resources.js';
import { buildLoadDriverResourceEvidence, createHostResourceCollector } from './host-resources.js';
import type { Fetcher as ProbeFetcher, ProbeLoopResult } from './node-probe.js';
import { PROBE_INTERVAL_MS, PROBE_TIMEOUT_MS, runProbeLoop } from './node-probe.js';
import type { RunnerOptions, SubmissionAggregate } from './tx-generator.js';
import { startTxGenerator, SUBMISSION_AGGREGATES_FILE } from './tx-generator.js';

const ALL_QUERIES = [...NODE_METRICS, ...CADVISOR_METRICS] as const;
const MAX_MISSING_METRICS_IN_ERROR = 5;
const PROGRESS_TICK_INTERVAL_MS = 1_000;
const PROGRESS_LOG_EVERY_N_TICKS = 5;
const PROGRESS_BAR_WIDTH = 24;

type SnapshotCapture = PrometheusSnapshotEvent['capture'];
type TierExecutionPhase = 'load' | 'recovery';

export interface TierRunOptions {
  runnerOptions?: RunnerOptions;
  probeFetcher?: ProbeFetcher;
  probeIntervalMs?: number;
  probeTimeoutMs?: number;
  // Override durations in ms for testing; defaults to tier values * 1000
  tierDurationMs?: number;
  recoveryDurationMs?: number;
  rangeStepSeconds?: number;
  // Injectable for testing
  collectWindowFn?: (
    client: PrometheusClient,
    tierIndex: number,
    targetTps: number,
    startedAt: Date,
    stoppedAt: Date,
    recoveryStartedAt: Date,
    recoveryStoppedAt: Date,
    queries: ReadonlyArray<string>,
    stepSeconds?: number
  ) => Promise<TierMetricWindow>;
  hostResourceCollectorFactory?: () => HostResourceCollector;
  // Optional Loki/Tempo clients for evidence capture after the recovery window.
  lokiClient?: LokiClient;
  lokiNodeQuery?: string;
  tempoClient?: TempoClient;
  tempoServiceName?: string;
}

export interface MetricStopCondition {
  reason: StopConditionEvent['reason'];
  metricValues: Record<string, number | boolean>;
}

interface LiveMetricBaseline {
  commitmentFailuresTotal: number | null;
  mergeFailuresTotal: number | null;
  mempoolAcceptedTotal: number | null;
}

export interface TierRunResult {
  tierIndex: number;
  targetTps: number;
  startedAt: string;
  stoppedAt: string;
  recoveryStoppedAt: string;
  elapsedMs: number;
  totalElapsedMs: number;
  stopConditionTriggered: boolean;
  stopReason: 'completed' | 'stop_condition' | 'error';
  shouldContinue: boolean;
  probeResult: ProbeLoopResult;
  recoveryProbeResult: ProbeLoopResult;
  metricStopCondition: MetricStopCondition | null;
  metricWindow: TierMetricWindow | null;
  windowSummary: TierWindowSummary | null;
  evidenceIncomplete: boolean;
  txGeneratorExitCode: number | null;
  txGeneratorErrorSnippet: string | null;
  submissionAggregate: SubmissionAggregate | null;
  loadDriverResourceEvidence: LoadDriverResourceEvidence | null;
  lokiCapture: LokiTierCapture | null;
  tempoCapture: TempoTierCapture | null;
}

function createTimedController(ms: number): { controller: AbortController; cancel: () => void } {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), ms);
  return { controller, cancel: () => clearTimeout(timer) };
}

function computeEvidenceIncomplete(window: TierMetricWindow): boolean {
  for (const q of NODE_METRICS) {
    if ((window.afterLoad[q] ?? null) === null) {
      return true;
    }
  }
  return false;
}

function makePrometheusSnapshotEvent(
  runId: string,
  tierIndex: number,
  targetTps: number,
  capture: SnapshotCapture,
  values: Record<string, number | null>
): PrometheusSnapshotEvent {
  const metrics: Record<string, number> = {};
  for (const [metric, value] of Object.entries(values)) {
    if (value !== null) {
      metrics[metric] = value;
    }
  }

  const missingPrimaryMetrics = NODE_METRICS.filter((metric) => (values[metric] ?? null) === null);
  const ok = missingPrimaryMetrics.length === 0;
  const missingMetricPreview = missingPrimaryMetrics
    .slice(0, MAX_MISSING_METRICS_IN_ERROR)
    .join(', ');
  const remainingMissingMetrics =
    missingPrimaryMetrics.length -
    Math.min(missingPrimaryMetrics.length, MAX_MISSING_METRICS_IN_ERROR);
  const errorMessage = ok
    ? undefined
    : `Missing primary metrics for ${capture}: ${missingMetricPreview}` +
      (remainingMissingMetrics > 0 ? ` (+${remainingMissingMetrics} more)` : '');

  return makeEvent<PrometheusSnapshotEvent>({
    event: 'prometheus_snapshot',
    runId,
    tierIndex,
    targetTps,
    capture,
    ok,
    metrics,
    errorMessage,
  });
}

function formatDuration(ms: number): string {
  const totalSeconds = Math.max(0, Math.floor(ms / 1000));
  const hours = Math.floor(totalSeconds / 3600);
  const minutes = Math.floor((totalSeconds % 3600) / 60);
  const seconds = totalSeconds % 60;

  if (hours > 0) {
    return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}:${seconds
      .toString()
      .padStart(2, '0')}`;
  }

  return `${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}`;
}

function renderProgressBar(ratio: number): string {
  const clamped = Math.max(0, Math.min(1, ratio));
  const filled = Math.round(clamped * PROGRESS_BAR_WIDTH);
  const empty = PROGRESS_BAR_WIDTH - filled;
  return `[${'='.repeat(filled)}${'-'.repeat(empty)}]`;
}

async function readLiveSubmissionAggregate(
  tierArtifactDir: string
): Promise<SubmissionAggregate | null> {
  try {
    const raw = await readFile(path.join(tierArtifactDir, SUBMISSION_AGGREGATES_FILE), 'utf8');
    return JSON.parse(raw) as SubmissionAggregate;
  } catch {
    return null;
  }
}

function formatSubmissionProgress(aggregate: SubmissionAggregate | null): string {
  const counters = aggregate?.counters;
  if (counters === undefined) {
    return 'tx submitted=n/a attempted=n/a rejected=n/a';
  }

  return (
    `tx submitted=${counters.submitted} attempted=${counters.attempted} ` +
    `rejected=${counters.rejected} unavailable=${counters.node_unavailable} timeout=${counters.timed_out}`
  );
}

function createTierProgressReporter(params: {
  tierIndex: number;
  targetTps: number;
  loadDurationMs: number;
  recoveryDurationMs: number;
  tierArtifactDir: string;
}): {
  updatePhase: (phase: TierExecutionPhase) => void;
  updateProbeStats: (input: {
    totalProbes: number;
    failedProbes: number;
    consecutiveFailures: number;
  }) => void;
  stop: () => void;
} {
  const disableProgressReporter = process.env.VITEST === 'true';
  if (disableProgressReporter) {
    return {
      updatePhase: () => {
        // no-op in test mode
      },
      updateProbeStats: () => {
        // no-op in test mode
      },
      stop: () => {
        // no-op in test mode
      },
    };
  }

  const { tierIndex, targetTps, loadDurationMs, recoveryDurationMs, tierArtifactDir } = params;
  let phase: TierExecutionPhase = 'load';
  let phaseStartedAtMs = Date.now();
  let probeStats = { totalProbes: 0, failedProbes: 0, consecutiveFailures: 0 };
  let stopped = false;
  let tickCount = 0;
  let lastLoggedTick = -1;
  let renderInFlight = false;

  const renderOnce = async (forceLog: boolean): Promise<void> => {
    if (stopped) return;

    const nowMs = Date.now();
    const durationMs = phase === 'load' ? loadDurationMs : recoveryDurationMs;
    const elapsedMs = Math.max(0, Math.min(durationMs, nowMs - phaseStartedAtMs));
    const ratio = durationMs > 0 ? elapsedMs / durationMs : 1;
    const phaseLabel = phase === 'load' ? 'LOAD' : 'RECOVERY';
    const bar = renderProgressBar(ratio);
    const aggregate = await readLiveSubmissionAggregate(tierArtifactDir);

    const line =
      `  [${tierIndex}] ${targetTps} TPS ${phaseLabel} ${bar} ` +
      `${Math.round(ratio * 100)}% (${formatDuration(elapsedMs)}/${formatDuration(durationMs)}) | ` +
      `probes total=${probeStats.totalProbes} failed=${probeStats.failedProbes} ` +
      `consecutive=${probeStats.consecutiveFailures} | ${formatSubmissionProgress(aggregate)}`;

    const shouldLogThisTick =
      tickCount !== lastLoggedTick && tickCount > 0 && tickCount % PROGRESS_LOG_EVERY_N_TICKS === 0;
    if (forceLog || shouldLogThisTick) {
      lastLoggedTick = tickCount;
      console.log(line);
    }
  };

  const scheduleRender = (forceLog = false): void => {
    if (renderInFlight || stopped) return;
    renderInFlight = true;
    void renderOnce(forceLog).finally(() => {
      renderInFlight = false;
    });
  };

  const timer = setInterval(() => {
    tickCount++;
    scheduleRender(false);
  }, PROGRESS_TICK_INTERVAL_MS);
  timer.unref?.();
  scheduleRender(true);

  return {
    updatePhase(nextPhase: TierExecutionPhase) {
      phase = nextPhase;
      phaseStartedAtMs = Date.now();
      scheduleRender(true);
    },
    updateProbeStats(input) {
      probeStats = {
        totalProbes: input.totalProbes,
        failedProbes: input.failedProbes,
        consecutiveFailures: input.consecutiveFailures,
      };
      scheduleRender(false);
    },
    stop() {
      stopped = true;
      clearInterval(timer);
    },
  };
}

// Pure function — exported for direct unit testing.
export function checkMetricStopConditions(
  stopConditions: StopConditions,
  window: TierMetricWindow,
  windowSummary: TierWindowSummary,
  tierDurationSeconds: number,
  targetTps: number
): MetricStopCondition | null {
  if (stopConditions.stopOnPrometheusDown) {
    // null = Prometheus unreachable; 0 = node not being scraped — both treated as down
    const upValue = window.afterLoad['up{job="midgard_nodes"}'] ?? null;
    if (upValue === null || upValue === 0) {
      return {
        reason: 'prometheus_down',
        metricValues: {
          'up{job="midgard_nodes"}': upValue !== null ? upValue : false,
        },
      };
    }
  }

  if (stopConditions.stopOnCommitmentFailure) {
    const delta = windowSummary.counterDeltas.find(
      (d) => d.query === 'commit_block_commitment_failures_total'
    );
    if (delta !== undefined && delta.deltaLoad !== null && delta.deltaLoad > 0) {
      if (stopConditions.maxCommitmentFailureRatio !== undefined) {
        const mempoolAcceptedDelta =
          windowSummary.counterDeltas.find(
            (d) => d.query === 'tx_submissions_mempool_accepted_total'
          )?.deltaLoad ?? null;
        if (mempoolAcceptedDelta !== null && mempoolAcceptedDelta > 0) {
          const commitmentFailureRatio = delta.deltaLoad / mempoolAcceptedDelta;
          if (commitmentFailureRatio <= stopConditions.maxCommitmentFailureRatio) {
            return null;
          }
          return {
            reason: 'commitment_failure',
            metricValues: {
              commit_block_commitment_failures_total: delta.deltaLoad,
              mempoolAcceptedDelta,
              commitmentFailureRatio,
              maxCommitmentFailureRatio: stopConditions.maxCommitmentFailureRatio,
            },
          };
        }
      }
      return {
        reason: 'commitment_failure',
        metricValues: { commit_block_commitment_failures_total: delta.deltaLoad },
      };
    }
  }

  if (stopConditions.stopOnMergeFailure) {
    const delta = windowSummary.counterDeltas.find((d) => d.query === 'merge_block_failures_total');
    if (delta !== undefined && delta.deltaLoad !== null && delta.deltaLoad > 0) {
      return {
        reason: 'merge_failure',
        metricValues: { merge_block_failures_total: delta.deltaLoad },
      };
    }
  }

  if (stopConditions.maxRecoveryQueueSize !== undefined) {
    const size = window.afterRecovery['tx_queue_size'] ?? null;
    if (size !== null && size > stopConditions.maxRecoveryQueueSize) {
      return {
        reason: 'recovery_queue_exceeded',
        metricValues: { tx_queue_size: size },
      };
    }
  }

  if (stopConditions.maxRecoveryMempoolSize !== undefined) {
    const size = window.afterRecovery['mempool_tx_count'] ?? null;
    if (size !== null && size > stopConditions.maxRecoveryMempoolSize) {
      return {
        reason: 'recovery_mempool_exceeded',
        metricValues: { mempool_tx_count: size },
      };
    }
  }

  if (stopConditions.minCommitToAcceptedRatio !== undefined) {
    const mempoolAcceptedDelta =
      windowSummary.counterDeltas.find((d) => d.query === 'tx_submissions_mempool_accepted_total')
        ?.deltaLoad ?? null;
    const committedTxDelta =
      windowSummary.counterDeltas.find((d) => d.query === 'commit_block_tx_count_total')
        ?.deltaLoad ?? null;
    if (mempoolAcceptedDelta !== null && committedTxDelta !== null && mempoolAcceptedDelta > 0) {
      const commitToAcceptedRatio = committedTxDelta / mempoolAcceptedDelta;
      if (commitToAcceptedRatio < stopConditions.minCommitToAcceptedRatio) {
        return {
          reason: 'commit_drain_below_threshold',
          metricValues: {
            committedTxDelta,
            mempoolAcceptedDelta,
            commitToAcceptedRatio,
            minCommitToAcceptedRatio: stopConditions.minCommitToAcceptedRatio,
          },
        };
      }
    }
  }

  if (stopConditions.minUsefulThroughputRatio !== undefined) {
    const mempoolAcceptedDelta =
      windowSummary.counterDeltas.find((d) => d.query === 'tx_submissions_mempool_accepted_total')
        ?.deltaLoad ?? null;
    if (mempoolAcceptedDelta !== null && tierDurationSeconds > 0 && targetTps > 0) {
      const observedMempoolAcceptedTps = mempoolAcceptedDelta / tierDurationSeconds;
      const usefulThroughputRatio = observedMempoolAcceptedTps / targetTps;
      if (usefulThroughputRatio < stopConditions.minUsefulThroughputRatio) {
        return {
          reason: 'throughput_below_minimum',
          metricValues: {
            mempoolAcceptedDelta,
            tierDurationSeconds,
            observedMempoolAcceptedTps,
            targetTps,
            usefulThroughputRatio,
            minUsefulThroughputRatio: stopConditions.minUsefulThroughputRatio,
          },
        };
      }
    }
  }

  return null;
}

async function queryInstantScalar(client: PrometheusClient, query: string): Promise<number | null> {
  const series = await client.queryInstant(query);
  if (series.length === 0) return null;

  let total = 0;
  let hasFiniteValue = false;
  for (const sample of series) {
    const parsed = parseFloat(sample.value[1]);
    if (!isNaN(parsed) && isFinite(parsed)) {
      total += parsed;
      hasFiniteValue = true;
    }
  }

  return hasFiniteValue ? total : null;
}

async function captureLiveMetricBaseline(
  stopConditions: StopConditions,
  prometheusClient: PrometheusClient
): Promise<LiveMetricBaseline> {
  if (!stopConditions.stopOnCommitmentFailure && !stopConditions.stopOnMergeFailure) {
    return { commitmentFailuresTotal: null, mergeFailuresTotal: null, mempoolAcceptedTotal: null };
  }

  try {
    const [commitmentFailuresTotal, mergeFailuresTotal, mempoolAcceptedTotal] = await Promise.all([
      stopConditions.stopOnCommitmentFailure
        ? queryInstantScalar(prometheusClient, 'commit_block_commitment_failures_total')
        : Promise.resolve(null),
      stopConditions.stopOnMergeFailure
        ? queryInstantScalar(prometheusClient, 'merge_block_failures_total')
        : Promise.resolve(null),
      stopConditions.stopOnCommitmentFailure &&
      stopConditions.maxCommitmentFailureRatio !== undefined
        ? queryInstantScalar(prometheusClient, 'tx_submissions_mempool_accepted_total')
        : Promise.resolve(null),
    ]);
    return { commitmentFailuresTotal, mergeFailuresTotal, mempoolAcceptedTotal };
  } catch {
    return { commitmentFailuresTotal: null, mergeFailuresTotal: null, mempoolAcceptedTotal: null };
  }
}

async function checkLiveMetricStopCondition(
  stopConditions: StopConditions,
  baseline: LiveMetricBaseline,
  prometheusClient: PrometheusClient
): Promise<MetricStopCondition | null> {
  if (
    !stopConditions.stopOnPrometheusDown &&
    !stopConditions.stopOnCommitmentFailure &&
    !stopConditions.stopOnMergeFailure
  ) {
    return null;
  }

  try {
    if (stopConditions.stopOnPrometheusDown) {
      const upValue = await queryInstantScalar(prometheusClient, 'up{job="midgard_nodes"}');
      if (upValue === null || upValue === 0) {
        return {
          reason: 'prometheus_down',
          metricValues: {
            'up{job="midgard_nodes"}': upValue !== null ? upValue : false,
          },
        };
      }
    }

    if (stopConditions.stopOnCommitmentFailure) {
      const [currentFailures, currentMempoolAccepted] = await Promise.all([
        queryInstantScalar(prometheusClient, 'commit_block_commitment_failures_total'),
        stopConditions.maxCommitmentFailureRatio !== undefined
          ? queryInstantScalar(prometheusClient, 'tx_submissions_mempool_accepted_total')
          : Promise.resolve(null),
      ]);
      if (
        currentFailures !== null &&
        baseline.commitmentFailuresTotal !== null &&
        currentFailures > baseline.commitmentFailuresTotal
      ) {
        const commitmentFailuresDelta = currentFailures - baseline.commitmentFailuresTotal;
        if (
          stopConditions.maxCommitmentFailureRatio !== undefined &&
          currentMempoolAccepted !== null &&
          baseline.mempoolAcceptedTotal !== null
        ) {
          const mempoolAcceptedDelta = currentMempoolAccepted - baseline.mempoolAcceptedTotal;
          if (mempoolAcceptedDelta > 0) {
            const commitmentFailureRatio = commitmentFailuresDelta / mempoolAcceptedDelta;
            if (commitmentFailureRatio <= stopConditions.maxCommitmentFailureRatio) {
              // Stay in the tier until the failure ratio exceeds the configured budget.
              return null;
            }
            return {
              reason: 'commitment_failure',
              metricValues: {
                commit_block_commitment_failures_total: commitmentFailuresDelta,
                mempoolAcceptedDelta,
                commitmentFailureRatio,
                maxCommitmentFailureRatio: stopConditions.maxCommitmentFailureRatio,
              },
            };
          }
        }
        return {
          reason: 'commitment_failure',
          metricValues: {
            commit_block_commitment_failures_total: commitmentFailuresDelta,
          },
        };
      }
    }

    if (stopConditions.stopOnMergeFailure) {
      const currentFailures = await queryInstantScalar(
        prometheusClient,
        'merge_block_failures_total'
      );
      if (
        currentFailures !== null &&
        baseline.mergeFailuresTotal !== null &&
        currentFailures > baseline.mergeFailuresTotal
      ) {
        return {
          reason: 'merge_failure',
          metricValues: {
            merge_block_failures_total: currentFailures - baseline.mergeFailuresTotal,
          },
        };
      }
    }
  } catch {
    // Non-fatal here; end-of-tier checks still evaluate full window evidence.
  }

  return null;
}

export async function runTier(
  scenario: ScalabilityScenario,
  tier: LoadTier,
  writer: ArtifactWriter,
  prometheusClient: PrometheusClient,
  options: TierRunOptions = {}
): Promise<TierRunResult> {
  const {
    runnerOptions,
    probeFetcher,
    probeIntervalMs = PROBE_INTERVAL_MS,
    probeTimeoutMs = PROBE_TIMEOUT_MS,
    tierDurationMs = tier.durationSeconds * 1000,
    recoveryDurationMs = tier.recoverySeconds * 1000,
    rangeStepSeconds = RANGE_STEP_SECONDS,
    collectWindowFn = collectTierWindow,
    hostResourceCollectorFactory = createHostResourceCollector,
    lokiClient,
    lokiNodeQuery,
    tempoClient,
    tempoServiceName,
  } = options;

  const tierArtifactDir = path.resolve(writer.runDir, `tier-${tier.tierIndex}`);
  await mkdir(tierArtifactDir, { recursive: true });

  const startedAt = new Date();
  const hostResourceCollector = hostResourceCollectorFactory();

  await writer.appendLoadEvent(
    makeEvent<TierStartedEvent>({
      event: 'tier_started',
      runId: scenario.runId,
      tierIndex: tier.tierIndex,
      targetTps: tier.targetTps,
      durationSeconds: tier.durationSeconds,
      seed: tier.seed,
    })
  );

  const generatorHandle = await startTxGenerator(
    scenario,
    tier,
    writer,
    tierArtifactDir,
    runnerOptions
  );
  const progressReporter = createTierProgressReporter({
    tierIndex: tier.tierIndex,
    targetTps: tier.targetTps,
    loadDurationMs: tierDurationMs,
    recoveryDurationMs,
    tierArtifactDir,
  });
  hostResourceCollector.resetEventLoopLag();
  const beforeLoadResourceSnapshot = await hostResourceCollector.captureSnapshot();

  let probeResult: ProbeLoopResult;
  let recoveryProbeResult: ProbeLoopResult;
  let stoppedAt: Date;
  let generatorResult: Awaited<ReturnType<typeof generatorHandle.stop>> | null = null;
  let recoveryStartedAt: Date;
  let recoveryStoppedAt: Date;
  let liveMetricStopCondition: MetricStopCondition | null = null;

  try {
    const liveMetricBaseline = await captureLiveMetricBaseline(
      scenario.stopConditions,
      prometheusClient
    );
    const { controller: loadController, cancel: cancelLoadTimer } =
      createTimedController(tierDurationMs);
    let liveMetricCheckInFlight = false;
    const scheduleLiveMetricCheck = (): void => {
      if (liveMetricStopCondition !== null || liveMetricCheckInFlight) return;
      liveMetricCheckInFlight = true;
      void checkLiveMetricStopCondition(
        scenario.stopConditions,
        liveMetricBaseline,
        prometheusClient
      )
        .then((stopCondition) => {
          if (stopCondition !== null && liveMetricStopCondition === null) {
            liveMetricStopCondition = stopCondition;
            loadController.abort();
          }
        })
        .finally(() => {
          liveMetricCheckInFlight = false;
        });
    };

    // Abort the load phase immediately if the generator process crashes.
    void generatorHandle.processExited.then((exitCode) => {
      if (!loadController.signal.aborted && exitCode !== null && exitCode !== 0) {
        loadController.abort();
      }
    });

    scheduleLiveMetricCheck();
    probeResult = await runProbeLoop(
      {
        nodeEndpoint: scenario.nodeEndpoint,
        runId: scenario.runId,
        tierIndex: tier.tierIndex,
        maxConsecutiveFailures: scenario.stopConditions.maxConsecutiveNodeProbeFailures,
        intervalMs: probeIntervalMs,
        timeoutMs: probeTimeoutMs,
        onProbeResult: ({ totalProbes, failedProbes, consecutiveFailures }) => {
          progressReporter.updateProbeStats({ totalProbes, failedProbes, consecutiveFailures });
          scheduleLiveMetricCheck();
        },
      },
      writer,
      loadController.signal,
      probeFetcher
    );
    cancelLoadTimer();

    const afterLoadResourceSnapshot = await hostResourceCollector.captureSnapshot();
    hostResourceCollector.resetEventLoopLag();
    stoppedAt = new Date();
    generatorResult = await generatorHandle.stop();
    if (generatorResult === null) {
      throw new Error('tx-generator did not return a result');
    }
    const elapsedMs = stoppedAt.getTime() - startedAt.getTime();

    await writer.appendLoadEvent(
      makeEvent<TierStoppedEvent>({
        event: 'tier_stopped',
        runId: scenario.runId,
        tierIndex: tier.tierIndex,
        targetTps: tier.targetTps,
        elapsedMs,
        reason:
          probeResult.stopConditionTriggered || liveMetricStopCondition !== null
            ? 'stop_condition'
            : 'completed',
      })
    );

    if (liveMetricStopCondition !== null) {
      recoveryStartedAt = new Date();
      recoveryStoppedAt = recoveryStartedAt;
      recoveryProbeResult = {
        stopConditionTriggered: false,
        consecutiveFailures: 0,
        totalProbes: 0,
        failedProbes: 0,
      };
    } else {
      recoveryStartedAt = new Date();
      progressReporter.updatePhase('recovery');
      const { controller: recoveryController, cancel: cancelRecoveryTimer } =
        createTimedController(recoveryDurationMs);
      recoveryProbeResult = await runProbeLoop(
        {
          nodeEndpoint: scenario.nodeEndpoint,
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          maxConsecutiveFailures: scenario.stopConditions.maxConsecutiveNodeProbeFailures,
          intervalMs: probeIntervalMs,
          timeoutMs: probeTimeoutMs,
          onProbeResult: ({ totalProbes, failedProbes, consecutiveFailures }) => {
            progressReporter.updateProbeStats({ totalProbes, failedProbes, consecutiveFailures });
          },
        },
        writer,
        recoveryController.signal,
        probeFetcher
      );
      cancelRecoveryTimer();
      recoveryStoppedAt = new Date();
    }

    const afterRecoveryResourceSnapshot = await hostResourceCollector.captureSnapshot();
    const totalElapsedMs = recoveryStoppedAt.getTime() - startedAt.getTime();
    const loadDriverResourceEvidence = buildLoadDriverResourceEvidence(
      beforeLoadResourceSnapshot,
      afterLoadResourceSnapshot,
      afterRecoveryResourceSnapshot
    );

    let metricWindow: TierMetricWindow | null = null;
    let windowSummary: TierWindowSummary | null = null;
    let metricStopCondition: MetricStopCondition | null = null;
    let evidenceIncomplete = false;

    try {
      metricWindow = await collectWindowFn(
        prometheusClient,
        tier.tierIndex,
        tier.targetTps,
        startedAt,
        stoppedAt,
        recoveryStartedAt,
        recoveryStoppedAt,
        ALL_QUERIES,
        rangeStepSeconds
      );

      await writer.appendLoadEvent(
        makePrometheusSnapshotEvent(
          scenario.runId,
          tier.tierIndex,
          tier.targetTps,
          'before',
          metricWindow.before
        )
      );
      await writer.appendLoadEvent(
        makePrometheusSnapshotEvent(
          scenario.runId,
          tier.tierIndex,
          tier.targetTps,
          'after_load',
          metricWindow.afterLoad
        )
      );
      await writer.appendLoadEvent(
        makePrometheusSnapshotEvent(
          scenario.runId,
          tier.tierIndex,
          tier.targetTps,
          'after_recovery',
          metricWindow.afterRecovery
        )
      );

      windowSummary = summarizeTierWindow(metricWindow, ALL_QUERIES);
      evidenceIncomplete = computeEvidenceIncomplete(metricWindow);
      metricStopCondition =
        liveMetricStopCondition ??
        checkMetricStopConditions(
          scenario.stopConditions,
          metricWindow,
          windowSummary,
          elapsedMs / 1000,
          tier.targetTps
        );
    } catch (err) {
      evidenceIncomplete = true;
      await writer.appendLoadEvent(
        makeEvent<HarnessErrorEvent>({
          event: 'harness_error',
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          errorMessage: err instanceof Error ? err.message : String(err),
          stack: err instanceof Error ? err.stack : undefined,
        })
      );
    }

    if (metricStopCondition !== null) {
      await writer.appendLoadEvent(
        makeEvent<StopConditionEvent>({
          event: 'stop_condition',
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          reason: metricStopCondition.reason,
          metricValues: metricStopCondition.metricValues,
        })
      );
    }

    const txGeneratorFailed = generatorResult.exitCode !== null && generatorResult.exitCode !== 0;
    if (txGeneratorFailed) {
      evidenceIncomplete = true;
      await writer.appendLoadEvent(
        makeEvent<HarnessErrorEvent>({
          event: 'harness_error',
          runId: scenario.runId,
          tierIndex: tier.tierIndex,
          errorMessage:
            `tx-generator exited non-zero (exitCode=${generatorResult.exitCode}, signal=${generatorResult.signal ?? 'none'})` +
            (generatorResult.stderrTail !== null ? `\n${generatorResult.stderrTail}` : ''),
        })
      );
    }

    // Loki/Tempo evidence capture — covers the full tier window (load + recovery).
    // Errors are recorded in the capture object and do not affect tier outcome.
    let lokiCapture: LokiTierCapture | null = null;
    if (lokiClient !== undefined) {
      const capturedAt = new Date().toISOString();
      const query = lokiNodeQuery ?? '{job="containerlogs"}';
      try {
        const result = await lokiClient.queryRange(query, startedAt, recoveryStoppedAt);
        lokiCapture = {
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          capturedAt,
          query,
          startedAt: startedAt.toISOString(),
          recoveryStoppedAt: recoveryStoppedAt.toISOString(),
          result,
          error: null,
        };
      } catch (err) {
        lokiCapture = {
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          capturedAt,
          query,
          startedAt: startedAt.toISOString(),
          recoveryStoppedAt: recoveryStoppedAt.toISOString(),
          result: null,
          error: err instanceof Error ? err.message : String(err),
        };
      }
    }

    let tempoCapture: TempoTierCapture | null = null;
    if (tempoClient !== undefined) {
      const capturedAt = new Date().toISOString();
      const svcName = tempoServiceName ?? 'midgard-node';
      try {
        const result = await tempoClient.searchTraces(svcName, startedAt, recoveryStoppedAt);
        tempoCapture = {
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          capturedAt,
          serviceName: svcName,
          startedAt: startedAt.toISOString(),
          recoveryStoppedAt: recoveryStoppedAt.toISOString(),
          result,
          error: null,
        };
      } catch (err) {
        tempoCapture = {
          tierIndex: tier.tierIndex,
          targetTps: tier.targetTps,
          capturedAt,
          serviceName: svcName,
          startedAt: startedAt.toISOString(),
          recoveryStoppedAt: recoveryStoppedAt.toISOString(),
          result: null,
          error: err instanceof Error ? err.message : String(err),
        };
      }
    }

    const stopConditionTriggered =
      probeResult.stopConditionTriggered ||
      recoveryProbeResult.stopConditionTriggered ||
      metricStopCondition !== null ||
      txGeneratorFailed;

    const stopReason: TierRunResult['stopReason'] = stopConditionTriggered
      ? txGeneratorFailed
        ? 'error'
        : 'stop_condition'
      : 'completed';

    const tierSummary: TierSummary = {
      tierIndex: tier.tierIndex,
      targetTps: tier.targetTps,
      startedAt: startedAt.toISOString(),
      stoppedAt: recoveryStoppedAt.toISOString(),
      elapsedMs: totalElapsedMs,
      reason: stopReason,
      loadElapsedMs: elapsedMs,
      probeStopConditionTriggered: probeResult.stopConditionTriggered,
      recoveryProbeStopConditionTriggered: recoveryProbeResult.stopConditionTriggered,
      metricStopCondition,
      evidenceIncomplete,
      windowSummary,
      submissionAggregate: generatorResult.submissionAggregate,
      loadDriverResourceEvidence,
      loadDriverSaturationFlags: loadDriverResourceEvidence.saturationFlags,
    };

    await writer.appendTierSummary(tierSummary);

    return {
      tierIndex: tier.tierIndex,
      targetTps: tier.targetTps,
      startedAt: startedAt.toISOString(),
      stoppedAt: stoppedAt.toISOString(),
      recoveryStoppedAt: recoveryStoppedAt.toISOString(),
      elapsedMs,
      totalElapsedMs,
      stopConditionTriggered,
      stopReason,
      shouldContinue: !stopConditionTriggered,
      probeResult,
      recoveryProbeResult,
      metricStopCondition,
      metricWindow,
      windowSummary,
      evidenceIncomplete,
      txGeneratorExitCode: generatorResult.exitCode,
      txGeneratorErrorSnippet: generatorResult.stderrTail,
      submissionAggregate: generatorResult.submissionAggregate,
      loadDriverResourceEvidence,
      lokiCapture,
      tempoCapture,
    };
  } finally {
    if (generatorResult === null) {
      try {
        await generatorHandle.stop();
      } catch {
        // Preserve the original error path; failure to stop is non-fatal for reporting.
      }
    }
    hostResourceCollector.close();
    progressReporter.stop();
  }
}
