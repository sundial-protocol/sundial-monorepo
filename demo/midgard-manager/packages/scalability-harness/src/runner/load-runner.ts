import { mkdir } from 'node:fs/promises';
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
import { CADVISOR_METRICS, NODE_METRICS, PrometheusClient } from '../metrics/prometheus.js';
import type { TierMetricWindow, TierWindowSummary } from '../metrics/window.js';
import { collectTierWindow, RANGE_STEP_SECONDS, summarizeTierWindow } from '../metrics/window.js';
import type { HostResourceCollector, LoadDriverResourceEvidence } from './host-resources.js';
import { buildLoadDriverResourceEvidence, createHostResourceCollector } from './host-resources.js';
import type { Fetcher as ProbeFetcher, ProbeLoopResult } from './node-probe.js';
import { PROBE_INTERVAL_MS, PROBE_TIMEOUT_MS, runProbeLoop } from './node-probe.js';
import type { RunnerOptions, SubmissionAggregate } from './tx-generator.js';
import { startTxGenerator } from './tx-generator.js';

const ALL_QUERIES = [...NODE_METRICS, ...CADVISOR_METRICS] as const;
const MAX_MISSING_METRICS_IN_ERROR = 5;

type SnapshotCapture = PrometheusSnapshotEvent['capture'];

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
}

export interface MetricStopCondition {
  reason: StopConditionEvent['reason'];
  metricValues: Record<string, number | boolean>;
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
  submissionAggregate: SubmissionAggregate | null;
  loadDriverResourceEvidence: LoadDriverResourceEvidence | null;
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
  } = options;

  const tierArtifactDir = path.join(writer.runDir, `tier-${tier.tierIndex}`);
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
  hostResourceCollector.resetEventLoopLag();
  const beforeLoadResourceSnapshot = await hostResourceCollector.captureSnapshot();

  const { controller: loadController, cancel: cancelLoadTimer } =
    createTimedController(tierDurationMs);
  const probeResult = await runProbeLoop(
    {
      nodeEndpoint: scenario.nodeEndpoint,
      runId: scenario.runId,
      tierIndex: tier.tierIndex,
      maxConsecutiveFailures: scenario.stopConditions.maxConsecutiveNodeProbeFailures,
      intervalMs: probeIntervalMs,
      timeoutMs: probeTimeoutMs,
    },
    writer,
    loadController.signal,
    probeFetcher
  );
  cancelLoadTimer();

  const afterLoadResourceSnapshot = await hostResourceCollector.captureSnapshot();
  hostResourceCollector.resetEventLoopLag();

  const stoppedAt = new Date();
  const generatorResult = await generatorHandle.stop();

  const elapsedMs = stoppedAt.getTime() - startedAt.getTime();

  await writer.appendLoadEvent(
    makeEvent<TierStoppedEvent>({
      event: 'tier_stopped',
      runId: scenario.runId,
      tierIndex: tier.tierIndex,
      targetTps: tier.targetTps,
      elapsedMs,
      reason: probeResult.stopConditionTriggered ? 'stop_condition' : 'completed',
    })
  );

  const recoveryStartedAt = new Date();
  const { controller: recoveryController, cancel: cancelRecoveryTimer } =
    createTimedController(recoveryDurationMs);
  const recoveryProbeResult = await runProbeLoop(
    {
      nodeEndpoint: scenario.nodeEndpoint,
      runId: scenario.runId,
      tierIndex: tier.tierIndex,
      maxConsecutiveFailures: scenario.stopConditions.maxConsecutiveNodeProbeFailures,
      intervalMs: probeIntervalMs,
      timeoutMs: probeTimeoutMs,
    },
    writer,
    recoveryController.signal,
    probeFetcher
  );
  cancelRecoveryTimer();

  const recoveryStoppedAt = new Date();
  const afterRecoveryResourceSnapshot = await hostResourceCollector.captureSnapshot();
  hostResourceCollector.close();
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
    metricStopCondition = checkMetricStopConditions(
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

  const stopConditionTriggered =
    probeResult.stopConditionTriggered ||
    recoveryProbeResult.stopConditionTriggered ||
    metricStopCondition !== null;

  const stopReason: TierRunResult['stopReason'] = stopConditionTriggered
    ? 'stop_condition'
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
    submissionAggregate: generatorResult.submissionAggregate,
    loadDriverResourceEvidence,
  };
}
