import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';

import chalk from 'chalk';

import {
  analyzeTiers,
  type BenchmarkConclusion,
  DEFAULT_RUN_CLASSIFICATION_POLICY,
  type RunClassificationPolicy,
} from '../analysis/analyzer.js';
import type { CollapseInputs } from '../analysis/collapse.js';
import { detectCollapse } from '../analysis/collapse.js';
import type { TierSummary as AnalysisTierSummary } from '../analysis/tier-summary.js';
import { buildTierSummary } from '../analysis/tier-summary.js';
import type { ScalabilityScenario, StopConditions } from '../config/scenario.js';
import type { LoadTier } from '../config/tiers.js';
import type { ArtifactWriter, RunManifest } from '../evidence/artifacts.js';
import { GrafanaScreenshotService } from '../evidence/grafana-screenshots.js';
import type { LokiTierCapture } from '../evidence/loki.js';
import { LokiClient } from '../evidence/loki.js';
import type { TempoTierCapture } from '../evidence/tempo.js';
import { TempoClient } from '../evidence/tempo.js';
import { PrometheusClient } from '../metrics/prometheus.js';
import type { TierMetricWindow, TierWindowSummary } from '../metrics/window.js';
import { generateCharts } from '../report/charts.js';
import { renderReport } from '../report/markdown.js';
import type { TierRunResult } from './load-runner.js';
import { runTier } from './load-runner.js';
import { computeSettings } from './tx-generator.js';

function lookupCounterDelta(summary: TierWindowSummary | null, query: string): number | null {
  return summary?.counterDeltas.find((d) => d.query === query)?.deltaLoad ?? null;
}

function lookupGaugeFinal(summary: TierWindowSummary | null, query: string): number | null {
  return summary?.gaugeSummaries.find((s) => s.query === query)?.final ?? null;
}

function derivePrometheusUp(result: TierRunResult): number | null {
  if (result.metricStopCondition?.reason === 'prometheus_down') return 0;
  return result.windowSummary !== null ? 1 : null;
}

function buildCollapseInputs(
  result: TierRunResult,
  stopConditions: StopConditions
): CollapseInputs {
  return {
    nodeUnavailable:
      result.probeResult.stopConditionTriggered ||
      result.recoveryProbeResult.stopConditionTriggered,
    consecutiveProbeFailures: Math.max(
      result.probeResult.consecutiveFailures,
      result.recoveryProbeResult.consecutiveFailures
    ),
    prometheusUp: derivePrometheusUp(result),
    txGeneratorExitCode: result.txGeneratorExitCode,
    commitmentFailuresDelta: lookupCounterDelta(
      result.windowSummary,
      'commit_block_commitment_failures_total'
    ),
    mergeFailuresDelta: lookupCounterDelta(result.windowSummary, 'merge_block_failures_total'),
    afterLoadMempoolSize: result.metricWindow?.afterLoad['mempool_tx_count'] ?? null,
    recoveryQueueSize: lookupGaugeFinal(result.windowSummary, 'tx_queue_size'),
    recoveryMempoolSize: lookupGaugeFinal(result.windowSummary, 'mempool_tx_count'),
    mempoolAcceptedDelta: lookupCounterDelta(
      result.windowSummary,
      'tx_submissions_mempool_accepted_total'
    ),
    committedTxDelta: lookupCounterDelta(result.windowSummary, 'commit_block_tx_count_total'),
    tierDurationSeconds: result.elapsedMs / 1000,
    targetTps: result.targetTps,
    stopConditions,
  };
}

function buildRunClassificationPolicy(scenario: ScalabilityScenario): RunClassificationPolicy {
  const policyOverrides = scenario.runClassificationPolicy ?? {};
  return {
    ...DEFAULT_RUN_CLASSIFICATION_POLICY,
    minDurableThroughputRatio:
      scenario.stopConditions.minUsefulThroughputRatio ??
      DEFAULT_RUN_CLASSIFICATION_POLICY.minDurableThroughputRatio,
    maxFinalQueueSizeAfterRecovery: scenario.stopConditions.maxRecoveryQueueSize,
    maxFinalMempoolSizeAfterRecovery: scenario.stopConditions.maxRecoveryMempoolSize,
    ...policyOverrides,
  };
}

export interface RunScenarioOptions {
  requestEvents?: 'off' | 'sampled' | 'all';
}

export interface RunScenarioResult {
  tierSummaries: AnalysisTierSummary[];
  conclusion: BenchmarkConclusion;
  harnessErrorOccurred: boolean;
}

// Executes the full tier loop for a single scenario and writes all per-run artifacts.
// The writer's runDir must already exist (created by ArtifactWriter.create or createAt).
// The manifest is read from run-manifest.json in the runDir and passed to renderReport.
export async function runScenario(
  scenario: ScalabilityScenario,
  scenarioPath: string,
  tiers: LoadTier[],
  writer: ArtifactWriter,
  options: RunScenarioOptions = {}
): Promise<RunScenarioResult> {
  let manifest: RunManifest;
  try {
    const text = await readFile(path.join(writer.runDir, 'run-manifest.json'), 'utf8');
    manifest = JSON.parse(text) as RunManifest;
  } catch (err) {
    throw new Error(`Failed to read run manifest from ${writer.runDir}: ${String(err)}`);
  }

  const prometheusClient = new PrometheusClient(scenario.prometheusEndpoint);
  const lokiClient =
    scenario.lokiEndpoint !== undefined ? new LokiClient(scenario.lokiEndpoint) : undefined;
  const tempoClient =
    scenario.tempoEndpoint !== undefined ? new TempoClient(scenario.tempoEndpoint) : undefined;
  let screenshotService: GrafanaScreenshotService | null = null;
  try {
    screenshotService = await GrafanaScreenshotService.createForScenario(
      scenario,
      scenarioPath,
      writer.runDir
    );
  } catch (err) {
    console.error(chalk.red(`  Failed to initialize Grafana screenshot service: ${String(err)}`));
  }

  const analysisSummaries: AnalysisTierSummary[] = [];
  const prometheusWindows: TierMetricWindow[] = [];
  const lokiCaptures: LokiTierCapture[] = [];
  const tempoCaptures: TempoTierCapture[] = [];
  let harnessErrorOccurred = false;

  for (const tier of tiers) {
    const generatorSettings = computeSettings(tier.targetTps, scenario.txGeneratorTaskCostSeconds);
    console.log(chalk.gray(`\n  [${tier.tierIndex}] ${tier.targetTps} TPS running...`));
    console.log(
      chalk.gray(
        `      load=${tier.durationSeconds}s recovery=${tier.recoverySeconds}s ` +
          `batch=${generatorSettings.batchSize} concurrency=${generatorSettings.concurrency} ` +
          `interval=${generatorSettings.intervalSeconds}s est_tps=${generatorSettings.actualTpsEstimate.toFixed(2)}`
      )
    );

    let result: TierRunResult;
    try {
      result = await runTier(scenario, tier, writer, prometheusClient, {
        runnerOptions: { requestEvents: options.requestEvents ?? 'off' },
        lokiClient,
        lokiNodeQuery: scenario.lokiNodeQuery,
        tempoClient,
        tempoServiceName: scenario.tempoServiceName,
      });
    } catch (err) {
      console.error(chalk.red(`\n  Harness error during tier ${tier.tierIndex}: ${String(err)}`));
      harnessErrorOccurred = true;
      break;
    }

    if (result.txGeneratorExitCode !== null && result.txGeneratorExitCode !== 0) {
      console.error(
        chalk.red(
          `  [${tier.tierIndex}] tx-generator exited non-zero (exitCode=${result.txGeneratorExitCode}).`
        )
      );
      if (result.txGeneratorErrorSnippet !== null && result.txGeneratorErrorSnippet.trim() !== '') {
        console.error(chalk.red(`  tx-generator stderr tail:\n${result.txGeneratorErrorSnippet}`));
      }
    }

    const submissionCounters = result.submissionAggregate?.counters;
    if (submissionCounters !== undefined) {
      const nodeErrorCounts: Array<{ name: string; count: number }> = [
        { name: 'node_unavailable', count: submissionCounters.node_unavailable },
        { name: 'timed_out', count: submissionCounters.timed_out },
        { name: 'error', count: submissionCounters.error },
      ].filter((entry) => entry.count > 0);

      if (nodeErrorCounts.length > 0) {
        const summary = nodeErrorCounts.map((entry) => `${entry.name}=${entry.count}`).join(', ');
        console.error(
          chalk.red(
            `  [${tier.tierIndex}] node-side submission errors detected: ${summary} ` +
              `(attempted=${submissionCounters.attempted}, submitted=${submissionCounters.submitted}).`
          )
        );
      }
    }

    const collapseResult = detectCollapse(buildCollapseInputs(result, scenario.stopConditions));

    const summary = buildTierSummary({
      tierIndex: result.tierIndex,
      targetTps: result.targetTps,
      startedAt: result.startedAt,
      stoppedAt: result.recoveryStoppedAt,
      metricWindow: result.metricWindow,
      windowSummary: result.windowSummary,
      submissionAggregate: result.submissionAggregate,
      loadDriverResourceEvidence: result.loadDriverResourceEvidence,
      collapse: collapseResult,
      evidenceIncomplete: result.evidenceIncomplete,
    });

    analysisSummaries.push(summary);
    if (result.metricWindow !== null) prometheusWindows.push(result.metricWindow);
    if (result.lokiCapture !== null) lokiCaptures.push(result.lokiCapture);
    if (result.tempoCapture !== null) tempoCaptures.push(result.tempoCapture);
    if (screenshotService !== null) {
      try {
        await screenshotService.captureTierEvents({
          tierIndex: result.tierIndex,
          startedAt: result.startedAt,
          stoppedAt: result.stoppedAt,
          recoveryStoppedAt: result.recoveryStoppedAt,
          metricWindow: result.metricWindow,
          metricStopReason: result.metricStopCondition?.reason ?? null,
          stopConditions: scenario.stopConditions,
        });
      } catch (err) {
        console.error(chalk.red(`  Grafana screenshot capture failed: ${String(err)}`));
      }
    }

    try {
      await writer.writePrometheusSamples({
        capturedAt: new Date().toISOString(),
        runId: scenario.runId,
        tiers: prometheusWindows,
        evidenceIncomplete: result.evidenceIncomplete,
      });
    } catch (err) {
      console.error(chalk.red(`  Failed to write Prometheus samples: ${String(err)}`));
      harnessErrorOccurred = true;
    }

    const label =
      summary.result === 'collapsed'
        ? chalk.red(`collapsed (${summary.collapseReason ?? 'unknown'})`)
        : summary.result === 'evidence_incomplete'
          ? chalk.yellow('evidence incomplete')
          : chalk.green('completed');
    console.log(chalk.gray(`  [${tier.tierIndex}] done — ${label}`));

    if (!result.shouldContinue || collapseResult !== null) {
      console.log(chalk.yellow('  Stop condition triggered — no further tiers will run.'));
      break;
    }
  }

  const conclusion = analyzeTiers(analysisSummaries, {
    policy: buildRunClassificationPolicy(scenario),
    harnessErrorOccurred,
    harnessErrorMessage: harnessErrorOccurred
      ? 'Harness encountered an execution error; results are incomplete.'
      : undefined,
  });

  if (prometheusWindows.length === 0) {
    try {
      await writer.writePrometheusSamples({
        capturedAt: new Date().toISOString(),
        runId: scenario.runId,
        tiers: [],
        evidenceIncomplete: true,
      });
    } catch (err) {
      console.error(chalk.red(`  Failed to write Prometheus samples: ${String(err)}`));
      harnessErrorOccurred = true;
    }
  }

  if (lokiCaptures.length > 0) {
    try {
      await writer.writeLokiCaptures(lokiCaptures);
    } catch (err) {
      console.error(chalk.red(`  Failed to write Loki captures: ${String(err)}`));
      harnessErrorOccurred = true;
    }
  }

  if (tempoCaptures.length > 0) {
    try {
      await writer.writeTempoCaptures(tempoCaptures);
    } catch (err) {
      console.error(chalk.red(`  Failed to write Tempo captures: ${String(err)}`));
      harnessErrorOccurred = true;
    }
  }

  try {
    await writer.writeSummary({
      classification: conclusion.classification,
      classificationReasons: conclusion.classificationReasons,
      violatedChecks: conclusion.violatedChecks,
      criteriaChecks: conclusion.criteriaChecks,
      policy: conclusion.policy,
      tierSummaries: analysisSummaries,
      conclusion,
    });
  } catch (err) {
    console.error(chalk.red(`  Failed to write summary: ${String(err)}`));
    harnessErrorOccurred = true;
  }

  let artifactFiles: string[] = [];
  try {
    const entries = await readdir(writer.runDir, { withFileTypes: true });
    artifactFiles = entries
      .filter((e) => e.isFile())
      .map((e) => e.name)
      .sort();
    if (!artifactFiles.includes('report.md')) {
      artifactFiles = [...artifactFiles, 'report.md'].sort();
    }
  } catch {
    // Non-fatal: report renders with empty artifact index.
  }

  let chartRecords: Awaited<ReturnType<typeof generateCharts>> | undefined;
  if (prometheusWindows.length > 0) {
    try {
      chartRecords = await generateCharts(prometheusWindows, writer.runDir);
    } catch (err) {
      console.error(chalk.red(`  Failed to generate charts: ${String(err)}`));
    }
  }

  const markdown = renderReport({
    manifest,
    scenario,
    tierSummaries: analysisSummaries,
    conclusion,
    artifactFiles,
    lokiCaptures: lokiCaptures.length > 0 ? lokiCaptures : undefined,
    tempoCaptures: tempoCaptures.length > 0 ? tempoCaptures : undefined,
    chartRecords,
  });

  try {
    await writer.writeReport(markdown);
  } catch (err) {
    console.error(chalk.red(`  Failed to write report: ${String(err)}`));
    harnessErrorOccurred = true;
  }

  if (screenshotService !== null) {
    try {
      await screenshotService.close();
    } catch (err) {
      console.error(chalk.red(`  Failed to finalize Grafana screenshots: ${String(err)}`));
    }
  }

  return { tierSummaries: analysisSummaries, conclusion, harnessErrorOccurred };
}
