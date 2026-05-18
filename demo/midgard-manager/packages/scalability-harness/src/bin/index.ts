#!/usr/bin/env node

import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import chalk from 'chalk';
import { Command } from 'commander';

import {
  analyzeTiers,
  DEFAULT_RUN_CLASSIFICATION_POLICY,
  type RunClassificationPolicy,
} from '../analysis/analyzer.js';
import type { CollapseInputs } from '../analysis/collapse.js';
import { detectCollapse } from '../analysis/collapse.js';
import type { TierSummary as AnalysisTierSummary } from '../analysis/tier-summary.js';
import { buildTierSummary } from '../analysis/tier-summary.js';
import {
  REQUEST_EVENT_MODES,
  type RequestEventsMode,
  type ScalabilityScenario,
  validateScenario,
} from '../config/scenario.js';
import { generateTiers } from '../config/tiers.js';
import type { RunManifest } from '../evidence/artifacts.js';
import { ArtifactWriter } from '../evidence/artifacts.js';
import { PrometheusClient } from '../metrics/prometheus.js';
import type { TierMetricWindow, TierWindowSummary } from '../metrics/window.js';
import { renderReport } from '../report/markdown.js';
import type { TierRunResult } from '../runner/load-runner.js';
import { runTier } from '../runner/load-runner.js';
import { runExecutionReadinessPreflight } from '../runner/preflight.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function readHarnessVersion(): Promise<string> {
  try {
    const pkgPath = path.join(__dirname, '..', '..', 'package.json');
    const raw = await readFile(pkgPath, 'utf8');
    const pkg = JSON.parse(raw) as { version?: string };
    return pkg.version ?? '0.0.0';
  } catch {
    return '0.0.0';
  }
}

class CliInputError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'CliInputError';
  }
}

async function loadScenario(scenarioPath: string): Promise<ScalabilityScenario> {
  let text: string;
  try {
    text = await readFile(scenarioPath, 'utf8');
  } catch (err) {
    throw new CliInputError(`Failed to read scenario file: ${scenarioPath}\n${String(err)}`);
  }

  let raw: unknown;
  try {
    raw = JSON.parse(text);
  } catch (err) {
    throw new CliInputError(`Failed to parse scenario JSON: ${scenarioPath}\n${String(err)}`);
  }

  try {
    return validateScenario(raw);
  } catch (err) {
    throw new CliInputError(`Invalid scenario: ${String(err)}`);
  }
}

function withResolvedReplayCorpusPath(
  scenario: ScalabilityScenario,
  scenarioPath: string
): ScalabilityScenario {
  if (scenario.replayCorpusPath === undefined || path.isAbsolute(scenario.replayCorpusPath)) {
    return scenario;
  }

  return {
    ...scenario,
    replayCorpusPath: path.resolve(path.dirname(scenarioPath), scenario.replayCorpusPath),
  };
}

function resolveRequestEventsMode(
  requestEvents: string | undefined
): RequestEventsMode | undefined {
  if (requestEvents === undefined) {
    return undefined;
  }
  if (!(REQUEST_EVENT_MODES as readonly string[]).includes(requestEvents)) {
    throw new CliInputError(`--request-events must be one of: ${REQUEST_EVENT_MODES.join(', ')}`);
  }
  return requestEvents as RequestEventsMode;
}

function applyScenarioOverrides(
  scenario: ScalabilityScenario,
  opts: {
    runId?: string;
    outputDir?: string;
    requestEvents?: string;
  }
): ScalabilityScenario {
  const requestEvents = resolveRequestEventsMode(opts.requestEvents);

  const withOverrides: ScalabilityScenario = {
    ...scenario,
    ...(opts.runId !== undefined ? { runId: opts.runId } : {}),
    ...(opts.outputDir !== undefined ? { outputDir: path.resolve(opts.outputDir) } : {}),
    ...(requestEvents !== undefined ? { requestEvents } : {}),
  };

  return validateScenario(withOverrides);
}

function printPreflightResult(
  result: Awaited<ReturnType<typeof runExecutionReadinessPreflight>>
): void {
  for (const check of result.checks) {
    const prefix = check.passed ? chalk.green('  [PASS]') : chalk.red('  [FAIL]');
    console.log(`${prefix} ${check.name} — ${check.summary}`);
    if (!check.passed && check.actionableReason) {
      console.log(chalk.yellow(`        Action: ${check.actionableReason}`));
    }
  }
}

function printScenarioValidityCheck(): void {
  console.log(
    `${chalk.green('  [PASS]')} scenario_validity — Scenario JSON and CLI overrides are valid.`
  );
}

function printPreflightHeading(classification: 'Passed' | 'Blocked'): void {
  const headlineColor = classification === 'Passed' ? chalk.green : chalk.red;
  console.log(headlineColor(`\nExecution Readiness: ${classification}`));
}

// ---------------------------------------------------------------------------
// Collapse input derivation — bridges TierRunResult to detectCollapse
// ---------------------------------------------------------------------------

function lookupCounterDelta(summary: TierWindowSummary | null, query: string): number | null {
  return summary?.counterDeltas.find((d) => d.query === query)?.deltaLoad ?? null;
}

function lookupGaugeFinal(summary: TierWindowSummary | null, query: string): number | null {
  return summary?.gaugeSummaries.find((s) => s.query === query)?.final ?? null;
}

// Derives a prometheusUp scalar from runtime signals.
// If the metric stop condition fired for prometheus_down, up=0.
// If we have any window summary, up=1 (Prometheus was reachable for that data).
// Otherwise null (no data to determine state).
function derivePrometheusUp(result: TierRunResult): number | null {
  if (result.metricStopCondition?.reason === 'prometheus_down') return 0;
  return result.windowSummary !== null ? 1 : null;
}

function buildCollapseInputs(
  result: TierRunResult,
  stopConditions: import('../config/scenario.js').StopConditions
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
    // Use gauge final (end of recovery window) as the recovery size snapshot.
    recoveryQueueSize: lookupGaugeFinal(result.windowSummary, 'tx_queue_size'),
    recoveryMempoolSize: lookupGaugeFinal(result.windowSummary, 'mempool_tx_count'),
    mempoolAcceptedDelta: lookupCounterDelta(
      result.windowSummary,
      'tx_submissions_mempool_accepted_total'
    ),
    tierDurationSeconds: result.elapsedMs / 1000,
    targetTps: result.targetTps,
    stopConditions,
  };
}

function buildRunClassificationPolicy(
  scenario: import('../config/scenario.js').ScalabilityScenario
): RunClassificationPolicy {
  return {
    ...DEFAULT_RUN_CLASSIFICATION_POLICY,
    minDurableThroughputRatio:
      scenario.stopConditions.minUsefulThroughputRatio ??
      DEFAULT_RUN_CLASSIFICATION_POLICY.minDurableThroughputRatio,
    maxFinalQueueSizeAfterRecovery: scenario.stopConditions.maxRecoveryQueueSize,
    maxFinalMempoolSizeAfterRecovery: scenario.stopConditions.maxRecoveryMempoolSize,
    ...(scenario.runClassificationPolicy ?? {}),
  };
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

const program = new Command();

program
  .name('midgard-scalability-harness')
  .description('Benchmark orchestration and evidence collection harness for Midgard L2')
  .version('0.1.0');

// ---------------------------------------------------------------------------
// run
// ---------------------------------------------------------------------------

program
  .command('run')
  .description('Execute a scalability benchmark scenario')
  .requiredOption('--scenario <path>', 'path to scenario JSON file')
  .option('--run-id <id>', 'override scenario runId')
  .option('--output-dir <dir>', 'override scenario outputDir')
  .option('--dry-run', 'validate config and print planned tiers without creating load')
  .option('--max-tier <number>', 'stop after this tier index (inclusive)', (v: string) =>
    parseInt(v, 10)
  )
  .option(
    '--request-events <mode>',
    `per-request submission events mode (${REQUEST_EVENT_MODES.join('|')})`
  )
  .option('--no-increase', 'run only the first tier (disables tier stepping)')
  .action(
    async (opts: {
      scenario: string;
      runId?: string;
      outputDir?: string;
      dryRun?: boolean;
      maxTier?: number;
      requestEvents?: string;
      increase: boolean;
    }) => {
      const scenarioPath = path.resolve(opts.scenario);
      let scenario: ScalabilityScenario;
      try {
        scenario = withResolvedReplayCorpusPath(await loadScenario(scenarioPath), scenarioPath);
        scenario = applyScenarioOverrides(scenario, {
          runId: opts.runId,
          outputDir: opts.outputDir,
          requestEvents: opts.requestEvents,
        });
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        console.error(chalk.red(`\nExecution Readiness: Blocked`));
        console.error(chalk.red(`  - ${message}`));
        process.exit(1);
      }

      // Validate --max-tier value
      if (opts.maxTier !== undefined && (isNaN(opts.maxTier) || opts.maxTier < 0)) {
        console.error(chalk.red('--max-tier must be a non-negative integer'));
        process.exit(1);
      }

      // Build and filter the tier list
      let tiers = generateTiers(scenario);
      if (opts.maxTier !== undefined) {
        tiers = tiers.filter((t) => t.tierIndex <= opts.maxTier!);
      }
      if (!opts.increase) {
        tiers = tiers.slice(0, 1);
      }

      // --- Dry run: print plan and exit without creating any load or artifacts ---
      if (opts.dryRun) {
        console.log(chalk.blue('\nScalability Harness — Dry Run'));
        console.log(chalk.gray(`Scenario: ${scenario.runId}`));
        console.log(chalk.gray(`Node:     ${scenario.nodeEndpoint}`));
        console.log(chalk.gray(`Metrics:  ${scenario.prometheusEndpoint}`));
        console.log(chalk.gray(`Output:   ${scenario.outputDir}`));
        console.log(chalk.gray(`Request Events: ${scenario.requestEvents ?? 'off'}`));

        if (tiers.length === 0) {
          console.log(chalk.yellow('\nNo tiers match the current filters.'));
        } else {
          console.log(chalk.gray(`\nPlanned tiers (${tiers.length}):`));
          for (const tier of tiers) {
            console.log(
              chalk.gray(
                `  [${tier.tierIndex}] ${tier.targetTps} TPS` +
                  ` — ${tier.durationSeconds}s load, ${tier.recoverySeconds}s recovery`
              )
            );
          }
        }

        console.log(chalk.yellow('\nDry run complete — no load created.'));
        return;
      }

      // --- Live run ---
      if (tiers.length === 0) {
        console.log(chalk.yellow('\nNo tiers match the current filters — nothing to run.'));
        return;
      }

      const preflight = await runExecutionReadinessPreflight(scenario);
      printPreflightHeading(preflight.classification);
      printScenarioValidityCheck();
      printPreflightResult(preflight);
      if (!preflight.passed) {
        console.log(chalk.red('\nFormal run is blocked until all preflight checks pass.'));
        for (const reason of preflight.blockedReasons) {
          console.log(chalk.red(`  - ${reason}`));
        }
        process.exit(1);
      }

      console.log(chalk.blue('\nScalability Harness'));
      console.log(chalk.gray(`Scenario: ${scenario.runId}`));
      console.log(chalk.gray(`Node:     ${scenario.nodeEndpoint}`));
      console.log(chalk.gray(`Metrics:  ${scenario.prometheusEndpoint}`));
      console.log(
        chalk.gray(`Tiers:    ${tiers.length} (${tiers.map((t) => t.targetTps).join(' → ')} TPS)`)
      );
      console.log(chalk.gray(`Output:   ${scenario.outputDir}`));
      console.log(chalk.gray(`Request Events: ${scenario.requestEvents ?? 'off'}`));

      const harnessVersion = await readHarnessVersion();

      let writer: ArtifactWriter;
      try {
        writer = await ArtifactWriter.create(scenario, scenarioPath, harnessVersion);
      } catch (err) {
        console.error(chalk.red(`Failed to initialize artifact directory: ${String(err)}`));
        process.exit(1);
      }

      console.log(chalk.green(`\nArtifacts: ${writer.runDir}`));

      // Read the manifest back — ArtifactWriter writes it during create().
      let manifest: RunManifest;
      try {
        const text = await readFile(path.join(writer.runDir, 'run-manifest.json'), 'utf8');
        manifest = JSON.parse(text) as RunManifest;
      } catch (err) {
        console.error(chalk.red(`Failed to read run manifest: ${String(err)}`));
        process.exit(1);
      }

      const prometheusClient = new PrometheusClient(scenario.prometheusEndpoint);

      // --- Tier loop ---
      const analysisSummaries: AnalysisTierSummary[] = [];
      const prometheusWindows: TierMetricWindow[] = [];
      let harnessErrorOccurred = false;

      for (const tier of tiers) {
        console.log(chalk.gray(`\n  [${tier.tierIndex}] ${tier.targetTps} TPS running...`));

        let result: TierRunResult;
        try {
          result = await runTier(scenario, tier, writer, prometheusClient, {
            runnerOptions: {
              requestEvents: scenario.requestEvents ?? 'off',
            },
          });
        } catch (err) {
          console.error(chalk.red(`\nHarness error during tier ${tier.tierIndex}: ${String(err)}`));
          harnessErrorOccurred = true;
          break;
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
        if (result.metricWindow !== null) {
          prometheusWindows.push(result.metricWindow);
        }

        try {
          await writer.writePrometheusSamples({
            capturedAt: new Date().toISOString(),
            runId: scenario.runId,
            tiers: prometheusWindows,
            evidenceIncomplete: result.evidenceIncomplete,
          });
        } catch (err) {
          console.error(chalk.red(`Failed to write Prometheus samples: ${String(err)}`));
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
          console.log(chalk.yellow('\nStop condition triggered — no further tiers will run.'));
          break;
        }
      }

      // --- Final analysis ---
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
          console.error(chalk.red(`Failed to write Prometheus samples: ${String(err)}`));
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
        console.error(chalk.red(`Failed to write summary: ${String(err)}`));
        harnessErrorOccurred = true;
      }

      // Enumerate artifact files after summary is written; report.md is added explicitly
      // because it is written in the next step and won't appear in the directory yet.
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
        // Non-fatal: the report will show an empty artifact index.
      }

      const markdown = renderReport({
        manifest,
        scenario,
        tierSummaries: analysisSummaries,
        conclusion,
        artifactFiles,
      });

      try {
        await writer.writeReport(markdown);
      } catch (err) {
        console.error(chalk.red(`Failed to write report: ${String(err)}`));
        harnessErrorOccurred = true;
      }

      const reportPath = path.join(writer.runDir, 'report.md');
      console.log(chalk.green(`\nReport:    ${reportPath}`));

      if (conclusion.firstCollapsedTier !== null) {
        console.log(
          chalk.yellow(
            `\nCollapse at tier ${conclusion.firstCollapsedTier}` +
              ` (${conclusion.firstCollapsedTargetTps} TPS) — bottleneck: ${conclusion.primaryBottleneck}`
          )
        );
      } else {
        console.log(chalk.green('\nAll tiers completed without collapse.'));
      }

      // System collapse is not a harness failure — exit 0 even when collapse was detected.
      if (harnessErrorOccurred) {
        process.exit(1);
      }
    }
  );

// ---------------------------------------------------------------------------
// preflight — execution readiness gate for formal benchmark runs
// ---------------------------------------------------------------------------

program
  .command('preflight')
  .description('Run execution-readiness checks required before formal benchmark runs')
  .requiredOption('--scenario <path>', 'path to scenario JSON file')
  .option('--run-id <id>', 'override scenario runId')
  .option('--output-dir <dir>', 'override scenario outputDir')
  .option(
    '--request-events <mode>',
    `per-request submission events mode (${REQUEST_EVENT_MODES.join('|')})`
  )
  .action(
    async (opts: {
      scenario: string;
      runId?: string;
      outputDir?: string;
      requestEvents?: string;
    }) => {
      const scenarioPath = path.resolve(opts.scenario);
      let scenario: ScalabilityScenario;

      try {
        scenario = withResolvedReplayCorpusPath(await loadScenario(scenarioPath), scenarioPath);
        scenario = applyScenarioOverrides(scenario, {
          runId: opts.runId,
          outputDir: opts.outputDir,
          requestEvents: opts.requestEvents,
        });
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        console.error(chalk.red('\nExecution Readiness: Blocked'));
        console.error(chalk.red(`  - ${message}`));
        process.exit(1);
      }

      const preflight = await runExecutionReadinessPreflight(scenario);
      printPreflightHeading(preflight.classification);
      printScenarioValidityCheck();
      printPreflightResult(preflight);

      if (!preflight.passed) {
        console.log(chalk.red('\nBlocked reasons:'));
        for (const reason of preflight.blockedReasons) {
          console.log(chalk.red(`  - ${reason}`));
        }
        process.exit(1);
      }
    }
  );

// ---------------------------------------------------------------------------
// tiers — preview the load tier plan without running anything
// ---------------------------------------------------------------------------

program
  .command('tiers')
  .description('Print the load tiers that would be generated for a scenario')
  .requiredOption('--scenario <path>', 'path to scenario JSON file')
  .option('--max-tier <number>', 'show only tiers up to this index (inclusive)', (v: string) =>
    parseInt(v, 10)
  )
  .option('--no-increase', 'show only the first tier')
  .action(async (opts: { scenario: string; maxTier?: number; increase: boolean }) => {
    const scenarioPath = path.resolve(opts.scenario);
    let scenario: ScalabilityScenario;
    try {
      scenario = await loadScenario(scenarioPath);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      console.error(chalk.red(message));
      process.exit(1);
    }

    let tiers = generateTiers(scenario);
    if (opts.maxTier !== undefined) {
      tiers = tiers.filter((t) => t.tierIndex <= opts.maxTier!);
    }
    if (!opts.increase) {
      tiers = tiers.slice(0, 1);
    }

    console.log(chalk.blue(`\nLoad tiers for scenario: ${scenario.runId}`));
    for (const tier of tiers) {
      console.log(
        chalk.gray(
          `  [${tier.tierIndex}] ${tier.targetTps} TPS` +
            ` — ${tier.durationSeconds}s load, ${tier.recoverySeconds}s recovery`
        )
      );
    }
  });

program.parse();
