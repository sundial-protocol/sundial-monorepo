#!/usr/bin/env node

import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import chalk from 'chalk';
import { Command } from 'commander';

import type { ScalabilityScenario } from '../config/scenario.js';
import {
  REQUEST_EVENT_MODES,
  type RequestEventsMode,
  validateScenario,
} from '../config/scenario.js';
import { generateTiers } from '../config/tiers.js';
import type { PlanConfig } from '../config/plan.js';
import { validatePlan } from '../config/plan.js';
import { ArtifactWriter } from '../evidence/artifacts.js';
import { PlanArtifactWriter } from '../evidence/plan-artifacts.js';
import type { ScenarioRunRecord } from '../report/plan-markdown.js';
import { buildPlanConclusion, renderPlanReport } from '../report/plan-markdown.js';
import { runScenario } from '../runner/scenario-runner.js';
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

async function loadPlan(planPath: string): Promise<PlanConfig> {
  let text: string;
  try {
    text = await readFile(planPath, 'utf8');
  } catch (err) {
    throw new CliInputError(`Failed to read plan file: ${planPath}\n${String(err)}`);
  }

  let raw: unknown;
  try {
    raw = JSON.parse(text);
  } catch (err) {
    throw new CliInputError(`Failed to parse plan JSON: ${planPath}\n${String(err)}`);
  }

  try {
    return validatePlan(raw);
  } catch (err) {
    throw new CliInputError(`Invalid plan: ${String(err)}`);
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
  if (requestEvents === undefined) return undefined;
  if (!(REQUEST_EVENT_MODES as readonly string[]).includes(requestEvents)) {
    throw new CliInputError(`--request-events must be one of: ${REQUEST_EVENT_MODES.join(', ')}`);
  }
  return requestEvents as RequestEventsMode;
}

function applyScenarioOverrides(
  scenario: ScalabilityScenario,
  opts: { runId?: string; outputDir?: string; requestEvents?: string }
): ScalabilityScenario {
  const requestEvents = resolveRequestEventsMode(opts.requestEvents);
  return validateScenario({
    ...scenario,
    ...(opts.runId !== undefined ? { runId: opts.runId } : {}),
    ...(opts.outputDir !== undefined ? { outputDir: path.resolve(opts.outputDir) } : {}),
    ...(requestEvents !== undefined ? { requestEvents } : {}),
  });
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

function printPreflightHeading(classification: 'Passed' | 'Blocked'): void {
  const headlineColor = classification === 'Passed' ? chalk.green : chalk.red;
  console.log(headlineColor(`\nExecution Readiness: ${classification}`));
}

// ---------------------------------------------------------------------------
// Plan execution
// ---------------------------------------------------------------------------

async function executePlan(opts: {
  planPath: string;
  dryRun?: boolean;
  outputDir?: string;
  requestEvents?: string;
}): Promise<void> {
  const planPath = path.resolve(opts.planPath);
  let plan: PlanConfig;
  try {
    plan = await loadPlan(planPath);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(chalk.red(`\nExecution Readiness: Blocked`));
    console.error(chalk.red(`  - ${message}`));
    process.exit(1);
  }

  if (opts.outputDir !== undefined) {
    plan = { ...plan, outputDir: path.resolve(opts.outputDir) };
  }

  // Resolve all scenario paths relative to the plan file's directory.
  const planDir = path.dirname(planPath);
  const resolvedScenarioPaths = plan.scenarios.map((s) =>
    path.isAbsolute(s) ? s : path.resolve(planDir, s)
  );

  // Load and validate all scenarios upfront so failures are caught before any load.
  const loadedScenarios: ScalabilityScenario[] = [];
  for (const [i, scenPath] of resolvedScenarioPaths.entries()) {
    let scenario: ScalabilityScenario;
    try {
      scenario = withResolvedReplayCorpusPath(await loadScenario(scenPath), scenPath);
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      console.error(chalk.red(`\nExecution Readiness: Blocked`));
      console.error(chalk.red(`  - scenarios[${i}] (${scenPath}): ${message}`));
      process.exit(1);
    }
    if (opts.requestEvents !== undefined) {
      try {
        scenario = applyScenarioOverrides(scenario, { requestEvents: opts.requestEvents });
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        console.error(chalk.red(`\nExecution Readiness: Blocked`));
        console.error(chalk.red(`  - ${message}`));
        process.exit(1);
      }
    }
    loadedScenarios.push(scenario);
  }

  // --- Dry run ---
  if (opts.dryRun) {
    console.log(chalk.blue(`\nScalability Harness — Plan Dry Run`));
    console.log(chalk.gray(`Plan:   ${plan.planId}`));
    if (plan.description) console.log(chalk.gray(`        ${plan.description}`));
    console.log(chalk.gray(`Output: ${plan.outputDir}`));
    console.log(chalk.gray(`Stop on failure: ${plan.stopOnFailure ?? true}`));
    console.log('');
    for (const [i, scenario] of loadedScenarios.entries()) {
      const tiers = generateTiers(scenario);
      console.log(
        chalk.gray(
          `Scenario ${i + 1}/${loadedScenarios.length} — ${scenario.runId} (${resolvedScenarioPaths[i]})`
        )
      );
      for (const tier of tiers) {
        console.log(
          chalk.gray(
            `  [${tier.tierIndex}] ${tier.targetTps} TPS — ${tier.durationSeconds}s load, ${tier.recoverySeconds}s recovery`
          )
        );
      }
    }
    console.log(chalk.yellow('\nDry run complete — no load created.'));
    return;
  }

  // --- Live plan run ---
  const harnessVersion = await readHarnessVersion();

  // Preflight against the first scenario's endpoints.
  const firstScenario = loadedScenarios[0];
  const preflight = await runExecutionReadinessPreflight(firstScenario);
  printPreflightHeading(preflight.classification);
  console.log(chalk.green('  [PASS]') + ' scenario_validity — All plan scenarios are valid.');
  printPreflightResult(preflight);

  if (!preflight.passed) {
    console.log(chalk.red('\nFormal plan run is blocked until all preflight checks pass.'));
    for (const reason of preflight.blockedReasons) {
      console.log(chalk.red(`  - ${reason}`));
    }
    process.exit(1);
  }

  const planWriter = await PlanArtifactWriter.create(
    plan,
    planPath,
    resolvedScenarioPaths,
    harnessVersion
  );

  console.log(chalk.blue(`\nScalability Harness — Plan Run`));
  console.log(chalk.gray(`Plan:      ${plan.planId}`));
  console.log(chalk.gray(`Scenarios: ${loadedScenarios.length}`));
  console.log(chalk.gray(`Output:    ${planWriter.planDir}`));

  const records: ScenarioRunRecord[] = [];
  let stopRemaining = false;
  let planHarnessError = false;

  for (const [i, scenario] of loadedScenarios.entries()) {
    const scenPath = resolvedScenarioPaths[i];

    if (stopRemaining) {
      records.push({
        scenarioIndex: i,
        scenarioPath: scenPath,
        runId: scenario.runId,
        runDir: planWriter.scenarioRunDir(i, scenario.runId),
        targetTps: scenario.maxTps,
        scenario,
        tierSummaries: [],
        conclusion: {
          highestCompletedTier: null,
          highestCompletedTargetTps: null,
          firstCollapsedTier: null,
          firstCollapsedTargetTps: null,
          primaryBottleneck: 'skipped',
          classification: 'Blocked',
          classificationReasons: ['Skipped — prior scenario triggered plan stop.'],
          violatedChecks: [],
          criteriaChecks: [],
          policy: {
            maxCollapsedTiers: 0,
            minCompletedTiers: 1,
            maxEvidenceIncompleteTiers: 0,
            minDurableThroughputRatio: 0.5,
            maxRejectedRatio: 0.02,
            maxProcessingFailedRatio: 0.01,
          },
          notes: [],
        },
        skipped: true,
      });
      continue;
    }

    const tiers = generateTiers(scenario);
    const runDir = planWriter.scenarioRunDir(i, scenario.runId);

    console.log(
      chalk.blue(
        `\nScenario ${i + 1}/${loadedScenarios.length} — ${scenario.runId} (${tiers.length} tier${tiers.length === 1 ? '' : 's'}, target ${scenario.maxTps} TPS)`
      )
    );

    let writer: ArtifactWriter;
    try {
      writer = await ArtifactWriter.createAt(runDir, scenario, scenPath, harnessVersion);
    } catch (err) {
      console.error(chalk.red(`Failed to initialize artifact directory: ${String(err)}`));
      planHarnessError = true;
      stopRemaining = true;
      continue;
    }

    console.log(chalk.gray(`  Artifacts: ${writer.runDir}`));

    let result: Awaited<ReturnType<typeof runScenario>>;
    try {
      result = await runScenario(scenario, scenPath, tiers, writer, {
        requestEvents: opts.requestEvents as RequestEventsMode | undefined,
      });
    } catch (err) {
      console.error(chalk.red(`Harness error in scenario ${scenario.runId}: ${String(err)}`));
      planHarnessError = true;
      stopRemaining = true;
      continue;
    }

    if (result.harnessErrorOccurred) planHarnessError = true;

    const classLabel = result.conclusion.classification;
    const classColor =
      classLabel === 'Passed'
        ? chalk.green
        : classLabel === 'Passed with Observations'
          ? chalk.yellow
          : chalk.red;
    console.log(chalk.gray(`  Report:    ${path.join(writer.runDir, 'report.md')}`));
    console.log(`  Result:    ${classColor(classLabel)}`);

    records.push({
      scenarioIndex: i,
      scenarioPath: scenPath,
      runId: scenario.runId,
      runDir: writer.runDir,
      targetTps: scenario.maxTps,
      scenario,
      tierSummaries: result.tierSummaries,
      conclusion: result.conclusion,
      skipped: false,
    });

    const stopOnFailure = plan.stopOnFailure ?? true;
    if (
      stopOnFailure &&
      (classLabel === 'Failed' || classLabel === 'Blocked')
    ) {
      console.log(
        chalk.yellow(
          `\nPlan stop: ${scenario.runId} returned ${classLabel} — remaining scenarios will be skipped.`
        )
      );
      stopRemaining = true;
    }
  }

  // --- Plan-level conclusion and report ---
  const planConclusion = buildPlanConclusion(records, planHarnessError);

  try {
    await planWriter.writePlanSummary({ conclusion: planConclusion, records: records.map((r) => ({
      scenarioIndex: r.scenarioIndex,
      runId: r.runId,
      runDir: r.runDir,
      targetTps: r.targetTps,
      skipped: r.skipped,
      classification: r.skipped ? 'Blocked' : r.conclusion.classification,
    })) });
  } catch (err) {
    console.error(chalk.red(`Failed to write plan summary: ${String(err)}`));
  }

  const planMarkdown = renderPlanReport({
    manifest: JSON.parse(
      await readFile(path.join(planWriter.planDir, 'plan-manifest.json'), 'utf8')
    ),
    records,
    conclusion: planConclusion,
  });

  try {
    await planWriter.writePlanReport(planMarkdown);
  } catch (err) {
    console.error(chalk.red(`Failed to write plan report: ${String(err)}`));
  }

  const planReportPath = path.join(planWriter.planDir, 'plan-report.md');
  console.log(chalk.green(`\nPlan Report: ${planReportPath}`));

  const planLabel =
    planConclusion.classification === 'Passed'
      ? chalk.green(planConclusion.classification)
      : planConclusion.classification === 'Passed with Observations'
        ? chalk.yellow(planConclusion.classification)
        : chalk.red(planConclusion.classification);
  console.log(`Plan Result: ${planLabel}`);
  if (planConclusion.highestPassedRunId !== null) {
    console.log(
      chalk.gray(
        `Highest passed: ${planConclusion.highestPassedRunId} (${planConclusion.highestPassedTargetTps} TPS)`
      )
    );
  }

  if (planHarnessError) process.exit(1);
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
  .description('Execute a scalability benchmark scenario or plan')
  .option('--scenario <path>', 'path to a single scenario JSON file')
  .option('--plan <path>', 'path to a plan JSON file (runs scenarios sequentially)')
  .option('--run-id <id>', 'override scenario runId (scenario mode only)')
  .option('--output-dir <dir>', 'override outputDir')
  .option('--dry-run', 'validate config and print planned tiers without creating load')
  .option('--max-tier <number>', 'stop after this tier index, inclusive (scenario mode only)', (v: string) =>
    parseInt(v, 10)
  )
  .option(
    '--request-events <mode>',
    `per-request submission events mode (${REQUEST_EVENT_MODES.join('|')})`
  )
  .option('--no-increase', 'run only the first tier (scenario mode only)')
  .action(
    async (opts: {
      scenario?: string;
      plan?: string;
      runId?: string;
      outputDir?: string;
      dryRun?: boolean;
      maxTier?: number;
      requestEvents?: string;
      increase: boolean;
    }) => {
      if (opts.plan !== undefined && opts.scenario !== undefined) {
        console.error(chalk.red('Provide either --scenario or --plan, not both.'));
        process.exit(1);
      }

      // --- Plan mode ---
      if (opts.plan !== undefined) {
        await executePlan({
          planPath: opts.plan,
          dryRun: opts.dryRun,
          outputDir: opts.outputDir,
          requestEvents: opts.requestEvents,
        });
        return;
      }

      // --- Single scenario mode ---
      if (opts.scenario === undefined) {
        console.error(chalk.red('Provide --scenario <path> or --plan <path>.'));
        process.exit(1);
      }

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

      if (opts.maxTier !== undefined && (isNaN(opts.maxTier) || opts.maxTier < 0)) {
        console.error(chalk.red('--max-tier must be a non-negative integer'));
        process.exit(1);
      }

      let tiers = generateTiers(scenario);
      if (opts.maxTier !== undefined) {
        tiers = tiers.filter((t) => t.tierIndex <= opts.maxTier!);
      }
      if (!opts.increase) {
        tiers = tiers.slice(0, 1);
      }

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

      if (tiers.length === 0) {
        console.log(chalk.yellow('\nNo tiers match the current filters — nothing to run.'));
        return;
      }

      const preflight = await runExecutionReadinessPreflight(scenario);
      printPreflightHeading(preflight.classification);
      console.log(
        chalk.green('  [PASS]') +
          ' scenario_validity — Scenario JSON and CLI overrides are valid.'
      );
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

      const result = await runScenario(scenario, scenarioPath, tiers, writer, {
        requestEvents: scenario.requestEvents,
      });

      const reportPath = path.join(writer.runDir, 'report.md');
      console.log(chalk.green(`\nReport:    ${reportPath}`));

      if (result.conclusion.firstCollapsedTier !== null) {
        console.log(
          chalk.yellow(
            `\nCollapse at tier ${result.conclusion.firstCollapsedTier}` +
              ` (${result.conclusion.firstCollapsedTargetTps} TPS) — bottleneck: ${result.conclusion.primaryBottleneck}`
          )
        );
      } else {
        console.log(chalk.green('\nAll tiers completed without collapse.'));
      }

      if (result.harnessErrorOccurred) process.exit(1);
    }
  );

// ---------------------------------------------------------------------------
// preflight
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
      console.log(
        chalk.green('  [PASS]') +
          ' scenario_validity — Scenario JSON and CLI overrides are valid.'
      );
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
// tiers
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
