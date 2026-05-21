import { spawn, type SpawnOptions } from 'node:child_process';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import path from 'node:path';

import type { ScalabilityScenario } from '../config/scenario.js';
import { generateTiers } from '../config/tiers.js';
import type {
  Fetcher as PrometheusFetcher,
  PrometheusVectorResult,
} from '../metrics/prometheus.js';
import {
  ALWAYS_PRESENT_NODE_METRICS,
  COUNTER_NODE_METRICS,
  PrometheusClient,
} from '../metrics/prometheus.js';
import { sanitizePathLikeText } from '../path-sanitization.js';
import type { Fetcher as NodeProbeFetcher, ProbeResult } from './node-probe.js';
import { PROBE_TIMEOUT_MS, probeNode } from './node-probe.js';

const TX_GENERATOR_PREFLIGHT_TIMEOUT_MS = 30_000;
const COMMITMENT_WALLET_BALANCE_PROBE_TIMEOUT_MS = 5_000;
const SUBMIT_BACKLOG_RECHECK_ATTEMPTS = 5;
const SUBMIT_BACKLOG_RECHECK_DELAY_MS = 2_000;
const COMMIT_PIPELINE_RECHECK_ATTEMPTS = 5;
const COMMIT_PIPELINE_RECHECK_DELAY_MS = 2_000;
const MEMPOOL_BACKLOG_RECHECK_ATTEMPTS = 3;
const MEMPOOL_BACKLOG_RECHECK_DELAY_MS = 200;
const MAX_PREFLIGHT_MEMPOOL_TX_COUNT = 0;
const FALLBACK_FEE_PER_BLOCK_LOVELACE = 300_000n;
const FALLBACK_BLOCK_RATE_PER_SECOND = 1 / 30;
const BALANCE_SAFETY_MULTIPLIER_NUMERATOR = 3n;
const BALANCE_SAFETY_MULTIPLIER_DENOMINATOR = 2n;
const LOVELACE_PER_ADA = 1_000_000n;
const TX_GENERATOR_CLI_PATH = 'dist/bin/index.js';

export const PREFLIGHT_CHECK_NAMES = [
  'scenario_validity',
  'node_probe',
  'prometheus_scrape_health',
  'required_metrics_presence',
  'no_unsubmitted_block_backlog',
  'commit_pipeline_ready',
  'no_preexisting_mempool_backlog',
  'artifact_directory_writable',
  'tx_generator_invocable',
  'commitment_wallet_balance',
  'loki_reachable',
  'tempo_reachable',
] as const;

export type PreflightCheckName = (typeof PREFLIGHT_CHECK_NAMES)[number];

export interface PreflightCheckResult {
  name: PreflightCheckName;
  passed: boolean;
  summary: string;
  actionableReason?: string;
  // When false, a failing check contributes to observations but does not block the run.
  // Defaults to true (blocking) when undefined.
  blocking?: boolean;
}

export interface ExecutionReadinessPreflightResult {
  passed: boolean;
  classification: 'Passed' | 'Blocked';
  checks: PreflightCheckResult[];
  blockedReasons: string[];
}

interface PrometheusClientLike {
  queryInstant(query: string, time?: Date): Promise<PrometheusVectorResult>;
}

export interface TxGeneratorInvocabilityResult {
  passed: boolean;
  summary: string;
  actionableReason?: string;
}

export interface TxGeneratorInvoker {
  invoke(cwd: string): Promise<TxGeneratorInvocabilityResult>;
}

export type NodeBalanceFetcher = (nodeEndpoint: string) => Promise<bigint | null>;

export interface PreflightDependencies {
  nodeProbeFetcher?: NodeProbeFetcher;
  prometheusFetcher?: PrometheusFetcher;
  probeNodeFn?: (
    nodeEndpoint: string,
    timeoutMs: number,
    fetcher?: NodeProbeFetcher
  ) => Promise<ProbeResult>;
  prometheusClientFactory?: (endpoint: string, fetcher?: PrometheusFetcher) => PrometheusClientLike;
  txGeneratorInvoker?: TxGeneratorInvoker;
  nodeBalanceFetcher?: NodeBalanceFetcher;
}

export interface RunExecutionReadinessPreflightOptions {
  cwd?: string;
  probeTimeoutMs?: number;
  dependencies?: PreflightDependencies;
}

function pass(name: PreflightCheckName, summary: string): PreflightCheckResult {
  return { name, passed: true, summary };
}

function fail(
  name: PreflightCheckName,
  summary: string,
  actionableReason: string
): PreflightCheckResult {
  return { name, passed: false, summary, actionableReason };
}

// Non-blocking observation: recorded in check results but does not block the run.
function observe(
  name: PreflightCheckName,
  summary: string,
  actionableReason: string
): PreflightCheckResult {
  return { name, passed: false, summary, actionableReason, blocking: false };
}

async function checkNodeProbe(
  scenario: ScalabilityScenario,
  timeoutMs: number,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const probeFn = dependencies?.probeNodeFn ?? probeNode;
  const result = await probeFn(scenario.nodeEndpoint, timeoutMs, dependencies?.nodeProbeFetcher);

  if (result.ok) {
    return pass(
      'node_probe',
      `Node probe succeeded (status=${result.statusCode}, latencyMs=${result.latencyMs}).`
    );
  }

  const detail =
    result.statusCode !== undefined
      ? `status=${result.statusCode}`
      : `error=${result.error ?? 'unknown'}`;

  return fail(
    'node_probe',
    `Node probe failed (${detail}, latencyMs=${result.latencyMs}).`,
    'Verify midgard-node is running and reachable at scenario.nodeEndpoint.'
  );
}

async function checkPrometheusScrapeHealth(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  try {
    const up = await client.queryInstant('up{job="midgard_nodes"}');
    if (up.length === 0) {
      return fail(
        'prometheus_scrape_health',
        'Prometheus scrape-health query returned no series for up{job="midgard_nodes"}.',
        'Fix Prometheus target labels/job naming so midgard node appears under job="midgard_nodes".'
      );
    }

    const hasHealthySeries = up.some((s) => parseFloat(s.value[1]) === 1);
    if (!hasHealthySeries) {
      return fail(
        'prometheus_scrape_health',
        `Prometheus scrape-health query returned ${up.length} series but none are up=1.`,
        'Restore scrape connectivity between Prometheus and midgard-node before benchmarking.'
      );
    }

    return pass(
      'prometheus_scrape_health',
      `Prometheus scrape health is up (series=${up.length}).`
    );
  } catch (err) {
    return fail(
      'prometheus_scrape_health',
      `Prometheus scrape-health check failed: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Prometheus is reachable at scenario.prometheusEndpoint and responding to /api/v1/query.'
    );
  }
}

async function checkRequiredMetricsPresence(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  type MetricCheck = { metric: string; ok: boolean; detail: string };

  async function probe(metric: string): Promise<MetricCheck> {
    try {
      const series = await client.queryInstant(metric);
      return {
        metric,
        ok: series.length > 0 && series.some((s) => !isNaN(parseFloat(s.value[1]))),
        detail: series.length === 0 ? 'no_series' : '',
      };
    } catch (err) {
      return {
        metric,
        ok: false,
        detail: err instanceof Error ? err.message : String(err),
      };
    }
  }

  const [alwaysPresentChecks, counterChecks] = await Promise.all([
    Promise.all(ALWAYS_PRESENT_NODE_METRICS.map(probe)),
    Promise.all(COUNTER_NODE_METRICS.map(probe)),
  ]);

  // Always-present metrics (gauges + scrape health) are hard requirements.
  // If any are missing the node's telemetry pipeline is broken.
  const missingAlwaysPresent = alwaysPresentChecks.filter((c) => !c.ok);
  if (missingAlwaysPresent.length > 0) {
    const sample = missingAlwaysPresent
      .slice(0, 4)
      .map((m) => `${m.metric} (${m.detail})`)
      .join(', ');
    return fail(
      'required_metrics_presence',
      `Always-present node metrics missing: ${missingAlwaysPresent.length}/${ALWAYS_PRESENT_NODE_METRICS.length}. Example: ${sample}`,
      'Ensure midgard-node is running with monitoring enabled and Prometheus is scraping it.'
    );
  }

  // Counter metrics only appear after the first matching node event. Their
  // absence on a freshly-started idle node is expected — report as a
  // warning note but do not block the run.
  const inactiveCounters = counterChecks.filter((c) => !c.ok);
  if (inactiveCounters.length > 0) {
    const total = ALWAYS_PRESENT_NODE_METRICS.length + COUNTER_NODE_METRICS.length;
    const present = total - inactiveCounters.length;
    return pass(
      'required_metrics_presence',
      `${present}/${total} metrics queryable; ${inactiveCounters.length} counter(s) not yet active ` +
        `(expected on a freshly-started idle node — will appear after first node activity): ` +
        inactiveCounters
          .slice(0, 3)
          .map((c) => c.metric)
          .join(', ') +
        (inactiveCounters.length > 3 ? ` +${inactiveCounters.length - 3} more` : '')
    );
  }

  const total = ALWAYS_PRESENT_NODE_METRICS.length + COUNTER_NODE_METRICS.length;
  return pass('required_metrics_presence', `All ${total} metrics are queryable.`);
}

async function checkNoUnsubmittedBlockBacklog(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  try {
    const queryBacklog = async (): Promise<
      | {
          ok: true;
          backlog: number;
          commitCounter: number | null;
          submitCounter: number | null;
        }
      | { ok: false; reason: string; actionableReason: string }
    > => {
      const [backlogSeries, commitSeries, submitSeries] = await Promise.all([
        client.queryInstant('unsubmitted_block_backlog'),
        client.queryInstant('commit_block_count_total'),
        client.queryInstant('submit_block_count_total'),
      ]);
      if (backlogSeries.length === 0) {
        return {
          ok: false as const,
          reason:
            'Could not evaluate unsubmitted-block backlog: unsubmitted_block_backlog is missing.',
          actionableReason:
            'Ensure unsubmitted_block_backlog is exposed in Prometheus before running the harness.',
        };
      }

      const backlog = parseFloat(backlogSeries[0].value[1]);
      if (isNaN(backlog)) {
        return {
          ok: false as const,
          reason: 'Could not evaluate unsubmitted-block backlog: metric value is not numeric.',
          actionableReason:
            'Verify Prometheus returns a numeric value for unsubmitted_block_backlog.',
        };
      }

      const commitCounter = commitSeries.length > 0 ? parseFloat(commitSeries[0].value[1]) : null;
      const submitCounter = submitSeries.length > 0 ? parseFloat(submitSeries[0].value[1]) : null;

      return {
        ok: true as const,
        backlog,
        commitCounter: commitCounter !== null && !isNaN(commitCounter) ? commitCounter : null,
        submitCounter: submitCounter !== null && !isNaN(submitCounter) ? submitCounter : null,
      };
    };

    const wait = (ms: number) =>
      new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
      });

    const initial = await queryBacklog();
    if (!initial.ok) {
      return fail('no_unsubmitted_block_backlog', initial.reason, initial.actionableReason);
    }

    if (initial.backlog <= 0) {
      return pass(
        'no_unsubmitted_block_backlog',
        `No unsubmitted-block backlog detected (unsubmitted_block_backlog=${initial.backlog}).`
      );
    }

    let last = initial;
    for (let attempt = 1; attempt <= SUBMIT_BACKLOG_RECHECK_ATTEMPTS; attempt += 1) {
      await wait(SUBMIT_BACKLOG_RECHECK_DELAY_MS);
      const probe = await queryBacklog();
      if (!probe.ok) {
        return fail('no_unsubmitted_block_backlog', probe.reason, probe.actionableReason);
      }
      last = probe;
    }

    const backlogGrowth = last.backlog - initial.backlog;
    const inferredBacklogGrowth =
      initial.commitCounter !== null &&
      last.commitCounter !== null &&
      initial.submitCounter !== null &&
      last.submitCounter !== null
        ? last.commitCounter - initial.commitCounter - (last.submitCounter - initial.submitCounter)
        : null;
    const effectiveBacklogGrowth =
      inferredBacklogGrowth !== null
        ? Math.max(backlogGrowth, inferredBacklogGrowth)
        : backlogGrowth;

    if (effectiveBacklogGrowth > 1) {
      return fail(
        'no_unsubmitted_block_backlog',
        `Submit backlog increased during preflight window: baseline=${initial.backlog}, final=${last.backlog}, growth=${effectiveBacklogGrowth}.`,
        'Clear or stabilize the submit pipeline before starting load tiers.'
      );
    }

    if (effectiveBacklogGrowth > 0) {
      return observe(
        'no_unsubmitted_block_backlog',
        `Submit backlog increased slightly during preflight (baseline=${initial.backlog}, final=${last.backlog}, growth=${effectiveBacklogGrowth}).`,
        'Optional cleanup: clear historical submit backlog before formal evidence runs.'
      );
    }

    if (initial.backlog <= 0 && last.backlog <= 0) {
      return pass(
        'no_unsubmitted_block_backlog',
        `No unsubmitted-block backlog detected (unsubmitted_block_backlog=${last.backlog}).`
      );
    }

    return observe(
      'no_unsubmitted_block_backlog',
      `Pre-existing unsubmitted-block backlog remained stable during preflight (baseline=${initial.backlog}, final=${last.backlog}).`,
      'Optional cleanup: clear historical submit backlog before formal evidence runs.'
    );
  } catch (err) {
    return fail(
      'no_unsubmitted_block_backlog',
      `Unsubmitted-block backlog check failed: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Prometheus is reachable and exposes unsubmitted_block_backlog.'
    );
  }
}

async function checkNoPreexistingMempoolBacklog(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  try {
    const queryMempoolSize = async (): Promise<
      { ok: true; mempoolSize: number } | { ok: false; reason: string; actionableReason: string }
    > => {
      const series = await client.queryInstant('mempool_tx_count');
      if (series.length === 0) {
        return {
          ok: false,
          reason: 'Could not evaluate pre-existing mempool backlog: mempool_tx_count is missing.',
          actionableReason:
            'Ensure mempool_tx_count is exposed in Prometheus before running the harness.',
        };
      }
      const mempoolSize = parseFloat(series[0].value[1]);
      if (isNaN(mempoolSize)) {
        return {
          ok: false,
          reason:
            'Could not evaluate pre-existing mempool backlog: mempool_tx_count is not numeric.',
          actionableReason: 'Verify Prometheus returns numeric values for mempool_tx_count.',
        };
      }
      return { ok: true, mempoolSize };
    };

    const wait = (ms: number) =>
      new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
      });

    const initial = await queryMempoolSize();
    if (!initial.ok) {
      return fail('no_preexisting_mempool_backlog', initial.reason, initial.actionableReason);
    }

    if (initial.mempoolSize <= MAX_PREFLIGHT_MEMPOOL_TX_COUNT) {
      return pass(
        'no_preexisting_mempool_backlog',
        `No pre-existing mempool backlog detected (mempool_tx_count=${initial.mempoolSize}).`
      );
    }

    let last = initial;
    for (let attempt = 1; attempt <= MEMPOOL_BACKLOG_RECHECK_ATTEMPTS; attempt += 1) {
      await wait(MEMPOOL_BACKLOG_RECHECK_DELAY_MS);
      const probe = await queryMempoolSize();
      if (!probe.ok) {
        return fail('no_preexisting_mempool_backlog', probe.reason, probe.actionableReason);
      }
      last = probe;
      if (probe.mempoolSize <= MAX_PREFLIGHT_MEMPOOL_TX_COUNT) {
        return pass(
          'no_preexisting_mempool_backlog',
          `Transient pre-existing mempool backlog resolved before run start (initial=${initial.mempoolSize}, final=${probe.mempoolSize}).`
        );
      }
    }

    return fail(
      'no_preexisting_mempool_backlog',
      `Preflight detected pre-existing mempool backlog: initial mempool_tx_count=${initial.mempoolSize}, final=${last.mempoolSize} after ${MEMPOOL_BACKLOG_RECHECK_ATTEMPTS} rechecks.`,
      'Clear or recover mempool backlog before running load tiers (for example, reset node state or allow the submission pipeline to drain).'
    );
  } catch (err) {
    return fail(
      'no_preexisting_mempool_backlog',
      `Pre-existing mempool backlog check failed: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Prometheus is reachable and exposes mempool_tx_count.'
    );
  }
}

async function checkCommitPipelineReady(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  try {
    const queryPipelineCounters = async (): Promise<
      | { ok: true; commitValue: number; submitValue: number }
      | { ok: false; reason: string; actionableReason: string }
    > => {
      const [commitSeries, submitSeries] = await Promise.all([
        client.queryInstant('commit_block_count_total'),
        client.queryInstant('submit_block_count_total'),
      ]);

      if (commitSeries.length === 0 || submitSeries.length === 0) {
        return {
          ok: false,
          reason:
            'Could not evaluate commit pipeline readiness: commit_block_count_total or submit_block_count_total is missing.',
          actionableReason:
            'Ensure commit_block_count_total and submit_block_count_total are exposed in Prometheus before running the harness.',
        };
      }

      const commitValue = parseFloat(commitSeries[0].value[1]);
      const submitValue = parseFloat(submitSeries[0].value[1]);
      if (isNaN(commitValue) || isNaN(submitValue)) {
        return {
          ok: false,
          reason: 'Could not evaluate commit pipeline readiness: counter values are not numeric.',
          actionableReason:
            'Verify Prometheus returns numeric values for commit_block_count_total and submit_block_count_total.',
        };
      }

      return { ok: true, commitValue, submitValue };
    };

    const wait = (ms: number) =>
      new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
      });

    const initial = await queryPipelineCounters();
    if (!initial.ok) {
      return fail('commit_pipeline_ready', initial.reason, initial.actionableReason);
    }

    if (initial.commitValue > 0 && initial.submitValue > 0) {
      return pass(
        'commit_pipeline_ready',
        `Commit pipeline is warm (commit_block_count_total=${initial.commitValue}, submit_block_count_total=${initial.submitValue}).`
      );
    }

    let last = initial;
    for (let attempt = 1; attempt <= COMMIT_PIPELINE_RECHECK_ATTEMPTS; attempt += 1) {
      await wait(COMMIT_PIPELINE_RECHECK_DELAY_MS);
      const probe = await queryPipelineCounters();
      if (!probe.ok) {
        return fail('commit_pipeline_ready', probe.reason, probe.actionableReason);
      }
      last = probe;
      if (probe.commitValue > 0 && probe.submitValue > 0) {
        return pass(
          'commit_pipeline_ready',
          `Commit pipeline became ready before run start (initial commit=${initial.commitValue}, submit=${initial.submitValue}; now commit=${probe.commitValue}, submit=${probe.submitValue}).`
        );
      }
    }

    return fail(
      'commit_pipeline_ready',
      `Commit pipeline is cold: commit_block_count_total=${last.commitValue}, submit_block_count_total=${last.submitValue} after ${COMMIT_PIPELINE_RECHECK_ATTEMPTS} rechecks.`,
      'Warm up the commit+submit pipeline so at least one block has been committed and submitted, then rerun preflight.'
    );
  } catch (err) {
    return fail(
      'commit_pipeline_ready',
      `Commit pipeline readiness check failed: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Prometheus is reachable and exposes commit_block_count_total / submit_block_count_total.'
    );
  }
}

async function checkArtifactDirectoryWritable(outputDir: string): Promise<PreflightCheckResult> {
  const redactedOutputDir = sanitizePathLikeText(outputDir);
  try {
    await mkdir(outputDir, { recursive: true });
    const tempDir = await mkdtemp(path.join(outputDir, '.preflight-'));
    const probeFile = path.join(tempDir, 'write-probe.txt');
    await writeFile(probeFile, 'ok');
    await rm(tempDir, { recursive: true, force: true });

    return pass(
      'artifact_directory_writable',
      `Artifact output directory is writable (${redactedOutputDir}).`
    );
  } catch (err) {
    return fail(
      'artifact_directory_writable',
      `Artifact directory write probe failed for ${redactedOutputDir}: ${
        err instanceof Error ? err.message : String(err)
      }`,
      'Fix outputDir permissions or choose a writable --output-dir before formal runs.'
    );
  }
}

function defaultTxGeneratorInvoker(): TxGeneratorInvoker {
  return {
    invoke(cwd: string) {
      return new Promise<TxGeneratorInvocabilityResult>((resolve) => {
        const args = [
          '--filter',
          '@midgard-manager/tx-generator',
          'exec',
          'node',
          TX_GENERATOR_CLI_PATH,
          '--help',
        ];

        const spawnOptions: SpawnOptions = {
          cwd,
          stdio: ['ignore', 'pipe', 'pipe'],
        };

        const proc = spawn('pnpm', args, spawnOptions);
        let stdoutBuffer = '';
        let stderrBuffer = '';
        const MAX_CAPTURE = 4000;

        const appendWithLimit = (current: string, chunk: string): string => {
          const combined = current + chunk;
          if (combined.length <= MAX_CAPTURE) {
            return combined;
          }
          return combined.slice(combined.length - MAX_CAPTURE);
        };

        proc.stdout?.on('data', (chunk) => {
          stdoutBuffer = appendWithLimit(stdoutBuffer, String(chunk));
        });
        proc.stderr?.on('data', (chunk) => {
          stderrBuffer = appendWithLimit(stderrBuffer, String(chunk));
        });

        const timeout = setTimeout(() => {
          proc.kill('SIGKILL');
          resolve({
            passed: false,
            summary: `tx-generator invocability check timed out after ${TX_GENERATOR_PREFLIGHT_TIMEOUT_MS}ms.`,
            actionableReason: `Verify pnpm workspace commands are executable and tx-generator is built (${TX_GENERATOR_CLI_PATH}).`,
          });
        }, TX_GENERATOR_PREFLIGHT_TIMEOUT_MS);

        proc.once('error', (err) => {
          clearTimeout(timeout);
          resolve({
            passed: false,
            summary: `tx-generator invocability check failed to spawn: ${err.message}`,
            actionableReason:
              'Install pnpm/Corepack and ensure workspace commands are runnable from the current working directory.',
          });
        });

        proc.once('close', (code, signal) => {
          clearTimeout(timeout);
          if (code === 0) {
            resolve({
              passed: true,
              summary: 'tx-generator command is invocable.',
            });
            return;
          }

          const detail = (stderrBuffer.trim() || stdoutBuffer.trim())
            .split('\n')
            .map((line) => line.trim())
            .filter((line) => line.length > 0)
            .slice(-20)
            .join(' | ');

          resolve({
            passed: false,
            summary:
              `tx-generator invocability check exited non-zero (code=${code}, signal=${signal ?? 'none'}).` +
              (detail.length > 0 ? ` Detail: ${detail}` : ''),
            actionableReason:
              'Run tx-generator standalone (built dist CLI) and fix startup/build issues before formal harness runs.',
          });
        });
      });
    },
  };
}

async function checkLokiReachable(endpoint: string): Promise<PreflightCheckResult> {
  try {
    const url = `${endpoint.replace(/\/$/, '')}/loki/api/v1/labels`;
    const res = await (globalThis.fetch as (url: string) => Promise<Response>)(url);
    if (res.ok) {
      return pass('loki_reachable', `Loki is reachable at ${endpoint} (HTTP ${res.status}).`);
    }
    return observe(
      'loki_reachable',
      `Loki at ${endpoint} returned HTTP ${res.status}.`,
      'Verify Loki is running and reachable. Log evidence capture will fail for this run.'
    );
  } catch (err) {
    return observe(
      'loki_reachable',
      `Loki at ${endpoint} is not reachable: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Loki is running and reachable. Log evidence capture will fail for this run.'
    );
  }
}

async function checkTempoReachable(endpoint: string): Promise<PreflightCheckResult> {
  try {
    const url = `${endpoint.replace(/\/$/, '')}/api/search/tags`;
    const res = await (globalThis.fetch as (url: string) => Promise<Response>)(url);
    if (res.ok) {
      return pass('tempo_reachable', `Tempo is reachable at ${endpoint} (HTTP ${res.status}).`);
    }
    return observe(
      'tempo_reachable',
      `Tempo at ${endpoint} returned HTTP ${res.status}.`,
      'Verify Tempo is running and reachable. Trace evidence capture will fail for this run.'
    );
  } catch (err) {
    return observe(
      'tempo_reachable',
      `Tempo at ${endpoint} is not reachable: ${err instanceof Error ? err.message : String(err)}`,
      'Verify Tempo is running and reachable. Trace evidence capture will fail for this run.'
    );
  }
}

async function checkTxGeneratorInvocable(
  cwd: string,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const invoker = dependencies?.txGeneratorInvoker ?? defaultTxGeneratorInvoker();
  const result = await invoker.invoke(cwd);
  return result.passed
    ? pass('tx_generator_invocable', result.summary)
    : fail(
        'tx_generator_invocable',
        result.summary,
        result.actionableReason ??
          'Run tx-generator directly to identify and resolve invocability issues.'
      );
}

async function defaultNodeBalanceFetcher(nodeEndpoint: string): Promise<bigint | null> {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), COMMITMENT_WALLET_BALANCE_PROBE_TIMEOUT_MS);
  try {
    const url = `${nodeEndpoint.replace(/\/$/, '')}/commitment-wallet/balance`;
    const res = await (globalThis.fetch as typeof fetch)(url, { signal: controller.signal });
    if (!res.ok) return null;
    const json = (await res.json()) as { lovelaceBalance?: unknown };
    if (typeof json.lovelaceBalance !== 'string') return null;
    return BigInt(json.lovelaceBalance);
  } catch {
    return null;
  } finally {
    clearTimeout(timeout);
  }
}

async function estimateNeededLovelace(
  scenario: ScalabilityScenario,
  client: PrometheusClientLike
): Promise<bigint> {
  const tiers = generateTiers(scenario);
  const totalDurationSeconds = tiers.reduce(
    (sum, t) => sum + t.durationSeconds + t.recoverySeconds,
    0
  );

  let feePerBlock = FALLBACK_FEE_PER_BLOCK_LOVELACE;
  try {
    const feeResult = await client.queryInstant('l1_commitment_fee_lovelace_last');
    if (feeResult.length > 0) {
      const feeValue = parseFloat(feeResult[0].value[1]);
      if (!isNaN(feeValue) && feeValue > 0) {
        feePerBlock = BigInt(Math.ceil(feeValue));
      }
    }
  } catch {
    // use fallback
  }

  let blockRatePerSecond = FALLBACK_BLOCK_RATE_PER_SECOND;
  try {
    const rateResult = await client.queryInstant('rate(commit_block_count_total[5m])');
    if (rateResult.length > 0) {
      const rateValue = parseFloat(rateResult[0].value[1]);
      if (!isNaN(rateValue) && rateValue > 0) {
        blockRatePerSecond = rateValue;
      }
    }
  } catch {
    // use fallback
  }

  const estimatedBlocks = Math.ceil(totalDurationSeconds * blockRatePerSecond);
  const estimatedBlocksBigInt = BigInt(estimatedBlocks);
  const estimatedBaseLovelace = feePerBlock * estimatedBlocksBigInt;
  const estimatedWithSafety =
    (estimatedBaseLovelace * BALANCE_SAFETY_MULTIPLIER_NUMERATOR +
      BALANCE_SAFETY_MULTIPLIER_DENOMINATOR -
      1n) /
    BALANCE_SAFETY_MULTIPLIER_DENOMINATOR;
  return estimatedWithSafety;
}

function lovelaceToAdaString(value: bigint): string {
  const whole = value / LOVELACE_PER_ADA;
  const fractional = value % LOVELACE_PER_ADA;
  return `${whole}.${fractional.toString().padStart(6, '0')}`;
}

async function checkCommitmentWalletBalance(
  scenario: ScalabilityScenario,
  dependencies?: PreflightDependencies
): Promise<PreflightCheckResult> {
  const balanceFetcher = dependencies?.nodeBalanceFetcher ?? defaultNodeBalanceFetcher;
  const client =
    dependencies?.prometheusClientFactory?.(
      scenario.prometheusEndpoint,
      dependencies.prometheusFetcher
    ) ?? new PrometheusClient(scenario.prometheusEndpoint, dependencies?.prometheusFetcher);

  const balance = await balanceFetcher(scenario.nodeEndpoint);

  if (balance === null) {
    return observe(
      'commitment_wallet_balance',
      'Commitment wallet balance could not be retrieved; balance check skipped.',
      'Ensure the node exposes GET /commitment-wallet/balance and is reachable. ' +
        'Top up the block commitment wallet if needed before running formal scenarios.'
    );
  }

  const neededLovelace = await estimateNeededLovelace(scenario, client);
  const balanceAda = lovelaceToAdaString(balance);
  const neededAda = lovelaceToAdaString(neededLovelace);

  if (balance < neededLovelace) {
    const shortfallLovelace = neededLovelace - balance;
    const shortfallAda = lovelaceToAdaString(shortfallLovelace);
    return fail(
      'commitment_wallet_balance',
      `Commitment wallet has ${balanceAda} ADA but an estimated ${neededAda} ADA is needed ` +
        `(shortfall: ${shortfallLovelace.toString()} lovelace / ${shortfallAda} ADA).`,
      `Top up the block commitment wallet before running: from demo/, run \`npm run wallet:block-commitment:topup\`.`
    );
  }

  return pass(
    'commitment_wallet_balance',
    `Commitment wallet balance is sufficient: ${balanceAda} ADA available, estimated ${neededAda} ADA needed.`
  );
}

export async function runExecutionReadinessPreflight(
  scenario: ScalabilityScenario,
  options: RunExecutionReadinessPreflightOptions = {}
): Promise<ExecutionReadinessPreflightResult> {
  const cwd = options.cwd ?? process.cwd();
  const outputDir = path.isAbsolute(scenario.outputDir)
    ? scenario.outputDir
    : path.resolve(cwd, scenario.outputDir);
  const probeTimeoutMs = options.probeTimeoutMs ?? PROBE_TIMEOUT_MS;

  const checks: PreflightCheckResult[] = [];

  checks.push(await checkNodeProbe(scenario, probeTimeoutMs, options.dependencies));
  checks.push(await checkPrometheusScrapeHealth(scenario, options.dependencies));
  checks.push(await checkRequiredMetricsPresence(scenario, options.dependencies));
  checks.push(await checkNoUnsubmittedBlockBacklog(scenario, options.dependencies));
  checks.push(await checkCommitPipelineReady(scenario, options.dependencies));
  checks.push(await checkNoPreexistingMempoolBacklog(scenario, options.dependencies));
  checks.push(await checkArtifactDirectoryWritable(outputDir));
  checks.push(await checkTxGeneratorInvocable(cwd, options.dependencies));
  checks.push(await checkCommitmentWalletBalance(scenario, options.dependencies));

  // Non-blocking reachability checks — only run when endpoint is configured.
  if (scenario.lokiEndpoint !== undefined) {
    checks.push(await checkLokiReachable(scenario.lokiEndpoint));
  }
  if (scenario.tempoEndpoint !== undefined) {
    checks.push(await checkTempoReachable(scenario.tempoEndpoint));
  }

  // Only blocking checks (blocking !== false) prevent the run from starting.
  const blockedReasons = checks
    .filter((c) => !c.passed && c.blocking !== false)
    .map((c) => c.actionableReason ?? c.summary);

  const passed = blockedReasons.length === 0;
  return {
    passed,
    classification: passed ? 'Passed' : 'Blocked',
    checks,
    blockedReasons,
  };
}
