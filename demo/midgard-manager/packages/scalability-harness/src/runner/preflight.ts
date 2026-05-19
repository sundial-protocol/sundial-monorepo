import { spawn, type SpawnOptions } from 'node:child_process';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import path from 'node:path';

import type { ScalabilityScenario } from '../config/scenario.js';
import type {
  Fetcher as PrometheusFetcher,
  PrometheusVectorResult,
} from '../metrics/prometheus.js';
import {
  ALWAYS_PRESENT_NODE_METRICS,
  COUNTER_NODE_METRICS,
  PrometheusClient,
} from '../metrics/prometheus.js';
import type { Fetcher as NodeProbeFetcher, ProbeResult } from './node-probe.js';
import { PROBE_TIMEOUT_MS, probeNode } from './node-probe.js';

const TX_GENERATOR_PREFLIGHT_TIMEOUT_MS = 30_000;
const TX_GENERATOR_CLI_PATH = 'dist/bin/index.js';

export const PREFLIGHT_CHECK_NAMES = [
  'scenario_validity',
  'node_probe',
  'prometheus_scrape_health',
  'required_metrics_presence',
  'artifact_directory_writable',
  'tx_generator_invocable',
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

async function checkArtifactDirectoryWritable(outputDir: string): Promise<PreflightCheckResult> {
  try {
    await mkdir(outputDir, { recursive: true });
    const tempDir = await mkdtemp(path.join(outputDir, '.preflight-'));
    const probeFile = path.join(tempDir, 'write-probe.txt');
    await writeFile(probeFile, 'ok');
    await rm(tempDir, { recursive: true, force: true });

    return pass(
      'artifact_directory_writable',
      `Artifact output directory is writable (${outputDir}).`
    );
  } catch (err) {
    return fail(
      'artifact_directory_writable',
      `Artifact directory write probe failed for ${outputDir}: ${err instanceof Error ? err.message : String(err)}`,
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
  checks.push(await checkArtifactDirectoryWritable(outputDir));
  checks.push(await checkTxGeneratorInvocable(cwd, options.dependencies));

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
