import { spawn, type SpawnOptions } from 'node:child_process';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import path from 'node:path';

import type { ScalabilityScenario } from '../config/scenario.js';
import type {
  Fetcher as PrometheusFetcher,
  PrometheusVectorResult,
} from '../metrics/prometheus.js';
import { NODE_METRICS, PrometheusClient } from '../metrics/prometheus.js';
import type { Fetcher as NodeProbeFetcher, ProbeResult } from './node-probe.js';
import { PROBE_TIMEOUT_MS, probeNode } from './node-probe.js';

const TX_GENERATOR_PREFLIGHT_TIMEOUT_MS = 30_000;

export const PREFLIGHT_CHECK_NAMES = [
  'scenario_validity',
  'node_probe',
  'prometheus_scrape_health',
  'required_metrics_presence',
  'artifact_directory_writable',
  'tx_generator_invocable',
] as const;

export type PreflightCheckName = (typeof PREFLIGHT_CHECK_NAMES)[number];

export interface PreflightCheckResult {
  name: PreflightCheckName;
  passed: boolean;
  summary: string;
  actionableReason?: string;
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

  const checks = await Promise.all(
    NODE_METRICS.map(async (metric) => {
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
    })
  );

  const missing = checks.filter((c) => !c.ok);
  if (missing.length > 0) {
    const sample = missing
      .slice(0, 4)
      .map((m) => `${m.metric} (${m.detail})`)
      .join(', ');
    return fail(
      'required_metrics_presence',
      `Required metric queries missing/unusable: ${missing.length}/${NODE_METRICS.length}. Example: ${sample}`,
      'Ensure node telemetry exports all required benchmark metrics and Prometheus can query them.'
    );
  }

  return pass(
    'required_metrics_presence',
    `All required metrics are queryable (${NODE_METRICS.length}/${NODE_METRICS.length}).`
  );
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
          'tsx',
          'src/bin/index.ts',
          '--help',
        ];

        const spawnOptions: SpawnOptions = {
          cwd,
          stdio: 'ignore',
        };

        const proc = spawn('pnpm', args, spawnOptions);
        const timeout = setTimeout(() => {
          proc.kill('SIGKILL');
          resolve({
            passed: false,
            summary: `tx-generator invocability check timed out after ${TX_GENERATOR_PREFLIGHT_TIMEOUT_MS}ms.`,
            actionableReason:
              'Verify pnpm workspace commands and tx-generator dependencies are executable.',
          });
        }, TX_GENERATOR_PREFLIGHT_TIMEOUT_MS);

        proc.once('error', (err) => {
          clearTimeout(timeout);
          resolve({
            passed: false,
            summary: `tx-generator invocability check failed to spawn: ${err.message}`,
            actionableReason:
              'Install pnpm/Corepack and ensure workspace scripts are runnable from the current working directory.',
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

          resolve({
            passed: false,
            summary: `tx-generator invocability check exited non-zero (code=${code}, signal=${signal ?? 'none'}).`,
            actionableReason:
              'Run tx-generator standalone and fix startup/dependency issues before formal harness runs.',
          });
        });
      });
    },
  };
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

  const blockedReasons = checks
    .filter((c) => !c.passed)
    .map((c) => c.actionableReason ?? c.summary);

  const passed = blockedReasons.length === 0;
  return {
    passed,
    classification: passed ? 'Passed' : 'Blocked',
    checks,
    blockedReasons,
  };
}
