import { mkdtemp, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';

import { describe, expect, it } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import {
  runExecutionReadinessPreflight,
  type TxGeneratorInvoker,
} from '../../src/runner/preflight.js';

function makeScenario(outputDir: string): ScalabilityScenario {
  return {
    runId: 'baseline-100-800',
    nodeEndpoint: 'http://localhost:3000',
    prometheusEndpoint: 'http://localhost:9090',
    outputDir,
    seed: 'test-seed',
    transactionType: 'one-to-one',
    tierDurationSeconds: 60,
    recoverySeconds: 30,
    startTps: 100,
    maxTps: 800,
    stepMultiplier: 2,
    batchSize: 50,
    concurrency: 4,
    retryAttempts: 3,
    retryDelayMs: 500,
    stopConditions: {
      maxConsecutiveNodeProbeFailures: 5,
      stopOnPrometheusDown: false,
      stopOnCommitmentFailure: true,
      stopOnMergeFailure: false,
    },
  };
}

function makePromFactory(overrides?: Partial<Record<string, Array<{ value: [number, string] }>>>) {
  const defaultSeries: Array<{ value: [number, string] }> = [{ value: [1, '1'] }];
  return () => ({
    queryInstant: async (query: string) => {
      if (overrides && query in overrides) {
        return overrides[query] ?? [];
      }
      return defaultSeries;
    },
  });
}

const PASSING_TX_INVOKER: TxGeneratorInvoker = {
  invoke: async () => ({ passed: true, summary: 'ok' }),
};

describe('runExecutionReadinessPreflight', () => {
  it('passes when all readiness checks pass', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-pass-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: PASSING_TX_INVOKER,
      },
    });

    expect(result.passed).toBe(true);
    expect(result.classification).toBe('Passed');
    expect(result.blockedReasons).toEqual([]);
  });

  it('blocks when Prometheus scrape health is down', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-prom-down-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          'up{job="midgard_nodes"}': [{ value: [1, '0'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.classification).toBe('Blocked');
    expect(result.checks.find((c) => c.name === 'prometheus_scrape_health')?.passed).toBe(false);
  });

  it('blocks when required metrics are missing', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-metric-missing-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          tx_submissions_enqueued_total: [],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'required_metrics_presence')?.passed).toBe(false);
  });

  it('blocks when artifact output directory is not writable', async () => {
    const root = await mkdtemp(path.join(tmpdir(), 'harness-preflight-not-writable-'));
    const filePath = path.join(root, 'not-a-dir');
    await writeFile(filePath, 'x');

    const result = await runExecutionReadinessPreflight(makeScenario(filePath), {
      dependencies: {
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: PASSING_TX_INVOKER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'artifact_directory_writable')?.passed).toBe(false);
  });

  it('blocks when tx-generator is not invocable', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-txgen-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: {
          invoke: async () => ({
            passed: false,
            summary: 'failed',
            actionableReason: 'fix it',
          }),
        },
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'tx_generator_invocable')?.passed).toBe(false);
    expect(result.blockedReasons).toContain('fix it');
  });
});
