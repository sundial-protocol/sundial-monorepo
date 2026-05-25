import { mkdtemp, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';

import { describe, expect, it } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import {
  type NodeBalanceFetcher,
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
    txGeneratorTaskCostSeconds: 0.2,
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
  const defaultMempoolSeries: Array<{ value: [number, string] }> = [{ value: [1, '0'] }];
  const defaultStreamDepthSeries: Array<{ value: [number, string] }> = [{ value: [1, '0'] }];
  const defaultStreamPendingSeries: Array<{ value: [number, string] }> = [{ value: [1, '0'] }];
  return () => ({
    queryInstant: async (query: string) => {
      if (overrides && query in overrides) {
        return overrides[query] ?? [];
      }
      if (query === 'mempool_tx_count') {
        return defaultMempoolSeries;
      }
      if (query === 'tx_stream_depth') {
        return defaultStreamDepthSeries;
      }
      if (query === 'tx_stream_pending') {
        return defaultStreamPendingSeries;
      }
      return defaultSeries;
    },
  });
}

const PASSING_TX_INVOKER: TxGeneratorInvoker = {
  invoke: async () => ({ passed: true, summary: 'ok' }),
};

const NOOP_PREFLIGHT_DELAY = async (): Promise<void> => undefined;

// Returns null so the balance check observes (non-blocking) without making real network calls.
const NULL_BALANCE_FETCHER: NodeBalanceFetcher = async () => null;

describe('runExecutionReadinessPreflight', () => {
  it('passes when all readiness checks pass', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-pass-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
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
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          'up{job="sundial_nodes"}': [{ value: [1, '0'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.classification).toBe('Blocked');
    expect(result.checks.find((c) => c.name === 'prometheus_scrape_health')?.passed).toBe(false);
  });

  it('blocks when always-present gauge metrics are missing', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-gauge-missing-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          // tx_stream_depth is always-present — its absence means the node is not running fibers
          tx_stream_depth: [],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'required_metrics_presence')?.passed).toBe(false);
  });

  it('blocks when pre-existing mempool backlog is non-zero', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-mempool-backlog-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          mempool_tx_count: [{ value: [1, '42'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.classification).toBe('Blocked');
    expect(result.checks.find((c) => c.name === 'no_preexisting_mempool_backlog')?.passed).toBe(
      false
    );
  });

  it('blocks when pre-existing tx stream backlog is non-zero', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-stream-backlog-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          tx_stream_depth: [{ value: [1, '1200'] }],
          tx_stream_pending: [{ value: [1, '700'] }],
          mempool_tx_count: [{ value: [1, '0'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.classification).toBe('Blocked');
    expect(result.checks.find((c) => c.name === 'no_preexisting_stream_backlog')?.passed).toBe(
      false
    );
  });

  it('passes when counter metrics are absent on a freshly-started idle node', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-cold-start-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          // Counter metrics are absent on a fresh node — should not block
          tx_submissions_enqueued_total: [],
          tx_submissions_rejected_total: [],
          tx_submissions_mempool_accepted_total: [],
          tx_stream_fail_total: [],
          // commit pipeline readiness is now a separate blocking gate; keep these warm
          // here so this test remains focused on required_metrics_presence behavior.
          commit_block_count_total: [{ value: [1, '1'] }],
          submit_block_count_total: [{ value: [1, '1'] }],
          commit_block_tx_count_total: [],
          l1_commitment_fees_lovelace_total: [],
          l1_commitment_fee_lovelace_last: [],
          commit_block_commitment_failures_total: [],
          merge_block_count_total: [],
          merge_block_failures_total: [],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(true);
    expect(result.classification).toBe('Passed');
    const metricsCheck = result.checks.find((c) => c.name === 'required_metrics_presence');
    expect(metricsCheck?.passed).toBe(true);
    expect(metricsCheck?.summary).toMatch(/not yet active/);
  });

  it('does not block when small unsubmitted backlog remains on an idle pipeline', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-submit-delta-one-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          commit_block_count_total: [{ value: [1, '319'] }],
          submit_block_count_total: [{ value: [1, '317'] }],
          mempool_tx_count: [{ value: [1, '0'] }],
          tx_stream_depth: [{ value: [1, '0'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(true);
    const backlogCheck = result.checks.find((c) => c.name === 'no_unsubmitted_block_backlog');
    expect(backlogCheck?.passed).toBe(false);
    expect(backlogCheck?.blocking).toBe(false);
    expect(backlogCheck?.summary).toMatch(
      /Pre-existing unsubmitted-block backlog remained stable/i
    );
  });

  it('does not block when submit drift is +1 and queue/mempool are idle', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-submit-drift-idle-'));
    let commitCalls = 0;
    let submitCalls = 0;
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: () => ({
          queryInstant: async (query: string) => {
            if (query === 'commit_block_count_total') {
              // initial + 5 rechecks: commit grows once
              const values = ['100', '100', '100', '101', '101', '101'];
              const value = values[Math.min(commitCalls, values.length - 1)];
              commitCalls += 1;
              return [{ value: [1, value] as [number, string] }];
            }
            if (query === 'submit_block_count_total') {
              // submit stays flat
              submitCalls += 1;
              return [{ value: [1, '99'] as [number, string] }];
            }
            if (query === 'tx_stream_depth') {
              return [{ value: [1, '0'] as [number, string] }];
            }
            if (query === 'tx_stream_pending') {
              return [{ value: [1, '0'] as [number, string] }];
            }
            if (query === 'mempool_tx_count') {
              return [{ value: [1, '0'] as [number, string] }];
            }
            return [{ value: [1, '1'] as [number, string] }];
          },
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(true);
    const backlogCheck = result.checks.find((c) => c.name === 'no_unsubmitted_block_backlog');
    expect(backlogCheck?.passed).toBe(false);
    expect(backlogCheck?.blocking).toBe(false);
    expect(backlogCheck?.summary).toMatch(/Submit backlog increased slightly during preflight/i);
  });

  it('does not block when commit pipeline is cold at run start', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-commit-cold-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          commit_block_count_total: [{ value: [1, '0'] }],
          submit_block_count_total: [{ value: [1, '0'] }],
          mempool_tx_count: [{ value: [1, '0'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(true);
    expect(result.classification).toBe('Passed');
    const commitCheck = result.checks.find((c) => c.name === 'commit_pipeline_ready');
    expect(commitCheck?.passed).toBe(false);
    expect(commitCheck?.blocking).toBe(false);
    expect(commitCheck?.summary).toMatch(/Commit pipeline is cold/i);
  });

  it('blocks when artifact output directory is not writable', async () => {
    const root = await mkdtemp(path.join(tmpdir(), 'harness-preflight-not-writable-'));
    const filePath = path.join(root, 'not-a-dir');
    await writeFile(filePath, 'x');

    const result = await runExecutionReadinessPreflight(makeScenario(filePath), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'artifact_directory_writable')?.passed).toBe(false);
  });

  it('blocks when tx-generator is not invocable', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-txgen-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: {
          invoke: async () => ({
            passed: false,
            summary: 'failed',
            actionableReason: 'fix it',
          }),
        },
        nodeBalanceFetcher: NULL_BALANCE_FETCHER,
      },
    });

    expect(result.passed).toBe(false);
    expect(result.checks.find((c) => c.name === 'tx_generator_invocable')?.passed).toBe(false);
    expect(result.blockedReasons).toContain('fix it');
  });

  it('blocks when walletMode is external-key and WALLET_PRIVATE_KEY is missing', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-wallet-mode-'));
    const result = await runExecutionReadinessPreflight(
      {
        ...makeScenario(outputDir),
        walletMode: 'external-key',
      },
      {
        dependencies: {
          delay: NOOP_PREFLIGHT_DELAY,
          probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
          prometheusClientFactory: makePromFactory(),
          txGeneratorInvoker: PASSING_TX_INVOKER,
          nodeBalanceFetcher: NULL_BALANCE_FETCHER,
        },
      }
    );

    const walletModeCheck = result.checks.find((c) => c.name === 'wallet_mode_readiness');
    expect(walletModeCheck).toBeDefined();
    expect(walletModeCheck?.passed).toBe(false);
    expect(walletModeCheck?.blocking).not.toBe(false);
    expect(walletModeCheck?.summary).toMatch(/WALLET_PRIVATE_KEY/i);
    expect(result.passed).toBe(false);
  });

  it('observes (non-blocking) when commitment wallet balance is unavailable', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-wallet-unavailable-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory(),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: async () => null,
      },
    });

    const balanceCheck = result.checks.find((c) => c.name === 'commitment_wallet_balance');
    expect(balanceCheck).toBeDefined();
    expect(balanceCheck?.passed).toBe(false);
    expect(balanceCheck?.blocking).toBe(false);
    expect(balanceCheck?.summary).toMatch(/could not be retrieved/i);
    expect(result.passed).toBe(true);
  });

  it('passes when commitment wallet balance is sufficient for estimated scenario cost', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-wallet-sufficient-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          l1_commitment_fee_lovelace_last: [{ value: [1, '300000'] }],
          'rate(commit_block_count_total[5m])': [{ value: [1, '0.01'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: async () => 2_000_000n,
      },
    });

    const balanceCheck = result.checks.find((c) => c.name === 'commitment_wallet_balance');
    expect(balanceCheck).toBeDefined();
    expect(balanceCheck?.passed).toBe(true);
    expect(balanceCheck?.summary).toMatch(/balance is sufficient/i);
    expect(result.passed).toBe(true);
  });

  it('blocks when commitment wallet balance is insufficient', async () => {
    const outputDir = await mkdtemp(path.join(tmpdir(), 'harness-preflight-wallet-insufficient-'));
    const result = await runExecutionReadinessPreflight(makeScenario(outputDir), {
      dependencies: {
        delay: NOOP_PREFLIGHT_DELAY,
        probeNodeFn: async () => ({ ok: true, statusCode: 404, latencyMs: 5 }),
        prometheusClientFactory: makePromFactory({
          l1_commitment_fee_lovelace_last: [{ value: [1, '300000'] }],
          'rate(commit_block_count_total[5m])': [{ value: [1, '0.01'] }],
        }),
        txGeneratorInvoker: PASSING_TX_INVOKER,
        nodeBalanceFetcher: async () => 1_000_000n,
      },
    });

    const balanceCheck = result.checks.find((c) => c.name === 'commitment_wallet_balance');
    expect(balanceCheck).toBeDefined();
    expect(balanceCheck?.passed).toBe(false);
    expect(balanceCheck?.blocking).not.toBe(false);
    expect(balanceCheck?.summary).toMatch(/shortfall/i);
    expect(balanceCheck?.actionableReason).toMatch(/wallet:block-commitment:topup/);
    expect(result.classification).toBe('Blocked');
    expect(
      result.blockedReasons.some((reason) => reason.includes('wallet:block-commitment:topup'))
    ).toBe(true);
  });
});
