import { describe, expect, it } from 'vitest';

import { ensureSeedReadiness } from '../../src/runner/seed-readiness.js';

type PromSample = { value: [number, string] };

function makePrometheusClient(counterSeries: Record<string, number[]>) {
  const callIndex = new Map<string, number>();
  return {
    queryInstant: async (query: string): Promise<PromSample[]> => {
      const series = counterSeries[query] ?? [0];
      const idx = callIndex.get(query) ?? 0;
      callIndex.set(query, idx + 1);
      const value = series[Math.min(idx, series.length - 1)];
      return [{ value: [1, String(value)] }];
    },
  };
}

describe('ensureSeedReadiness', () => {
  it('returns already_ready when seeding is not required', async () => {
    const prometheusClient = makePrometheusClient({
      blocks_db_seed_attempts_total: [0, 0, 0],
      blocks_db_seed_success_total: [0, 0, 0],
      blocks_db_seed_failures_total: [0, 0, 0],
    });

    const diagnostics = [
      { status: 'ok', count: 1 },
      { status: 'ok', count: 1 },
      { status: 'ok', count: 1 },
    ];
    let diagnosticsCalls = 0;
    let commitCalls = 0;

    let nowMs = 0;
    const result = await ensureSeedReadiness(
      {
        nodeEndpoint: 'http://localhost:3000',
        prometheusClient,
        timeoutMs: 10_000,
        pollIntervalMs: 1_000,
        progressIntervalMs: 2_000,
        commitRetryIntervalMs: 5_000,
      },
      {
        now: () => nowMs,
        sleep: async (ms) => {
          nowMs += ms;
        },
        fetcher: async (url) => {
          if (url.endsWith('/commit')) {
            commitCalls += 1;
            return new Response(JSON.stringify({ message: 'ok' }), { status: 200 });
          }
          if (url.endsWith('/stateQueue/root-unit-diagnostics')) {
            const body = diagnostics[Math.min(diagnosticsCalls, diagnostics.length - 1)];
            diagnosticsCalls += 1;
            return new Response(JSON.stringify(body), { status: 200 });
          }
          throw new Error(`unexpected url: ${url}`);
        },
      }
    );

    expect(result.outcome).toBe('already_ready');
    expect(result.seedAttemptsDelta).toBe(0);
    expect(result.seedSuccessDelta).toBe(0);
    expect(result.seedFailuresDelta).toBe(0);
    expect(commitCalls).toBe(1);
  });

  it('returns seeded when seed success counter increases', async () => {
    const prometheusClient = makePrometheusClient({
      blocks_db_seed_attempts_total: [0, 1, 2, 2],
      blocks_db_seed_success_total: [0, 0, 1, 1],
      blocks_db_seed_failures_total: [0, 1, 1, 1],
    });

    let commitCalls = 0;
    let nowMs = 0;

    const result = await ensureSeedReadiness(
      {
        nodeEndpoint: 'http://localhost:3000',
        prometheusClient,
        timeoutMs: 20_000,
        pollIntervalMs: 1_000,
        progressIntervalMs: 2_000,
        commitRetryIntervalMs: 1_500,
      },
      {
        now: () => nowMs,
        sleep: async (ms) => {
          nowMs += ms;
        },
        fetcher: async (url) => {
          if (url.endsWith('/commit')) {
            commitCalls += 1;
            return new Response(JSON.stringify({ message: 'ok' }), { status: 200 });
          }
          if (url.endsWith('/stateQueue/root-unit-diagnostics')) {
            return new Response(JSON.stringify({ status: 'invalid', count: 0 }), { status: 200 });
          }
          throw new Error(`unexpected url: ${url}`);
        },
      }
    );

    expect(result.outcome).toBe('seeded');
    expect(result.seedAttemptsDelta).toBe(2);
    expect(result.seedSuccessDelta).toBe(1);
    expect(result.seedFailuresDelta).toBe(1);
    expect(commitCalls).toBeGreaterThanOrEqual(1);
  });

  it('throws when seeding does not complete before timeout', async () => {
    const prometheusClient = makePrometheusClient({
      blocks_db_seed_attempts_total: [0, 1, 2, 3, 4, 5],
      blocks_db_seed_success_total: [0, 0, 0, 0, 0, 0],
      blocks_db_seed_failures_total: [0, 1, 2, 3, 4, 5],
    });

    let nowMs = 0;

    await expect(
      ensureSeedReadiness(
        {
          nodeEndpoint: 'http://localhost:3000',
          prometheusClient,
          timeoutMs: 2_500,
          pollIntervalMs: 1_000,
          progressIntervalMs: 1_000,
          commitRetryIntervalMs: 1_000,
        },
        {
          now: () => nowMs,
          sleep: async (ms) => {
            nowMs += ms;
          },
          fetcher: async (url) => {
            if (url.endsWith('/commit')) {
              return new Response(JSON.stringify({ message: 'ok' }), { status: 200 });
            }
            if (url.endsWith('/stateQueue/root-unit-diagnostics')) {
              return new Response(JSON.stringify({ status: 'invalid', count: 0 }), { status: 200 });
            }
            throw new Error(`unexpected url: ${url}`);
          },
        }
      )
    ).rejects.toThrow(/timed out/i);
  });
});
