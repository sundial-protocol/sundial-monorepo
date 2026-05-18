import { describe, expect, it, vi } from 'vitest';

import type { PrometheusVectorResult } from '../../src/metrics/prometheus.js';
import { PrometheusClient } from '../../src/metrics/prometheus.js';
import type { PrometheusSeries, TierMetricWindow } from '../../src/metrics/window.js';
import {
  collectTierWindow,
  computeCounterDelta,
  computeGaugeFinal,
  computeGaugePeak,
  extractScalar,
  isCounter,
  RANGE_STEP_SECONDS,
  summarizeTierWindow,
} from '../../src/metrics/window.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function makeVector(...values: number[]): PrometheusVectorResult {
  return values.map((v) => ({
    metric: {},
    value: [Date.now() / 1000, String(v)] as [number, string],
  }));
}

function makeSeries(values: Array<[number, number]>): PrometheusSeries {
  return {
    metric: {},
    values: values.map(([ts, v]) => [ts, String(v)] as [number, string]),
  };
}

// ---------------------------------------------------------------------------
// isCounter
// ---------------------------------------------------------------------------

describe('isCounter', () => {
  it('returns true for metrics ending in _total', () => {
    expect(isCounter('tx_submissions_enqueued_total')).toBe(true);
    expect(isCounter('commit_block_count_total')).toBe(true);
    expect(isCounter('merge_block_failures_total')).toBe(true);
  });

  it('returns false for gauge metrics', () => {
    expect(isCounter('tx_queue_size')).toBe(false);
    expect(isCounter('tx_queue_peak_size')).toBe(false);
    expect(isCounter('mempool_tx_count')).toBe(false);
  });

  it('returns false for up with label selector', () => {
    expect(isCounter('up{job="midgard_nodes"}')).toBe(false);
  });

  it('returns false for metrics wrapped in rate()', () => {
    expect(isCounter('rate(container_cpu_user_seconds_total{image!=""}[1m])')).toBe(false);
    expect(isCounter('rate(container_network_receive_bytes_total{image!=""}[1m])')).toBe(false);
  });

  it('returns false for cAdvisor memory gauge', () => {
    expect(isCounter('container_memory_usage_bytes{image!=""}')).toBe(false);
  });

  it('returns true for a counter with label selectors', () => {
    expect(isCounter('my_counter_total{label="value"}')).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// extractScalar
// ---------------------------------------------------------------------------

describe('extractScalar', () => {
  it('returns the value from a single-series vector', () => {
    expect(extractScalar(makeVector(42))).toBe(42);
  });

  it('sums values across multiple series', () => {
    expect(extractScalar(makeVector(10, 20, 30))).toBe(60);
  });

  it('returns null for an empty vector', () => {
    expect(extractScalar([])).toBeNull();
  });

  it('returns null for a null result', () => {
    expect(extractScalar(null)).toBeNull();
  });

  it('skips non-finite values and returns null when all are non-finite', () => {
    const result: PrometheusVectorResult = [
      { metric: {}, value: [1, 'NaN'] },
      { metric: {}, value: [1, '+Inf'] },
    ];
    expect(extractScalar(result)).toBeNull();
  });

  it('sums only finite values when a mix is present', () => {
    const result: PrometheusVectorResult = [
      { metric: {}, value: [1, '10'] },
      { metric: {}, value: [1, 'NaN'] },
      { metric: {}, value: [1, '5'] },
    ];
    expect(extractScalar(result)).toBe(15);
  });
});

// ---------------------------------------------------------------------------
// computeGaugePeak
// ---------------------------------------------------------------------------

describe('computeGaugePeak', () => {
  it('returns null for empty series array', () => {
    expect(computeGaugePeak([])).toBeNull();
  });

  it('returns null for series with no values', () => {
    expect(computeGaugePeak([makeSeries([])])).toBeNull();
  });

  it('returns the single value from a one-point series', () => {
    expect(computeGaugePeak([makeSeries([[1000, 5]])])).toBe(5);
  });

  it('returns the maximum value across a single series', () => {
    expect(
      computeGaugePeak([
        makeSeries([
          [1, 3],
          [2, 7],
          [3, 4],
        ]),
      ])
    ).toBe(7);
  });

  it('returns the maximum value across multiple series', () => {
    expect(
      computeGaugePeak([
        makeSeries([
          [1, 3],
          [2, 6],
        ]),
        makeSeries([
          [1, 9],
          [2, 2],
        ]),
      ])
    ).toBe(9);
  });

  it('ignores non-finite values', () => {
    const series: PrometheusSeries = {
      metric: {},
      values: [
        [1, 'NaN'],
        [2, '5'],
        [3, '+Inf'],
      ],
    };
    expect(computeGaugePeak([series])).toBe(5);
  });

  it('returns null when all values are non-finite', () => {
    const series: PrometheusSeries = {
      metric: {},
      values: [
        [1, 'NaN'],
        [2, '+Inf'],
      ],
    };
    expect(computeGaugePeak([series])).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// computeGaugeFinal
// ---------------------------------------------------------------------------

describe('computeGaugeFinal', () => {
  it('returns null for empty series array', () => {
    expect(computeGaugeFinal([])).toBeNull();
  });

  it('returns null for series with no values', () => {
    expect(computeGaugeFinal([makeSeries([])])).toBeNull();
  });

  it('returns the last value of a single series', () => {
    expect(
      computeGaugeFinal([
        makeSeries([
          [1, 10],
          [2, 20],
          [3, 30],
        ]),
      ])
    ).toBe(30);
  });

  it('returns the value from the series with the highest last timestamp', () => {
    // series A ends at ts=2, series B ends at ts=5 — B wins
    const result = computeGaugeFinal([
      makeSeries([
        [1, 100],
        [2, 200],
      ]),
      makeSeries([
        [3, 300],
        [5, 999],
      ]),
    ]);
    expect(result).toBe(999);
  });

  it('ignores series whose last value is non-finite', () => {
    const a: PrometheusSeries = { metric: {}, values: [[1, '5']] };
    const b: PrometheusSeries = { metric: {}, values: [[2, 'NaN']] };
    // b has a later timestamp but NaN value — should fall back to a
    expect(computeGaugeFinal([a, b])).toBe(5);
  });
});

// ---------------------------------------------------------------------------
// computeCounterDelta
// ---------------------------------------------------------------------------

describe('computeCounterDelta', () => {
  it('returns after - before', () => {
    expect(computeCounterDelta(150, 100)).toBe(50);
  });

  it('returns 0 when before equals after', () => {
    expect(computeCounterDelta(100, 100)).toBe(0);
  });

  it('returns null when before is null', () => {
    expect(computeCounterDelta(100, null)).toBeNull();
  });

  it('returns null when after is null', () => {
    expect(computeCounterDelta(null, 100)).toBeNull();
  });

  it('returns null when both are null', () => {
    expect(computeCounterDelta(null, null)).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// summarizeTierWindow
// ---------------------------------------------------------------------------

const COUNTER_QUERY = 'tx_submissions_enqueued_total';
const GAUGE_QUERY = 'tx_queue_size';

function makeWindow(overrides?: Partial<TierMetricWindow>): TierMetricWindow {
  return {
    tierIndex: 0,
    targetTps: 100,
    startedAt: '2026-05-18T10:00:00.000Z',
    stoppedAt: '2026-05-18T10:01:00.000Z',
    recoveryStartedAt: '2026-05-18T10:01:00.000Z',
    recoveryStoppedAt: '2026-05-18T10:01:30.000Z',
    before: { [COUNTER_QUERY]: 1000, [GAUGE_QUERY]: 5 },
    afterLoad: { [COUNTER_QUERY]: 1200, [GAUGE_QUERY]: 8 },
    afterRecovery: { [COUNTER_QUERY]: 1250, [GAUGE_QUERY]: 3 },
    ranges: {
      [GAUGE_QUERY]: [
        makeSeries([
          [1, 5],
          [2, 8],
          [3, 6],
        ]),
      ],
    },
    ...overrides,
  };
}

describe('summarizeTierWindow', () => {
  it('computes deltaLoad for counters as afterLoad - before', () => {
    const { counterDeltas } = summarizeTierWindow(makeWindow(), [COUNTER_QUERY]);
    expect(counterDeltas[0].query).toBe(COUNTER_QUERY);
    expect(counterDeltas[0].deltaLoad).toBe(200); // 1200 - 1000
  });

  it('computes deltaRecovery for counters as afterRecovery - before', () => {
    const { counterDeltas } = summarizeTierWindow(makeWindow(), [COUNTER_QUERY]);
    expect(counterDeltas[0].deltaRecovery).toBe(250); // 1250 - 1000
  });

  it('returns null deltaLoad when before is missing', () => {
    const window = makeWindow({ before: { [COUNTER_QUERY]: null } });
    const { counterDeltas } = summarizeTierWindow(window, [COUNTER_QUERY]);
    expect(counterDeltas[0].deltaLoad).toBeNull();
  });

  it('returns null deltaLoad when afterLoad is missing', () => {
    const window = makeWindow({ afterLoad: { [COUNTER_QUERY]: null } });
    const { counterDeltas } = summarizeTierWindow(window, [COUNTER_QUERY]);
    expect(counterDeltas[0].deltaLoad).toBeNull();
  });

  it('computes gauge peak from range data', () => {
    const { gaugeSummaries } = summarizeTierWindow(makeWindow(), [GAUGE_QUERY]);
    expect(gaugeSummaries[0].peak).toBe(8);
  });

  it('computes gauge final from range data', () => {
    const { gaugeSummaries } = summarizeTierWindow(makeWindow(), [GAUGE_QUERY]);
    expect(gaugeSummaries[0].final).toBe(6);
  });

  it('returns null peak and final when range is empty', () => {
    const window = makeWindow({ ranges: { [GAUGE_QUERY]: [] } });
    const { gaugeSummaries } = summarizeTierWindow(window, [GAUGE_QUERY]);
    expect(gaugeSummaries[0].peak).toBeNull();
    expect(gaugeSummaries[0].final).toBeNull();
  });

  it('classifies rate() queries as gauges', () => {
    const rateQuery = 'rate(container_cpu_user_seconds_total{image!=""}[1m])';
    const window = makeWindow({ ranges: { [rateQuery]: [makeSeries([[1, 0.5]])] } });
    const { gaugeSummaries, counterDeltas } = summarizeTierWindow(window, [rateQuery]);
    expect(gaugeSummaries).toHaveLength(1);
    expect(counterDeltas).toHaveLength(0);
  });

  it('separates queries into counters and gauges correctly', () => {
    const { counterDeltas, gaugeSummaries } = summarizeTierWindow(makeWindow(), [
      COUNTER_QUERY,
      GAUGE_QUERY,
    ]);
    expect(counterDeltas.map((c) => c.query)).toContain(COUNTER_QUERY);
    expect(gaugeSummaries.map((g) => g.query)).toContain(GAUGE_QUERY);
    expect(counterDeltas.map((c) => c.query)).not.toContain(GAUGE_QUERY);
    expect(gaugeSummaries.map((g) => g.query)).not.toContain(COUNTER_QUERY);
  });
});

// ---------------------------------------------------------------------------
// collectTierWindow
// ---------------------------------------------------------------------------

const T0 = new Date('2026-05-18T10:00:00.000Z');
const T1 = new Date('2026-05-18T10:01:00.000Z');
const T2 = new Date('2026-05-18T10:01:00.000Z');
const T3 = new Date('2026-05-18T10:01:30.000Z');
const QUERIES = ['tx_submissions_enqueued_total', 'tx_queue_size'] as const;

function makeVectorResponse(value: number) {
  return {
    status: 'success',
    data: { resultType: 'vector', result: [{ metric: {}, value: [1000, String(value)] }] },
  };
}

function makeMatrixResponse(values: Array<[number, number]>) {
  return {
    status: 'success',
    data: {
      resultType: 'matrix',
      result: [{ metric: {}, values: values.map(([ts, v]) => [ts, String(v)]) }],
    },
  };
}

function makeMockClient(
  instantFn: (query: string, time?: Date) => Promise<PrometheusVectorResult>,
  rangeFn: () => Promise<PrometheusSeries[]>
) {
  const client = {
    queryInstant: vi.fn().mockImplementation(instantFn),
    queryRange: vi.fn().mockImplementation(rangeFn),
  };
  return client as unknown as PrometheusClient;
}

describe('collectTierWindow', () => {
  it('issues instant queries at startedAt, stoppedAt, and recoveryStoppedAt', async () => {
    const instantFn = vi.fn().mockResolvedValue(makeVector(10));
    const client = makeMockClient(instantFn, async () => []);

    await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    // 2 queries × 3 time points = 6 instant calls
    expect(instantFn).toHaveBeenCalledTimes(6);
    const times = instantFn.mock.calls.map(([, t]) => (t as Date).toISOString());
    expect(times).toContain(T0.toISOString());
    expect(times).toContain(T1.toISOString());
    expect(times).toContain(T3.toISOString());
  });

  it('issues range queries from startedAt to recoveryStoppedAt for each metric', async () => {
    const rangeFn = vi.fn().mockResolvedValue([]);
    const client = makeMockClient(async () => makeVector(1), rangeFn);

    await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    expect(rangeFn).toHaveBeenCalledTimes(QUERIES.length);
    const [, start, end] = rangeFn.mock.calls[0];
    expect((start as Date).toISOString()).toBe(T0.toISOString());
    expect((end as Date).toISOString()).toBe(T3.toISOString());
  });

  it('uses RANGE_STEP_SECONDS as the default step', async () => {
    const rangeFn = vi.fn().mockResolvedValue([]);
    const client = makeMockClient(async () => makeVector(1), rangeFn);

    await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    const [, , , step] = rangeFn.mock.calls[0];
    expect(step).toBe(RANGE_STEP_SECONDS);
  });

  it('accepts a custom step value', async () => {
    const rangeFn = vi.fn().mockResolvedValue([]);
    const client = makeMockClient(async () => makeVector(1), rangeFn);

    await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES, 30);

    const [, , , step] = rangeFn.mock.calls[0];
    expect(step).toBe(30);
  });

  it('stores null (not 0) when an instant query fails', async () => {
    const [q] = QUERIES;
    const instantFn = vi.fn().mockImplementation(async (query: string) => {
      if (query === q) throw new Error('prometheus down');
      return makeVector(1);
    });
    const client = makeMockClient(instantFn, async () => []);

    const window = await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    expect(window.before[q]).toBeNull();
    expect(window.afterLoad[q]).toBeNull();
    expect(window.afterRecovery[q]).toBeNull();
  });

  it('stores empty array (not null) when a range query fails', async () => {
    const [q] = QUERIES;
    const rangeFn = vi.fn().mockImplementation(async (query: string) => {
      if (query === q) throw new Error('prometheus down');
      return [];
    });
    const client = makeMockClient(async () => makeVector(1), rangeFn);

    const window = await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    expect(window.ranges[q]).toEqual([]);
  });

  it('collects afterRecovery values even when earlier instant queries failed', async () => {
    // Simulate tier failure: instant queries for T0 and T1 throw, T3 succeeds.
    const instantFn = vi.fn().mockImplementation(async (_query: string, time?: Date) => {
      if (!time || time.getTime() === T3.getTime()) return makeVector(99);
      throw new Error('tier failed');
    });
    const client = makeMockClient(instantFn, async () => []);

    const window = await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    // Recovery metrics must be collected regardless of tier-phase failures.
    for (const q of QUERIES) {
      expect(window.afterRecovery[q]).toBe(99);
    }
  });

  it('populates tierIndex, targetTps, and timing ISO strings', async () => {
    const client = makeMockClient(
      async () => makeVector(1),
      async () => []
    );
    const window = await collectTierWindow(client, 2, 400, T0, T1, T2, T3, QUERIES);

    expect(window.tierIndex).toBe(2);
    expect(window.targetTps).toBe(400);
    expect(window.startedAt).toBe(T0.toISOString());
    expect(window.stoppedAt).toBe(T1.toISOString());
    expect(window.recoveryStartedAt).toBe(T2.toISOString());
    expect(window.recoveryStoppedAt).toBe(T3.toISOString());
  });

  it('populates all query keys in before, afterLoad, afterRecovery, and ranges', async () => {
    const client = makeMockClient(
      async () => makeVector(1),
      async () => []
    );
    const window = await collectTierWindow(client, 0, 100, T0, T1, T2, T3, QUERIES);

    for (const q of QUERIES) {
      expect(Object.prototype.hasOwnProperty.call(window.before, q)).toBe(true);
      expect(Object.prototype.hasOwnProperty.call(window.afterLoad, q)).toBe(true);
      expect(Object.prototype.hasOwnProperty.call(window.afterRecovery, q)).toBe(true);
      expect(Object.prototype.hasOwnProperty.call(window.ranges, q)).toBe(true);
    }
  });
});
