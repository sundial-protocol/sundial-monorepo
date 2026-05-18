import { describe, expect, it, vi } from 'vitest';

import type { Fetcher } from '../../src/metrics/prometheus.js';
import {
  CADVISOR_METRICS,
  flattenToScalars,
  NODE_METRICS,
  PrometheusClient,
  PrometheusQueryError,
  snapshotNodeMetrics,
} from '../../src/metrics/prometheus.js';

// ---------------------------------------------------------------------------
// Response builders
// ---------------------------------------------------------------------------

function vectorResponse(results: Array<{ labels?: Record<string, string>; value: string }>) {
  return {
    status: 'success',
    data: {
      resultType: 'vector',
      result: results.map(({ labels = {}, value }) => ({
        metric: labels,
        value: [Date.now() / 1000, value] as [number, string],
      })),
    },
  };
}

function matrixResponse(
  results: Array<{ labels?: Record<string, string>; values: Array<[number, string]> }>
) {
  return {
    status: 'success',
    data: {
      resultType: 'matrix',
      result: results.map(({ labels = {}, values }) => ({ metric: labels, values })),
    },
  };
}

function errorResponse(errorType: string, error: string) {
  return { status: 'error', errorType, error };
}

function makeFetcher(body: unknown, status = 200): Fetcher {
  return vi.fn().mockResolvedValue({
    ok: status >= 200 && status < 300,
    status,
    json: async () => body,
  } as Response);
}

function makeErrorFetcher(err: Error): Fetcher {
  return vi.fn().mockRejectedValue(err);
}

// ---------------------------------------------------------------------------
// PrometheusClient.queryInstant
// ---------------------------------------------------------------------------

describe('PrometheusClient.queryInstant', () => {
  it('parses a vector response with one series', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(vectorResponse([{ value: '42' }]))
    );
    const result = await client.queryInstant('my_metric');
    expect(result).toHaveLength(1);
    expect(result[0].value[1]).toBe('42');
  });

  it('parses a vector response with multiple series', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(
        vectorResponse([
          { labels: { instance: 'a' }, value: '1' },
          { labels: { instance: 'b' }, value: '2' },
        ])
      )
    );
    const result = await client.queryInstant('my_metric');
    expect(result).toHaveLength(2);
    expect(result[0].metric.instance).toBe('a');
    expect(result[1].metric.instance).toBe('b');
  });

  it('returns an empty array for a vector with no series', async () => {
    const client = new PrometheusClient('http://localhost:9090', makeFetcher(vectorResponse([])));
    const result = await client.queryInstant('my_metric');
    expect(result).toEqual([]);
  });

  it('passes the query as a URL parameter', async () => {
    const fetcher = makeFetcher(vectorResponse([{ value: '1' }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    await client.queryInstant('up{job="midgard_nodes"}');
    const [url] = vi.mocked(fetcher).mock.calls[0];
    expect(url).toContain('/api/v1/query');
    expect(url).toContain(encodeURIComponent('up{job="midgard_nodes"}'));
  });

  it('includes the time parameter when provided', async () => {
    const fetcher = makeFetcher(vectorResponse([{ value: '1' }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const t = new Date('2026-05-18T10:00:00.000Z');
    await client.queryInstant('my_metric', t);
    const [url] = vi.mocked(fetcher).mock.calls[0];
    expect(url).toContain(`time=${t.getTime() / 1000}`);
  });

  it('throws PrometheusQueryError for status:"error" response', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(errorResponse('execution', 'vector cannot be parsed'))
    );
    await expect(client.queryInstant('bad_query')).rejects.toThrow(PrometheusQueryError);
    await expect(client.queryInstant('bad_query')).rejects.toThrow('vector cannot be parsed');
  });

  it('exposes errorType on PrometheusQueryError', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(errorResponse('execution', 'oops'))
    );
    await expect(client.queryInstant('q')).rejects.toMatchObject({
      name: 'PrometheusQueryError',
      errorType: 'execution',
      query: 'q',
    });
  });

  it('throws PrometheusQueryError for HTTP 400', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher({ status: 'error', error: 'bad expr' }, 400)
    );
    await expect(client.queryInstant('q')).rejects.toThrow(PrometheusQueryError);
  });

  it('throws PrometheusQueryError for HTTP 503', async () => {
    const client = new PrometheusClient('http://localhost:9090', makeFetcher({}, 503));
    await expect(client.queryInstant('q')).rejects.toThrow(PrometheusQueryError);
  });

  it('throws PrometheusQueryError on network error', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeErrorFetcher(new TypeError('ECONNREFUSED'))
    );
    await expect(client.queryInstant('q')).rejects.toThrow(PrometheusQueryError);
    await expect(client.queryInstant('q')).rejects.toThrow('ECONNREFUSED');
  });

  it('throws PrometheusQueryError when resultType is not vector', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher({
        status: 'success',
        data: { resultType: 'matrix', result: [] },
      })
    );
    await expect(client.queryInstant('q')).rejects.toThrow('Expected vector result');
  });
});

// ---------------------------------------------------------------------------
// PrometheusClient.queryRange
// ---------------------------------------------------------------------------

describe('PrometheusClient.queryRange', () => {
  const start = new Date('2026-05-18T10:00:00.000Z');
  const end = new Date('2026-05-18T11:00:00.000Z');

  it('parses a matrix response', async () => {
    const values: Array<[number, string]> = [
      [1747555200, '10'],
      [1747555260, '20'],
    ];
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(matrixResponse([{ values }]))
    );
    const result = await client.queryRange('my_metric', start, end, 60);
    expect(result).toHaveLength(1);
    expect(result[0].values).toEqual(values);
  });

  it('passes start, end, and step as URL parameters', async () => {
    const fetcher = makeFetcher(matrixResponse([{ values: [] }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    await client.queryRange('my_metric', start, end, 30);
    const [url] = vi.mocked(fetcher).mock.calls[0];
    expect(url).toContain('/api/v1/query_range');
    expect(url).toContain(`start=${start.getTime() / 1000}`);
    expect(url).toContain(`end=${end.getTime() / 1000}`);
    expect(url).toContain('step=30');
  });

  it('throws PrometheusQueryError for status:"error" response', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher(errorResponse('bad_data', 'invalid query'))
    );
    await expect(client.queryRange('q', start, end, 60)).rejects.toThrow(PrometheusQueryError);
  });

  it('throws PrometheusQueryError when resultType is not matrix', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeFetcher({
        status: 'success',
        data: { resultType: 'vector', result: [] },
      })
    );
    await expect(client.queryRange('q', start, end, 60)).rejects.toThrow('Expected matrix result');
  });

  it('throws PrometheusQueryError on network error', async () => {
    const client = new PrometheusClient(
      'http://localhost:9090',
      makeErrorFetcher(new TypeError('Connection refused'))
    );
    await expect(client.queryRange('q', start, end, 60)).rejects.toThrow(PrometheusQueryError);
  });
});

// ---------------------------------------------------------------------------
// flattenToScalars
// ---------------------------------------------------------------------------

describe('flattenToScalars', () => {
  it('maps query strings to their first series float value', () => {
    const snapshots = [
      {
        query: 'tx_submissions_enqueued_total',
        capturedAt: '',
        result: [{ metric: {}, value: [1234, '99'] as [number, string] }],
      },
    ];
    expect(flattenToScalars(snapshots)).toEqual({ tx_submissions_enqueued_total: 99 });
  });

  it('strips label selectors from keys', () => {
    const snapshots = [
      {
        query: 'up{job="midgard_nodes"}',
        capturedAt: '',
        result: [{ metric: {}, value: [1234, '1'] as [number, string] }],
      },
    ];
    const out = flattenToScalars(snapshots);
    expect(Object.keys(out)).toContain('up');
    expect(out['up']).toBe(1);
  });

  it('strips rate() wrapper from keys', () => {
    const snapshots = [
      {
        query: 'rate(container_cpu_user_seconds_total{image!=""}[1m])',
        capturedAt: '',
        result: [{ metric: {}, value: [1234, '0.5'] as [number, string] }],
      },
    ];
    const out = flattenToScalars(snapshots);
    expect(Object.keys(out)).toContain('container_cpu_user_seconds_total');
  });

  it('skips snapshots with null result', () => {
    const snapshots = [{ query: 'q', capturedAt: '', result: null, error: 'oops' }];
    expect(flattenToScalars(snapshots)).toEqual({});
  });

  it('skips snapshots with empty result array', () => {
    const snapshots = [{ query: 'q', capturedAt: '', result: [] }];
    expect(flattenToScalars(snapshots)).toEqual({});
  });

  it('skips entries where the value is not a parseable float', () => {
    const snapshots = [
      {
        query: 'q',
        capturedAt: '',
        result: [{ metric: {}, value: [1234, 'NaN'] as [number, string] }],
      },
    ];
    expect(flattenToScalars(snapshots)).toEqual({});
  });

  it('uses the first series value when multiple series are present', () => {
    const snapshots = [
      {
        query: 'my_metric',
        capturedAt: '',
        result: [
          { metric: { instance: 'a' }, value: [1, '10'] as [number, string] },
          { metric: { instance: 'b' }, value: [1, '20'] as [number, string] },
        ],
      },
    ];
    expect(flattenToScalars(snapshots)).toEqual({ my_metric: 10 });
  });
});

// ---------------------------------------------------------------------------
// snapshotNodeMetrics
// ---------------------------------------------------------------------------

describe('snapshotNodeMetrics', () => {
  it('queries all NODE_METRICS', async () => {
    const fetcher = makeFetcher(vectorResponse([{ value: '1' }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    const queriedQueries = samples.nodeMetrics.map((s) => s.query);
    for (const m of NODE_METRICS) {
      expect(queriedQueries).toContain(m);
    }
  });

  it('queries all CADVISOR_METRICS', async () => {
    const fetcher = makeFetcher(vectorResponse([{ value: '1' }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    const queriedQueries = samples.cadvisorMetrics.map((s) => s.query);
    for (const m of CADVISOR_METRICS) {
      expect(queriedQueries).toContain(m);
    }
  });

  it('sets evidenceIncomplete: false when all node metrics succeed', async () => {
    const fetcher = makeFetcher(vectorResponse([{ value: '1' }]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    expect(samples.evidenceIncomplete).toBe(false);
    expect(samples.missingPrimaryMetrics).toEqual([]);
  });

  it('sets evidenceIncomplete: true when a node metric fails', async () => {
    let calls = 0;
    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      // Fail the first node metric query only
      if (calls === 1) return { ok: false, status: 503, json: async () => ({}) };
      return { ok: true, status: 200, json: async () => vectorResponse([{ value: '1' }]) };
    });
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    expect(samples.evidenceIncomplete).toBe(true);
    expect(samples.missingPrimaryMetrics).toHaveLength(1);
  });

  it('does not mark evidenceIncomplete when only cAdvisor metrics fail', async () => {
    const nodeCount = NODE_METRICS.length;
    let calls = 0;
    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      // All node metrics succeed; cAdvisor metrics fail
      if (calls > nodeCount) {
        return { ok: false, status: 503, json: async () => ({}) };
      }
      return {
        ok: true,
        status: 200,
        json: async () => vectorResponse([{ value: '1' }]),
      };
    });
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    expect(samples.evidenceIncomplete).toBe(false);
    expect(samples.missingPrimaryMetrics).toEqual([]);
    // cAdvisor errors are captured but don't pollute missingPrimaryMetrics
    const cadvisorErrors = samples.cadvisorMetrics.filter((s) => s.result === null);
    expect(cadvisorErrors.length).toBe(CADVISOR_METRICS.length);
  });

  it('stores the full vector result per snapshot', async () => {
    const fetcher = makeFetcher(
      vectorResponse([{ labels: { instance: 'node:3000' }, value: '77' }])
    );
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    const first = samples.nodeMetrics[0];
    expect(first.result).not.toBeNull();
    expect(first.result![0].value[1]).toBe('77');
  });

  it('captures the error message in the snapshot when a query fails', async () => {
    const fetcher = makeErrorFetcher(new TypeError('connection refused'));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client);
    const failed = samples.nodeMetrics.find((s) => s.result === null);
    expect(failed).toBeDefined();
    expect(failed?.error).toContain('connection refused');
  });

  it('propagates tierIndex into the returned PrometheusSamples', async () => {
    const fetcher = makeFetcher(vectorResponse([]));
    const client = new PrometheusClient('http://localhost:9090', fetcher);
    const samples = await snapshotNodeMetrics(client, 3);
    expect(samples.tierIndex).toBe(3);
  });
});
