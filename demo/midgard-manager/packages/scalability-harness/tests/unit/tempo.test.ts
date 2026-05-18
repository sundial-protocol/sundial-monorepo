import { describe, expect, it } from 'vitest';

import { TempoClient, TempoQueryError } from '../../src/evidence/tempo.js';

const ENDPOINT = 'http://localhost:3200';

function makeFetcher(status: number, body: unknown): (url: string) => Promise<Response> {
  return async (_url) => new Response(JSON.stringify(body), { status });
}

function makeThrowingFetcher(message: string): (url: string) => Promise<Response> {
  return async (_url) => {
    throw new Error(message);
  };
}

const START = new Date('2026-01-01T00:00:00.000Z');
const END = new Date('2026-01-01T01:00:00.000Z');

const SAMPLE_RESPONSE = {
  traces: [
    {
      traceID: 'abc123',
      rootName: 'block-commitment-fiber',
      rootServiceName: 'midgard-node',
      startTimeUnixNano: '1746748800000000000',
      durationMs: 42,
    },
    {
      traceID: 'def456',
      rootName: 'submit-blocks-fiber',
      rootServiceName: 'midgard-node',
      startTimeUnixNano: '1746748900000000000',
      durationMs: 15,
    },
  ],
  metrics: {
    inspectedTraces: 150,
    inspectedBytes: 8192,
  },
};

describe('TempoClient.searchTraces', () => {
  it('parses a successful response into trace summaries', async () => {
    const client = new TempoClient(ENDPOINT, makeFetcher(200, SAMPLE_RESPONSE));
    const result = await client.searchTraces('midgard-node', START, END);

    expect(result.traces).toHaveLength(2);
    expect(result.traces[0]).toEqual({
      traceId: 'abc123',
      rootName: 'block-commitment-fiber',
      rootServiceName: 'midgard-node',
      startTimeUnixNano: '1746748800000000000',
      durationMs: 42,
    });
    expect(result.inspectedTraces).toBe(150);
    expect(result.truncated).toBe(false);
  });

  it('sets truncated=true when trace count equals the limit', async () => {
    const client = new TempoClient(ENDPOINT, makeFetcher(200, SAMPLE_RESPONSE));
    const result = await client.searchTraces('midgard-node', START, END, 2);

    expect(result.truncated).toBe(true);
  });

  it('returns empty traces on a success response with no traces field', async () => {
    const emptyResponse = { metrics: { inspectedTraces: 0 } };
    const client = new TempoClient(ENDPOINT, makeFetcher(200, emptyResponse));
    const result = await client.searchTraces('midgard-node', START, END);

    expect(result.traces).toHaveLength(0);
    expect(result.truncated).toBe(false);
  });

  it('returns inspectedTraces=null when metrics are absent', async () => {
    const noMetricsResponse = { traces: [] };
    const client = new TempoClient(ENDPOINT, makeFetcher(200, noMetricsResponse));
    const result = await client.searchTraces('midgard-node', START, END);

    expect(result.inspectedTraces).toBeNull();
  });

  it('throws TempoQueryError on HTTP error status', async () => {
    const client = new TempoClient(ENDPOINT, makeFetcher(503, {}));

    await expect(client.searchTraces('midgard-node', START, END)).rejects.toThrow(TempoQueryError);
    await expect(client.searchTraces('midgard-node', START, END)).rejects.toThrow('HTTP 503');
  });

  it('throws TempoQueryError on network failure', async () => {
    const client = new TempoClient(ENDPOINT, makeThrowingFetcher('ECONNREFUSED'));

    await expect(client.searchTraces('midgard-node', START, END)).rejects.toThrow(TempoQueryError);
    await expect(client.searchTraces('midgard-node', START, END)).rejects.toThrow('ECONNREFUSED');
  });

  it('includes endpoint and serviceName in thrown TempoQueryError', async () => {
    const client = new TempoClient(ENDPOINT, makeThrowingFetcher('timeout'));

    let caught: unknown;
    try {
      await client.searchTraces('my-service', START, END);
    } catch (err) {
      caught = err;
    }

    expect(caught).toBeInstanceOf(TempoQueryError);
    const e = caught as TempoQueryError;
    expect(e.endpoint).toBe(ENDPOINT);
    expect(e.serviceName).toBe('my-service');
  });

  it('uses Unix second timestamps in the query URL', async () => {
    let capturedUrl: string | undefined;
    const capturingFetcher = async (url: string): Promise<Response> => {
      capturedUrl = url;
      return new Response(JSON.stringify({ traces: [], metrics: {} }), { status: 200 });
    };

    const client = new TempoClient(
      ENDPOINT,
      capturingFetcher as Parameters<typeof TempoClient>[1]
    );
    await client.searchTraces('midgard-node', START, END);

    const expectedStartSec = Math.floor(START.getTime() / 1000).toString();
    const expectedEndSec = Math.ceil(END.getTime() / 1000).toString();
    expect(capturedUrl).toContain(`start=${expectedStartSec}`);
    expect(capturedUrl).toContain(`end=${expectedEndSec}`);
    expect(capturedUrl).toContain('service.name=midgard-node');
  });
});
