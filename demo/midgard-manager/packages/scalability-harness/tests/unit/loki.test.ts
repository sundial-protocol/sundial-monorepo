import { describe, expect, it } from 'vitest';

import { LokiClient, LokiQueryError } from '../../src/evidence/loki.js';

const ENDPOINT = 'http://localhost:3100';

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
  status: 'success',
  data: {
    resultType: 'streams',
    result: [
      {
        stream: { container: 'midgard-node', job: 'containerlogs' },
        values: [
          ['1746748800000000000', 'INFO fiber started'],
          ['1746748801000000000', 'INFO block committed'],
        ],
      },
    ],
  },
};

describe('LokiClient.queryRange', () => {
  it('parses a successful response into streams and entries', async () => {
    const client = new LokiClient(ENDPOINT, makeFetcher(200, SAMPLE_RESPONSE));
    const result = await client.queryRange('{job="containerlogs"}', START, END);

    expect(result.streams).toHaveLength(1);
    expect(result.streams[0]!.labels).toEqual({ container: 'midgard-node', job: 'containerlogs' });
    expect(result.streams[0]!.entries).toHaveLength(2);
    expect(result.streams[0]!.entries[0]).toEqual({
      timestampNs: '1746748800000000000',
      line: 'INFO fiber started',
    });
    expect(result.totalEntries).toBe(2);
    expect(result.truncated).toBe(false);
  });

  it('sets truncated=true when entry count equals the limit', async () => {
    const client = new LokiClient(ENDPOINT, makeFetcher(200, SAMPLE_RESPONSE));
    const result = await client.queryRange('{job="containerlogs"}', START, END, 2);

    expect(result.truncated).toBe(true);
  });

  it('returns empty streams on a success response with no results', async () => {
    const emptyResponse = {
      status: 'success',
      data: { resultType: 'streams', result: [] },
    };
    const client = new LokiClient(ENDPOINT, makeFetcher(200, emptyResponse));
    const result = await client.queryRange('{job="containerlogs"}', START, END);

    expect(result.streams).toHaveLength(0);
    expect(result.totalEntries).toBe(0);
    expect(result.truncated).toBe(false);
  });

  it('throws LokiQueryError on HTTP error status', async () => {
    const client = new LokiClient(ENDPOINT, makeFetcher(503, { status: 'error', error: 'down' }));

    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      LokiQueryError
    );
    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      'HTTP 503'
    );
  });

  it('throws LokiQueryError when Loki returns status=error in the body', async () => {
    const errorResponse = { status: 'error', errorType: 'bad_data', error: 'parse error' };
    const client = new LokiClient(ENDPOINT, makeFetcher(200, errorResponse));

    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      LokiQueryError
    );
    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      'parse error'
    );
  });

  it('throws LokiQueryError on network failure', async () => {
    const client = new LokiClient(ENDPOINT, makeThrowingFetcher('ECONNREFUSED'));

    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      LokiQueryError
    );
    await expect(client.queryRange('{job="containerlogs"}', START, END)).rejects.toThrow(
      'ECONNREFUSED'
    );
  });

  it('includes the endpoint and query in thrown LokiQueryError', async () => {
    const client = new LokiClient(ENDPOINT, makeThrowingFetcher('timeout'));

    let caught: unknown;
    try {
      await client.queryRange('{container="midgard-node"}', START, END);
    } catch (err) {
      caught = err;
    }

    expect(caught).toBeInstanceOf(LokiQueryError);
    const e = caught as LokiQueryError;
    expect(e.endpoint).toBe(ENDPOINT);
    expect(e.query).toBe('{container="midgard-node"}');
  });

  it('uses nanosecond timestamps in the query URL', async () => {
    let capturedUrl: string | undefined;
    const capturingFetcher = async (url: string): Promise<Response> => {
      capturedUrl = url;
      return new Response(
        JSON.stringify({ status: 'success', data: { resultType: 'streams', result: [] } }),
        { status: 200 }
      );
    };

    const client = new LokiClient(ENDPOINT, capturingFetcher as Parameters<typeof LokiClient>[1]);
    await client.queryRange('{job="containerlogs"}', START, END);

    const expectedStartNs = (START.getTime() * 1_000_000).toString();
    const expectedEndNs = (END.getTime() * 1_000_000).toString();
    expect(capturedUrl).toContain(`start=${expectedStartNs}`);
    expect(capturedUrl).toContain(`end=${expectedEndNs}`);
  });
});
