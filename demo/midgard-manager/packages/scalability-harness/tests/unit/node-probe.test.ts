import { describe, expect, it, vi } from 'vitest';

import type { ArtifactWriter } from '../../src/evidence/artifacts.js';
import type { Fetcher } from '../../src/runner/node-probe.js';
import { probeNode, runProbeLoop } from '../../src/runner/node-probe.js';

// ---------------------------------------------------------------------------
// Fetcher helpers
// ---------------------------------------------------------------------------

function makeFetcher(status: number): Fetcher {
  return vi.fn().mockResolvedValue({ status } as Response);
}

function makeErrorFetcher(err: Error): Fetcher {
  return vi.fn().mockRejectedValue(err);
}

function makeAbortFetcher(): Fetcher {
  return vi.fn().mockRejectedValue(Object.assign(new DOMException('signal aborted', 'AbortError')));
}

function makeWriter(overrides?: Partial<ArtifactWriter>): ArtifactWriter {
  return {
    appendLoadEvent: vi.fn().mockResolvedValue(undefined),
    logStdout: vi.fn().mockResolvedValue(undefined),
    logStderr: vi.fn().mockResolvedValue(undefined),
    ...overrides,
  } as unknown as ArtifactWriter;
}

// ---------------------------------------------------------------------------
// probeNode
// ---------------------------------------------------------------------------

describe('probeNode', () => {
  it('returns ok: true for a 200 response', async () => {
    const result = await probeNode('http://localhost:3000', 1000, makeFetcher(200));
    expect(result.ok).toBe(true);
    expect(result.statusCode).toBe(200);
  });

  it('returns ok: false for a 404 response', async () => {
    const result = await probeNode('http://localhost:3000', 1000, makeFetcher(404));
    expect(result.ok).toBe(false);
    expect(result.statusCode).toBe(404);
  });

  it('returns ok: false for a 500 response', async () => {
    const result = await probeNode('http://localhost:3000', 1000, makeFetcher(500));
    expect(result.ok).toBe(false);
    expect(result.statusCode).toBe(500);
  });

  it('returns ok: false for a connection error', async () => {
    const result = await probeNode(
      'http://localhost:3000',
      1000,
      makeErrorFetcher(new TypeError('ECONNREFUSED'))
    );
    expect(result.ok).toBe(false);
    expect(result.error).toContain('ECONNREFUSED');
    expect(result.statusCode).toBeUndefined();
  });

  it('returns ok: false with "probe timed out" for AbortError', async () => {
    const result = await probeNode('http://localhost:3000', 1000, makeAbortFetcher());
    expect(result.ok).toBe(false);
    expect(result.error).toBe('probe timed out');
  });

  it('always includes a non-negative latencyMs', async () => {
    const ok = await probeNode('http://localhost:3000', 1000, makeFetcher(200));
    const fail = await probeNode('http://localhost:3000', 1000, makeErrorFetcher(new Error('x')));
    expect(ok.latencyMs).toBeGreaterThanOrEqual(0);
    expect(fail.latencyMs).toBeGreaterThanOrEqual(0);
  });

  it('requests the health/live probe URL', async () => {
    const fetcher = makeFetcher(200);
    await probeNode('http://localhost:3000', 1000, fetcher);
    const [url] = vi.mocked(fetcher).mock.calls[0];
    expect(url).toBe('http://localhost:3000/health/live');
  });
});

// ---------------------------------------------------------------------------
// runProbeLoop
// ---------------------------------------------------------------------------

const BASE_CONFIG = {
  nodeEndpoint: 'http://localhost:3000',
  runId: 'test-run',
  tierIndex: 0,
  maxConsecutiveFailures: 3,
  intervalMs: 10,
  timeoutMs: 500,
};

describe('runProbeLoop', () => {
  it('writes a node_probe event for each probe', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      if (calls >= 2) controller.abort();
      return { status: 200 } as Response;
    });

    await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const probeEvents = events.filter((e) => e.event === 'node_probe');
    expect(probeEvents.length).toBeGreaterThanOrEqual(1);
    expect(probeEvents[0]).toMatchObject({ event: 'node_probe', ok: true });
  });

  it('increments consecutive failures and resets on success', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    // fail, fail, succeed, then abort
    const statuses = [500, 500, 200];
    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      const status = statuses[calls] ?? 200;
      calls++;
      if (calls >= statuses.length) controller.abort();
      return { status } as Response;
    });

    const result = await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    // After the 200 the counter must have been reset — the loop continued
    expect(result.consecutiveFailures).toBe(0);
    expect(result.failedProbes).toBe(2);
  });

  it('fires stop_condition after maxConsecutiveFailures consecutive failures', async () => {
    const writer = makeWriter();
    const controller = new AbortController();

    const fetcher: Fetcher = vi.fn().mockResolvedValue({ status: 503 } as Response);

    const result = await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    expect(result.stopConditionTriggered).toBe(true);
    expect(result.consecutiveFailures).toBe(BASE_CONFIG.maxConsecutiveFailures);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const stopEvent = events.find((e) => e.event === 'stop_condition');
    expect(stopEvent).toBeDefined();
    if (stopEvent?.event === 'stop_condition') {
      expect(stopEvent.reason).toBe('max_consecutive_probe_failures');
      expect(stopEvent.tierIndex).toBe(0);
    }
  });

  it('does not fire stop_condition when failures are below the threshold', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    // Two failures then a recovery, then abort — threshold is 3
    const statuses = [503, 503, 200];
    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      const status = statuses[calls] ?? 404;
      calls++;
      if (calls > statuses.length) controller.abort();
      return { status } as Response;
    });

    const result = await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    expect(result.stopConditionTriggered).toBe(false);
    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    expect(events.find((e) => e.event === 'stop_condition')).toBeUndefined();
  });

  it('exits cleanly when the external abort signal fires', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      return { status: 200 } as Response;
    });

    // Abort after a small delay
    setTimeout(() => controller.abort(), 25);
    const result = await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    expect(result.stopConditionTriggered).toBe(false);
    expect(calls).toBeGreaterThanOrEqual(1);
  });

  it('includes tierIndex and runId in all emitted events', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      if (calls >= 1) controller.abort();
      return { status: 200 } as Response;
    });

    await runProbeLoop(
      { ...BASE_CONFIG, tierIndex: 2, runId: 'my-run' },
      writer,
      controller.signal,
      fetcher
    );

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    for (const e of events) {
      expect(e.runId).toBe('my-run');
    }
    const probeEvents = events.filter((e) => e.event === 'node_probe');
    for (const e of probeEvents) {
      expect(e.tierIndex).toBe(2);
    }
  });

  it('counts totalProbes and failedProbes correctly', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    // 2 ok, 1 fail, then abort
    const statuses = [200, 200, 500];
    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      const status = statuses[calls] ?? 200;
      calls++;
      if (calls >= statuses.length) controller.abort();
      return { status } as Response;
    });

    const result = await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    expect(result.totalProbes).toBe(3);
    expect(result.failedProbes).toBe(1);
  });

  it('records the error message in node_probe events for connection errors', async () => {
    const writer = makeWriter();
    const controller = new AbortController();
    let calls = 0;

    const fetcher: Fetcher = vi.fn().mockImplementation(async () => {
      calls++;
      if (calls >= BASE_CONFIG.maxConsecutiveFailures) controller.abort();
      throw new TypeError('fetch failed');
    });

    await runProbeLoop(BASE_CONFIG, writer, controller.signal, fetcher);

    const events = vi.mocked(writer.appendLoadEvent).mock.calls.map(([e]) => e);
    const probeEvent = events.find((e) => e.event === 'node_probe');
    expect(probeEvent).toBeDefined();
    if (probeEvent?.event === 'node_probe') {
      expect(probeEvent.errorMessage).toContain('fetch failed');
    }
  });
});
