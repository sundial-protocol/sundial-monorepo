import { describe, expect, it } from 'vitest';

import { analyze } from '../../src/reliability/analyze.js';
import type { CollectedData } from '../../src/reliability/collect.js';
import { resolveWindow, windowRangeString } from '../../src/reliability/config.js';
import { detectIncidents } from '../../src/reliability/incidents.js';
import { findLeaks, redactText } from '../../src/reliability/redact.js';
import { renderMarkdown } from '../../src/reliability/render.js';

// --- config ---

describe('resolveWindow', () => {
  it('resolves a calendar month to a UTC [start, next-month-start) window', () => {
    const w = resolveWindow({ month: '2026-08' });
    expect(w.from.toISOString()).toBe('2026-08-01T00:00:00.000Z');
    expect(w.to.toISOString()).toBe('2026-09-01T00:00:00.000Z');
    expect(windowRangeString(w)).toBe(`${31 * 86400}s`);
  });

  it('accepts --from/--to as YYYY-MM-DD (UTC midnight)', () => {
    const w = resolveWindow({ from: '2026-08-01', to: '2026-08-08' });
    expect(w.from.toISOString()).toBe('2026-08-01T00:00:00.000Z');
    expect(w.to.toISOString()).toBe('2026-08-08T00:00:00.000Z');
  });

  it('rejects a bad month and an inverted range', () => {
    expect(() => resolveWindow({ month: '2026-8' })).toThrow();
    expect(() => resolveWindow({ from: '2026-08-08', to: '2026-08-01' })).toThrow();
    expect(() => resolveWindow({})).toThrow();
  });
});

// --- redaction ---

describe('redactText', () => {
  it('scrubs addresses, hashes, utxo refs, cbor and endpoints', () => {
    const raw = [
      'sent to addr_test1qq2z3x9k7m8n4p5r6s7t8u9v0w1x2y3z4a5b6c7d8e9f0g1h2j3k4l5m6n7p8q9',
      'tx a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2 committed',
      'utxo a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2#3',
      'scraping http://prometheus.internal:9090/api/v1/query',
    ].join('\n');
    const out = redactText(raw);
    expect(out).not.toMatch(/addr_test1/);
    expect(out).toContain('<addr>');
    expect(out).toContain('<hash>');
    expect(out).toContain('<utxo-ref>');
    expect(out).toContain('<endpoint>');
    expect(findLeaks(out)).toEqual([]);
  });
});

// --- analyze + incidents + render ---

const step = 60;
const t0 = Date.parse('2026-08-01T00:00:00Z');
const mkRange = (key: string, query: string, values: [number, string][]) => ({
  key,
  query,
  result: [{ metric: {}, values }],
});
const constSeries = (v: number, n = 60): [number, string][] =>
  Array.from({ length: n }, (_, i) => [t0 / 1000 + i * step, String(v)] as [number, string]);

function baseData(overrides: Partial<CollectedData> = {}): CollectedData {
  return {
    collectedAt: '2026-08-01T02:00:00Z',
    window: { from: '2026-08-01T00:00:00Z', to: '2026-08-01T01:00:00Z', label: '2026-08-01' },
    environment: 'testnet',
    prometheusEndpoint: 'http://prom:9090',
    sloSourcePath: '/repo/demo/midgard-node/slo/slo.json',
    slo: [
      {
        id: 'submit_ingress_availability',
        kind: 'ratio',
        objective: 0.99,
        description: 'Fraction of POST /submit answered 2xx.',
        aggregate: {
          ratio: {
            key: 'a',
            query: 'good/total',
            result: [{ metric: {}, value: [t0 / 1000, '0.995'] }],
          },
        },
        rolling: { ratio: mkRange('r', 'good/total', constSeries(0.995)) },
      },
      {
        id: 'l1_commitment_success',
        kind: 'ratio',
        objective: 0.999,
        description: 'Commit blocks without worker failure.',
        aggregate: {
          ratio: { key: 'a', query: 'x', result: [{ metric: {}, value: [t0 / 1000, '0.99'] }] },
        },
        rolling: { ratio: mkRange('r', 'x', constSeries(0.99)) },
      },
      {
        id: 'submit_ingress_latency',
        kind: 'latency',
        objective: 1,
        description: 'POST /submit latency.',
        aggregate: {
          '0.5': { key: 'a', query: 'q', result: [{ metric: {}, value: [t0 / 1000, '0.05'] }] },
          '0.95': { key: 'a', query: 'q', result: [{ metric: {}, value: [t0 / 1000, '0.4'] }] },
          '0.99': { key: 'a', query: 'q', result: [{ metric: {}, value: [t0 / 1000, '0.9'] }] },
        },
        rolling: { '0.95': mkRange('r', 'q', constSeries(0.4)) },
      },
    ],
    supportingInstant: [
      {
        key: 'build_info',
        query: 'midgard_node_build_info',
        result: [{ metric: { version: '1.0.0', commit: 'abc1234' }, value: [t0 / 1000, '1'] }],
      },
    ],
    supportingRange: [
      mkRange('up', 'up', [
        ...constSeries(1, 30),
        ...Array.from(
          { length: 5 },
          (_, i) => [t0 / 1000 + (30 + i) * step, '0'] as [number, string]
        ),
        ...Array.from(
          { length: 25 },
          (_, i) => [t0 / 1000 + (35 + i) * step, '1'] as [number, string]
        ),
      ]),
      mkRange('start_time', 'start_time', [
        ...constSeries(1000, 30),
        ...Array.from(
          { length: 30 },
          (_, i) => [t0 / 1000 + (30 + i) * step, '2000'] as [number, string]
        ),
      ]),
      mkRange('commit_block_rate_5m', 'rate', constSeries(0.1)),
      mkRange('commit_commitment_failures', 'x', [
        ...constSeries(0, 40),
        ...Array.from(
          { length: 20 },
          (_, i) => [t0 / 1000 + (40 + i) * step, '2'] as [number, string]
        ),
      ]),
      mkRange('merge_failures', 'x', constSeries(0)),
      mkRange('submit_block_failures', 'x', constSeries(0)),
      mkRange('tx_stream_fail', 'x', constSeries(0)),
      mkRange('tx_stream_dead_letter', 'x', constSeries(0)),
      mkRange('container_cpu', 'x', constSeries(0.5)),
      mkRange('container_memory_mib', 'x', constSeries(512)),
      mkRange('l1_commitment_fees_lovelace', 'x', constSeries(0)),
    ],
    ...overrides,
  };
}

describe('analyze', () => {
  it('computes SLO verdicts, error budget and disposition', () => {
    const a = analyze(baseData(), step);

    const avail = a.slos.find((s) => s.id === 'submit_ingress_availability')!;
    expect(avail.verdict).toBe('met');
    expect(avail.achieved).toBeCloseTo(0.995);
    expect(avail.errorBudget!.consumedFraction).toBeCloseTo(0.5, 1); // (1-0.995)/(1-0.99)

    const l1 = a.slos.find((s) => s.id === 'l1_commitment_success')!;
    expect(l1.verdict).toBe('missed'); // 0.99 < 0.999
    expect(a.disposition).toBe('Failed');

    const lat = a.slos.find((s) => s.id === 'submit_ingress_latency')!;
    expect(lat.verdict).toBe('met'); // p95 0.4 <= 1
    expect(lat.quantiles!['0.99']).toBeCloseTo(0.9);
  });

  it('detects the availability gap, downtime and restart', () => {
    const a = analyze(baseData(), step);
    expect(a.availability.fraction).toBeCloseTo(55 / 60, 2);
    expect(a.availability.scrapeGaps.length).toBe(1);
    expect(a.availability.downtimeSeconds).toBeGreaterThanOrEqual(4 * step);
    expect(a.availability.restarts).toBe(1);
    expect(a.availability.builds[0]).toMatchObject({ version: '1.0.0', commit: 'abc1234' });
  });

  it('totals failure counters', () => {
    const a = analyze(baseData(), step);
    expect(a.counterTotals.commit_commitment_failures).toBe(2);
  });
});

describe('detectIncidents', () => {
  it('produces gap, restart, breach and failure-spike incidents', () => {
    const data = baseData();
    const a = analyze(data, step);
    const incidents = detectIncidents(a, data, step);
    const kinds = new Set(incidents.map((i) => i.kind));
    expect(kinds.has('availability-gap')).toBe(true);
    expect(kinds.has('restart')).toBe(true);
    expect(kinds.has('failure-spike')).toBe(true);
    // sorted by start time
    const times = incidents.map((i) => i.startedAt);
    expect([...times].sort()).toEqual(times);
  });
});

describe('renderMarkdown', () => {
  it('renders internal + public variants; public has no endpoints and no leaks', () => {
    const data = baseData();
    const a = analyze(data, step);
    const incidents = detectIncidents(a, data, step);
    const input = {
      analysis: a,
      data,
      incidents,
      charts: [],
      manifestName: 'MANIFEST.sha256',
      evidenceFiles: ['collected.json'],
      sloSourceRelPath: 'demo/midgard-node/slo/slo.json',
      rollingWindow: '1h',
    };
    const internal = renderMarkdown(input, { public: false });
    const pub = renderMarkdown(input, { public: true });

    expect(internal).toContain('## 1. Executive summary');
    expect(internal).toContain('## 9. Appendix');
    expect(internal).toContain('http://prom:9090');

    expect(pub).not.toContain('## 9. Appendix');
    expect(pub).not.toContain('http://prom:9090');
    expect(pub).toContain('Disposition:');
    expect(findLeaks(pub)).toEqual([]);
  });
});
