import { readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { afterAll, beforeAll, describe, expect, it } from 'vitest';

import type { TierMetricWindow } from '../../src/metrics/window.js';
import {
  buildDataRows,
  type ChartSection,
  generateCharts,
  PANEL_SPECS,
} from '../../src/report/charts.js';

// ---------------------------------------------------------------------------
// Paths
// ---------------------------------------------------------------------------

const DATA_DIR = path.resolve(
  fileURLToPath(new URL('.', import.meta.url)),
  'data/2026-05-19T15-39-28.781Z-warmup'
);

// ---------------------------------------------------------------------------
// Real-data fixture loaded once
// ---------------------------------------------------------------------------

let realWindows: TierMetricWindow[] = [];

beforeAll(async () => {
  const raw = await readFile(path.join(DATA_DIR, 'prometheus-samples.json'), 'utf8');
  const parsed = JSON.parse(raw) as { tiers: TierMetricWindow[] };
  realWindows = parsed.tiers;
});

// ---------------------------------------------------------------------------
// PANEL_SPECS catalog invariants
// ---------------------------------------------------------------------------

describe('PANEL_SPECS catalog', () => {
  it('contains exactly 21 panel specs', () => {
    expect(PANEL_SPECS).toHaveLength(21);
  });

  it('all slugs are unique', () => {
    const slugs = PANEL_SPECS.map((s) => s.slug);
    expect(new Set(slugs).size).toBe(slugs.length);
  });

  const validSections: ChartSection[] = [
    'Throughput',
    'Queue and Mempool',
    'Block Pipeline',
    'Failure Signals',
    'Infrastructure',
  ];

  it('all sections are from the valid set', () => {
    for (const spec of PANEL_SPECS) {
      expect(validSections).toContain(spec.section);
    }
  });

  it('does not include the excluded Block Commitment Worker Duration panel', () => {
    const metrics = PANEL_SPECS.map((s) => s.metric);
    expect(metrics).not.toContain('commit_block_duration_seconds_sum');
    expect(metrics).not.toContain('commit_block_duration_seconds_count');
  });

  it('does not include the excluded Total L1 User Events panel', () => {
    const slugs = PANEL_SPECS.map((s) => s.slug);
    expect(slugs).not.toContain('l1-user-events');
  });

  it('has 4 Throughput panels', () => {
    expect(PANEL_SPECS.filter((s) => s.section === 'Throughput')).toHaveLength(4);
  });

  it('has 2 Queue and Mempool panels', () => {
    expect(PANEL_SPECS.filter((s) => s.section === 'Queue and Mempool')).toHaveLength(2);
  });

  it('has 8 Block Pipeline panels', () => {
    expect(PANEL_SPECS.filter((s) => s.section === 'Block Pipeline')).toHaveLength(8);
  });

  it('has 3 Failure Signals panels', () => {
    expect(PANEL_SPECS.filter((s) => s.section === 'Failure Signals')).toHaveLength(3);
  });

  it('has 4 Infrastructure panels', () => {
    expect(PANEL_SPECS.filter((s) => s.section === 'Infrastructure')).toHaveLength(4);
  });

  it('rate panels reuse the same metric as a corresponding direct panel (counter reuse)', () => {
    const rateSpecs = PANEL_SPECS.filter((s) => s.rate);
    for (const rateSpec of rateSpecs) {
      // Every rate panel must have a corresponding direct panel for the same metric
      const hasDirect = PANEL_SPECS.some((s) => !s.rate && s.metric === rateSpec.metric);
      expect(hasDirect, `no direct panel for rate slug ${rateSpec.slug}`).toBe(true);
    }
  });
});

// ---------------------------------------------------------------------------
// buildDataRows — data transformation (pure, no file I/O)
// ---------------------------------------------------------------------------

describe('buildDataRows — direct metric', () => {
  it('produces one row per data point for a single-series metric', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'tx-queue')!;
    const rows = buildDataRows(realWindows, spec);
    // tx_queue_size has 1 series × 49 points in this run
    expect(rows.length).toBe(49);
  });

  it('row timestamps are ISO strings', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'tx-queue')!;
    const rows = buildDataRows(realWindows, spec);
    for (const row of rows.slice(0, 3)) {
      expect(typeof row.ts).toBe('string');
      expect(() => new Date(row.ts)).not.toThrow();
      expect(isNaN(new Date(row.ts).getTime())).toBe(false);
    }
  });

  it('row values are finite numbers', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'mempool-count')!;
    const rows = buildDataRows(realWindows, spec);
    for (const row of rows) {
      expect(isFinite(row.value)).toBe(true);
    }
  });

  it('produces multiple rows for a multi-series metric (cAdvisor memory)', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'memory-usage')!;
    const rows = buildDataRows(realWindows, spec);
    // 8 container series × 49 points
    expect(rows.length).toBe(8 * 49);
  });

  it('assigns distinct series labels for multi-series metrics', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'memory-usage')!;
    const rows = buildDataRows(realWindows, spec);
    const labels = new Set(rows.map((r) => r.series));
    // 8 containers → at least 2 distinct labels
    expect(labels.size).toBeGreaterThanOrEqual(2);
  });

  it('returns empty array when metric is absent from all windows', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'txs-per-block')!;
    // commit_block_txs_per_block not in the old warmup data (added in task 1)
    const rows = buildDataRows(realWindows, spec);
    expect(rows).toHaveLength(0);
  });

  it('returns empty array for empty windows', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'tx-queue')!;
    const rows = buildDataRows([], spec);
    expect(rows).toHaveLength(0);
  });
});

describe('buildDataRows — rate computation', () => {
  it('rate panel produces fewer rows than direct (one lost to differentiation)', () => {
    const directSpec = PANEL_SPECS.find((s) => s.slug === 'received-cumulative')!;
    const rateSpec = PANEL_SPECS.find((s) => s.slug === 'received-tps')!;
    const directRows = buildDataRows(realWindows, directSpec);
    const rateRows = buildDataRows(realWindows, rateSpec);
    // Rate loses the first point per series (needs a predecessor)
    expect(rateRows.length).toBe(directRows.length - 1);
  });

  it('rate values are non-negative (counter resets produce zero, not negative)', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'received-tps')!;
    const rows = buildDataRows(realWindows, spec);
    for (const row of rows) {
      expect(row.value).toBeGreaterThanOrEqual(0);
    }
  });

  it('rate values are in reasonable TPS range for the warmup run', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'received-tps')!;
    const rows = buildDataRows(realWindows, spec);
    const nonZero = rows.filter((r) => r.value > 0);
    expect(nonZero.length).toBeGreaterThan(0);
    // Warmup targets 100 TPS; peak accepted rate should be below 200
    const max = Math.max(...rows.map((r) => r.value));
    expect(max).toBeLessThan(200);
    expect(max).toBeGreaterThan(0);
  });

  it('direct cumulative values are monotonically non-decreasing', () => {
    const spec = PANEL_SPECS.find((s) => s.slug === 'received-cumulative')!;
    const rows = buildDataRows(realWindows, spec);
    for (let i = 1; i < rows.length; i++) {
      // Values should not decrease (counter is cumulative)
      expect(rows[i].value).toBeGreaterThanOrEqual(rows[i - 1].value - 0.001);
    }
  });
});

describe('buildDataRows — multi-tier merging', () => {
  it('produces combined rows from multiple windows', () => {
    // Duplicate the single window to simulate a two-tier run
    const spec = PANEL_SPECS.find((s) => s.slug === 'tx-queue')!;
    const singleRows = buildDataRows(realWindows, spec);
    // Two identical windows → same label key → values concatenated → 2×49 pts
    const twoWindowRows = buildDataRows([realWindows[0], realWindows[0]], spec);
    expect(twoWindowRows.length).toBe(singleRows.length * 2);
  });

  it('rate computation across two distinct windows preserves continuity', () => {
    // Shift timestamps by 1 hour to simulate a second tier
    const tier1 = realWindows[0];
    const shiftSeconds = 3600;
    const tier2: TierMetricWindow = {
      ...tier1,
      startedAt: new Date(new Date(tier1.startedAt).getTime() + shiftSeconds * 1000).toISOString(),
      stoppedAt: new Date(new Date(tier1.stoppedAt).getTime() + shiftSeconds * 1000).toISOString(),
      recoveryStartedAt: new Date(
        new Date(tier1.recoveryStartedAt ?? tier1.stoppedAt).getTime() + shiftSeconds * 1000
      ).toISOString(),
      recoveryStoppedAt: new Date(
        new Date(tier1.recoveryStoppedAt).getTime() + shiftSeconds * 1000
      ).toISOString(),
      ranges: Object.fromEntries(
        Object.entries(tier1.ranges).map(([k, series]) => [
          k,
          series.map((s) => ({
            ...s,
            values: s.values.map(([ts, v]): [number, string] => [ts + shiftSeconds, v]),
          })),
        ])
      ),
    };
    const spec = PANEL_SPECS.find((s) => s.slug === 'received-tps')!;
    const twoTierRows = buildDataRows([tier1, tier2], spec);
    // Should have points from both tiers — more than a single window
    const singleRows = buildDataRows([tier1], spec);
    expect(twoTierRows.length).toBeGreaterThan(singleRows.length);
    // All values are non-negative
    for (const row of twoTierRows) {
      expect(row.value).toBeGreaterThanOrEqual(0);
    }
  });
});

// ---------------------------------------------------------------------------
// generateCharts — full pipeline against real warmup data
// ---------------------------------------------------------------------------

const PANELS_WITH_DATA_IN_OLD_RUN = new Set([
  'received-tps',
  'mempool-drain-rate',
  'received-cumulative',
  'committed-cumulative',
  'tx-queue',
  'mempool-count',
  'built-blocks',
  'built-blocks-rate',
  'submitted-blocks',
  'submitted-blocks-rate',
  'merged-blocks',
  'merged-blocks-rate',
  // txs-per-block → absent (commit_block_txs_per_block added in task 1)
  // block-size → absent (commit_block_events_size_bytes added in task 1)
  'commit-failures',
  'merge-failures',
  'rejected-submissions',
  'cpu-usage',
  'memory-usage',
  'network-rx',
  'network-tx',
]);

let tmpDir: string;
let chartRecords: Awaited<ReturnType<typeof generateCharts>>;

beforeAll(async () => {
  tmpDir = path.join(tmpdir(), `scalability-charts-test-${Date.now()}`);
  chartRecords = await generateCharts(realWindows, tmpDir);
});

afterAll(async () => {
  await rm(tmpDir, { recursive: true, force: true });
});

describe('generateCharts — record count and structure', () => {
  it('generates exactly one record per panel spec', () => {
    expect(chartRecords).toHaveLength(PANEL_SPECS.length);
  });

  it('every record slug matches a panel spec slug', () => {
    const specSlugs = new Set(PANEL_SPECS.map((s) => s.slug));
    for (const record of chartRecords) {
      expect(specSlugs.has(record.slug)).toBe(true);
    }
  });

  it('every record relPath starts with charts/', () => {
    for (const record of chartRecords) {
      expect(record.relPath.startsWith('charts/')).toBe(true);
    }
  });

  it('every record relPath ends with .svg', () => {
    for (const record of chartRecords) {
      expect(record.relPath.endsWith('.svg')).toBe(true);
    }
  });

  it('section assignments match panel specs', () => {
    for (const record of chartRecords) {
      const spec = PANEL_SPECS.find((s) => s.slug === record.slug)!;
      expect(record.section).toBe(spec.section);
    }
  });
});

describe('generateCharts — hasData flags against real warmup data', () => {
  it('marks panels present in the old run as hasData: true', () => {
    for (const slug of PANELS_WITH_DATA_IN_OLD_RUN) {
      const record = chartRecords.find((r) => r.slug === slug);
      expect(record, `record missing for ${slug}`).toBeDefined();
      expect(record!.hasData, `${slug} should have data`).toBe(true);
    }
  });

  it('marks txs-per-block as hasData: false (metric added in task 1, absent here)', () => {
    const record = chartRecords.find((r) => r.slug === 'txs-per-block')!;
    expect(record.hasData).toBe(false);
  });

  it('marks block-size as hasData: false (metric added in task 1, absent here)', () => {
    const record = chartRecords.find((r) => r.slug === 'block-size')!;
    expect(record.hasData).toBe(false);
  });

  it('total panels with data matches expected count', () => {
    const withData = chartRecords.filter((r) => r.hasData);
    expect(withData).toHaveLength(PANELS_WITH_DATA_IN_OLD_RUN.size);
  });
});

describe('generateCharts — SVG file output', () => {
  it('writes an SVG file for every panel (including no-data panels)', async () => {
    const { readFile: rf } = await import('node:fs/promises');
    for (const record of chartRecords) {
      const filePath = path.join(tmpDir, record.relPath);
      const content = await rf(filePath, 'utf8');
      expect(content.length).toBeGreaterThan(0);
    }
  });

  it('every written file starts with a valid SVG tag', async () => {
    const { readFile: rf } = await import('node:fs/promises');
    for (const record of chartRecords) {
      const filePath = path.join(tmpDir, record.relPath);
      const content = await rf(filePath, 'utf8');
      expect(content.trimStart()).toMatch(/^<svg/);
    }
  });

  it('creates the charts/ subdirectory inside runDir', async () => {
    const { stat } = await import('node:fs/promises');
    const chartsDir = path.join(tmpDir, 'charts');
    const s = await stat(chartsDir);
    expect(s.isDirectory()).toBe(true);
  });

  it('SVG for received-tps is larger than SVG for an empty panel (has more content)', async () => {
    const { readFile: rf } = await import('node:fs/promises');
    const fullPath = path.join(tmpDir, 'charts/received-tps.svg');
    const emptyPath = path.join(tmpDir, 'charts/txs-per-block.svg');
    const full = await rf(fullPath, 'utf8');
    const empty = await rf(emptyPath, 'utf8');
    expect(full.length).toBeGreaterThan(empty.length);
  });
});

describe('generateCharts — empty windows', () => {
  let emptyTmpDir: string;
  let emptyRecords: Awaited<ReturnType<typeof generateCharts>>;

  beforeAll(async () => {
    emptyTmpDir = path.join(tmpdir(), `scalability-charts-empty-${Date.now()}`);
    emptyRecords = await generateCharts([], emptyTmpDir);
  });

  afterAll(async () => {
    await rm(emptyTmpDir, { recursive: true, force: true });
  });

  it('generates a record for every panel spec', () => {
    expect(emptyRecords).toHaveLength(PANEL_SPECS.length);
  });

  it('all records have hasData: false', () => {
    for (const record of emptyRecords) {
      expect(record.hasData, `${record.slug} should have no data`).toBe(false);
    }
  });

  it('still writes SVG files for all panels', async () => {
    const { readFile: rf } = await import('node:fs/promises');
    for (const record of emptyRecords) {
      const filePath = path.join(emptyTmpDir, record.relPath);
      const content = await rf(filePath, 'utf8');
      expect(content.trimStart()).toMatch(/^<svg/);
    }
  });
});
