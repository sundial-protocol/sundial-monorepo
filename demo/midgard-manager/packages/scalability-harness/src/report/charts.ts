import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';

import * as vega from 'vega';
import { compile } from 'vega-lite';

import type { PrometheusSeries, TierMetricWindow } from '../metrics/window.js';

// ---------------------------------------------------------------------------
// Grafana-style dark theme config applied to every chart
// ---------------------------------------------------------------------------

const AXIS_COLOR = '#9fa7b3';
const GRID_COLOR = '#2c3235';

const DARK_CONFIG = {
  background: '#111217',
  view: { fill: '#181b1f', stroke: 'transparent' },
  axis: {
    domainColor: AXIS_COLOR,
    gridColor: GRID_COLOR,
    gridOpacity: 1,
    tickColor: AXIS_COLOR,
    labelColor: AXIS_COLOR,
    titleColor: AXIS_COLOR,
    labelFontSize: 10,
    titleFontSize: 10,
    labelFont: 'sans-serif',
    titleFont: 'sans-serif',
  },
  legend: {
    labelColor: '#d0d0d0',
    titleColor: '#d0d0d0',
    labelFontSize: 10,
    titleFontSize: 10,
    labelFont: 'sans-serif',
    titleFont: 'sans-serif',
    strokeColor: GRID_COLOR,
    fillColor: '#181b1f',
    padding: 6,
  },
  title: {
    color: '#d0d0d0',
    fontSize: 13,
    font: 'sans-serif',
    fontWeight: 'normal' as const,
    anchor: 'start' as const,
    offset: 8,
  },
  range: {
    category: [
      '#7EB26D', // green
      '#EAB839', // yellow
      '#6ED0E0', // cyan
      '#EF843C', // orange
      '#E24D42', // red
      '#1F78C1', // blue
      '#BA43A9', // purple
      '#705DA0', // violet
    ],
  },
};

// ---------------------------------------------------------------------------
// Panel types
// ---------------------------------------------------------------------------

export type FormatY = 'default' | 'bytes' | 'bytes/s' | 'percent';

export type ChartSection =
  | 'Throughput'
  | 'Queue and Mempool'
  | 'Block Pipeline'
  | 'Failure Signals'
  | 'Infrastructure';

export interface PanelSpec {
  slug: string;
  title: string;
  section: ChartSection;
  /** Key into TierMetricWindow.ranges */
  metric: string;
  /** Compute per-second rate from raw counter values */
  rate: boolean;
  unit: string;
  formatY: FormatY;
}

export interface ChartRecord {
  slug: string;
  title: string;
  section: ChartSection;
  /** Path relative to runDir, e.g. 'charts/received-tps.svg' */
  relPath: string;
  hasData: boolean;
}

// ---------------------------------------------------------------------------
// Panel catalog — mirrors Grafana dashboard panels, excluding:
//   - "Block Commitment Worker Duration (s)" (needs histogram rate division)
//   - "Total L1 User Events Count in Block Commitment" (excluded per spec)
// ---------------------------------------------------------------------------

export const PANEL_SPECS: readonly PanelSpec[] = [
  // --- Throughput ---
  {
    slug: 'received-tps',
    title: 'Received Transactions Per Second',
    section: 'Throughput',
    metric: 'tx_submissions_enqueued_total',
    rate: true,
    unit: 'tx/s',
    formatY: 'default',
  },
  {
    slug: 'mempool-drain-rate',
    title: 'Mempool Drain Rate (committed tx/s)',
    section: 'Throughput',
    metric: 'commit_block_tx_count_total',
    rate: true,
    unit: 'tx/s',
    formatY: 'default',
  },
  {
    slug: 'received-cumulative',
    title: 'Received Transactions',
    section: 'Throughput',
    metric: 'tx_submissions_enqueued_total',
    rate: false,
    unit: 'tx',
    formatY: 'default',
  },
  {
    slug: 'committed-cumulative',
    title: 'Committed Transactions',
    section: 'Throughput',
    metric: 'commit_block_tx_count_total',
    rate: false,
    unit: 'tx',
    formatY: 'default',
  },

  // --- Queue and Mempool ---
  {
    slug: 'tx-queue',
    title: 'Midgard Transactions in Queue',
    section: 'Queue and Mempool',
    metric: 'tx_queue_size',
    rate: false,
    unit: 'txs',
    formatY: 'default',
  },
  {
    slug: 'mempool-count',
    title: 'Mempool Transactions',
    section: 'Queue and Mempool',
    metric: 'mempool_tx_count',
    rate: false,
    unit: 'txs',
    formatY: 'default',
  },

  // --- Block Pipeline ---
  {
    slug: 'built-blocks',
    title: 'Built Blocks',
    section: 'Block Pipeline',
    metric: 'commit_block_count_total',
    rate: false,
    unit: 'blocks',
    formatY: 'default',
  },
  {
    slug: 'built-blocks-rate',
    title: 'Built Blocks Per Second',
    section: 'Block Pipeline',
    metric: 'commit_block_count_total',
    rate: true,
    unit: 'blocks/s',
    formatY: 'default',
  },
  {
    slug: 'submitted-blocks',
    title: 'Submitted Blocks',
    section: 'Block Pipeline',
    metric: 'submit_block_count_total',
    rate: false,
    unit: 'blocks',
    formatY: 'default',
  },
  {
    slug: 'submitted-blocks-rate',
    title: 'Submitted Blocks Per Second',
    section: 'Block Pipeline',
    metric: 'submit_block_count_total',
    rate: true,
    unit: 'blocks/s',
    formatY: 'default',
  },
  {
    slug: 'merged-blocks',
    title: 'Merged Blocks',
    section: 'Block Pipeline',
    metric: 'merge_block_count_total',
    rate: false,
    unit: 'blocks',
    formatY: 'default',
  },
  {
    slug: 'merged-blocks-rate',
    title: 'Merged Blocks Per Second',
    section: 'Block Pipeline',
    metric: 'merge_block_count_total',
    rate: true,
    unit: 'blocks/s',
    formatY: 'default',
  },
  {
    slug: 'txs-per-block',
    title: 'Tx Count per Committed Block',
    section: 'Block Pipeline',
    metric: 'commit_block_txs_per_block',
    rate: false,
    unit: 'txs',
    formatY: 'default',
  },
  {
    slug: 'block-size',
    title: 'Block Size',
    section: 'Block Pipeline',
    metric: 'commit_block_events_size_bytes',
    rate: false,
    unit: 'bytes',
    formatY: 'bytes',
  },

  // --- Failure Signals ---
  {
    slug: 'commit-failures',
    title: 'Block Commitment Failures',
    section: 'Failure Signals',
    metric: 'commit_block_commitment_failures_total',
    rate: false,
    unit: 'failures',
    formatY: 'default',
  },
  {
    slug: 'merge-failures',
    title: 'Merge Failures',
    section: 'Failure Signals',
    metric: 'merge_block_failures_total',
    rate: false,
    unit: 'failures',
    formatY: 'default',
  },
  {
    slug: 'rejected-submissions',
    title: 'Rejected Submissions',
    section: 'Failure Signals',
    metric: 'tx_submissions_rejected_total',
    rate: false,
    unit: 'rejected',
    formatY: 'default',
  },

  // --- Infrastructure ---
  {
    slug: 'cpu-usage',
    title: 'CPU Usage',
    section: 'Infrastructure',
    metric: 'rate(container_cpu_user_seconds_total{image!=""}[1m])',
    rate: false,
    unit: 'CPU %',
    formatY: 'percent',
  },
  {
    slug: 'memory-usage',
    title: 'Memory Usage',
    section: 'Infrastructure',
    metric: 'container_memory_usage_bytes{image!=""}',
    rate: false,
    unit: 'bytes',
    formatY: 'bytes',
  },
  {
    slug: 'network-rx',
    title: 'Network Rx',
    section: 'Infrastructure',
    metric: 'rate(container_network_receive_bytes_total{image!=""}[1m])',
    rate: false,
    unit: 'bytes/s',
    formatY: 'bytes/s',
  },
  {
    slug: 'network-tx',
    title: 'Network Tx',
    section: 'Infrastructure',
    metric: 'rate(container_network_transmit_bytes_total{image!=""}[1m])',
    rate: false,
    unit: 'bytes/s',
    formatY: 'bytes/s',
  },
] as const;

// ---------------------------------------------------------------------------
// Data row — vega-lite flat record format
// ---------------------------------------------------------------------------

interface DataRow {
  ts: string; // ISO date string
  value: number;
  series: string; // label for multi-series legend
}

// ---------------------------------------------------------------------------
// Series processing
// ---------------------------------------------------------------------------

function seriesLabel(metric: Record<string, string>): string {
  const name = metric['name'] ?? metric['container_name'];
  if (name !== undefined && name !== '' && name !== 'POD') return name;
  const image = metric['image'];
  if (image !== undefined) {
    const base = image.split('/').pop() ?? image;
    return (base.split(':')[0] ?? base).slice(0, 32);
  }
  return '';
}

// Merge PrometheusSeries from multiple tiers into one sorted value list per
// unique label set, so tier-boundary gaps render as line breaks rather than
// data duplication.
function mergeSeriesByMetric(
  series: PrometheusSeries[]
): Array<{ metric: Record<string, string>; values: Array<[number, number]> }> {
  const groups = new Map<
    string,
    { metric: Record<string, string>; values: Array<[number, number]> }
  >();

  for (const s of series) {
    const key = JSON.stringify(
      Object.fromEntries(
        Object.entries(s.metric)
          .filter(([k]) => k !== '__name__')
          .sort()
      )
    );
    const parsed: Array<[number, number]> = s.values
      .map(([ts, v]): [number, number] => [ts, parseFloat(v)])
      .filter(([, v]) => isFinite(v));

    const existing = groups.get(key);
    if (existing !== undefined) {
      existing.values.push(...parsed);
    } else {
      groups.set(key, { metric: s.metric, values: parsed });
    }
  }

  return [...groups.values()].map((g) => ({
    metric: g.metric,
    values: g.values.sort((a, b) => a[0] - b[0]),
  }));
}

// Per-second rate from consecutive counter samples.
// Counter resets (negative delta) are treated as zero rate.
function computeRate(values: Array<[number, number]>): Array<[number, number]> {
  const result: Array<[number, number]> = [];
  for (let i = 1; i < values.length; i++) {
    const [t0, v0] = values[i - 1];
    const [t1, v1] = values[i];
    const dt = t1 - t0;
    if (dt <= 0) continue;
    result.push([t1, Math.max(0, (v1 - v0) / dt)]);
  }
  return result;
}

export function buildDataRows(windows: TierMetricWindow[], spec: PanelSpec): DataRow[] {
  const allRawSeries = windows.flatMap((w) => w.ranges[spec.metric] ?? []);
  const merged = mergeSeriesByMetric(allRawSeries);
  const rows: DataRow[] = [];

  for (const { metric, values } of merged) {
    const label = seriesLabel(metric);
    const processed = spec.rate ? computeRate(values) : values;
    for (const [ts, value] of processed) {
      rows.push({ ts: new Date(ts * 1000).toISOString(), value, series: label });
    }
  }
  return rows;
}

// ---------------------------------------------------------------------------
// Vega-lite spec construction
// ---------------------------------------------------------------------------

function yAxisConfig(formatY: FormatY, unit: string): Record<string, unknown> {
  const base: Record<string, unknown> = {
    title: unit,
    labelColor: AXIS_COLOR,
    titleColor: AXIS_COLOR,
    gridColor: GRID_COLOR,
    domainColor: AXIS_COLOR,
    tickColor: AXIS_COLOR,
    labelFontSize: 10,
    titleFontSize: 10,
  };
  switch (formatY) {
    case 'bytes':
    case 'bytes/s':
      // SI prefixes (K/M/G): standard for monitoring dashboards
      return { ...base, format: '.2~s' };
    case 'percent':
      // Input values are fractions (e.g. 0.1 = 10%); format multiplies by 100
      return { ...base, format: '.1%' };
    default:
      return { ...base, format: '~s' };
  }
}

function buildSpec(
  title: string,
  data: DataRow[],
  tierBoundaryMs: number[],
  unit: string,
  formatY: FormatY
): Record<string, unknown> {
  const uniqueSeries = new Set(data.map((d) => d.series));
  const hasMultipleSeries = uniqueSeries.size > 1;

  const colorEncoding = hasMultipleSeries
    ? {
        field: 'series',
        type: 'nominal',
        legend: { direction: 'horizontal', orient: 'bottom' },
        scale: { range: DARK_CONFIG.range.category },
      }
    : { value: '#7EB26D' };

  const xEncoding = {
    field: 'ts',
    type: 'temporal',
    axis: {
      format: '%H:%M',
      title: 'Time (UTC)',
      labelColor: AXIS_COLOR,
      titleColor: AXIS_COLOR,
      gridColor: GRID_COLOR,
      domainColor: AXIS_COLOR,
      tickColor: AXIS_COLOR,
      labelFontSize: 10,
      titleFontSize: 10,
    },
  };

  const layers: unknown[] = [
    {
      data: { values: data },
      mark: { type: 'line', interpolate: 'monotone', strokeWidth: 1.5 },
      encoding: {
        x: xEncoding,
        y: {
          field: 'value',
          type: 'quantitative',
          scale: { zero: true },
          axis: yAxisConfig(formatY, unit),
        },
        color: colorEncoding,
      },
    },
  ];

  if (tierBoundaryMs.length > 0) {
    layers.push({
      data: {
        values: tierBoundaryMs.map((ts) => ({ ts: new Date(ts).toISOString() })),
      },
      mark: { type: 'rule', color: '#5c6e7e', strokeDash: [4, 4], strokeWidth: 1 },
      encoding: { x: { field: 'ts', type: 'temporal' } },
    });
  }

  return {
    $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
    title: { text: title },
    width: 760,
    height: 160,
    config: DARK_CONFIG,
    layer: layers,
    resolve: { scale: { color: 'independent' } },
  };
}

// ---------------------------------------------------------------------------
// SVG rendering via vega runtime
// ---------------------------------------------------------------------------

async function renderToSvg(spec: Record<string, unknown>): Promise<string> {
  // compile() validates the spec and produces a Vega runtime spec.
  // The cast here is intentional: we build the vega-lite spec as a plain
  // object to avoid fighting vega-lite's deeply nested generic types while
  // still relying on compile() for runtime validation.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const vegaSpec = compile(spec as any).spec;
  const view = new vega.View(vega.parse(vegaSpec), { renderer: 'none' });
  await view.runAsync();
  const svg = await view.toSVG();
  await view.finalize();
  return svg;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export async function generateCharts(
  windows: TierMetricWindow[],
  runDir: string
): Promise<ChartRecord[]> {
  const chartsDir = path.join(runDir, 'charts');
  await mkdir(chartsDir, { recursive: true });

  // Tier boundaries: start of each tier after the first, in milliseconds
  const tierBoundaryMs = windows.slice(1).map((w) => new Date(w.startedAt).getTime());

  const records: ChartRecord[] = [];

  for (const panelSpec of PANEL_SPECS) {
    const data = buildDataRows(windows, panelSpec);
    const hasData = data.length > 0;
    const fileName = `${panelSpec.slug}.svg`;
    const relPath = `charts/${fileName}`;

    try {
      const spec = buildSpec(
        panelSpec.title,
        data,
        tierBoundaryMs,
        panelSpec.unit,
        panelSpec.formatY
      );
      const svg = await renderToSvg(spec);
      await writeFile(path.join(chartsDir, fileName), svg, 'utf8');
    } catch (err) {
      console.error(`  Chart generation failed for ${panelSpec.slug}: ${String(err)}`);
      records.push({
        slug: panelSpec.slug,
        title: panelSpec.title,
        section: panelSpec.section,
        relPath,
        hasData: false,
      });
      continue;
    }

    records.push({
      slug: panelSpec.slug,
      title: panelSpec.title,
      section: panelSpec.section,
      relPath,
      hasData,
    });
  }

  return records;
}
