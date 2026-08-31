// Minimal dark-theme SVG charts for the reliability report, rendered with the
// same vega / vega-lite toolchain the benchmark report uses.

import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';

import * as vega from 'vega';
import { compile } from 'vega-lite';

import type { ReliabilityAnalysis } from './analyze.js';
import type { CollectedData } from './collect.js';
import { matrixToPoints } from './series.js';

export type ChartRecord = { slug: string; title: string; relPath: string; hasData: boolean };

const AXIS = '#8a8a8a';
const GRID = '#2a2a2a';

type Row = { ts: string; value: number; series: string };

function buildSpec(title: string, rows: Row[], objective: number | null, yFormat: string): object {
  const multi = new Set(rows.map((r) => r.series)).size > 1;
  const layers: unknown[] = [
    {
      data: { values: rows },
      mark: { type: 'line', interpolate: 'monotone', strokeWidth: 1.5 },
      encoding: {
        x: {
          field: 'ts',
          type: 'temporal',
          axis: {
            format: '%m-%d %H:%M',
            title: 'Time (UTC)',
            labelColor: AXIS,
            titleColor: AXIS,
            gridColor: GRID,
            domainColor: AXIS,
            tickColor: AXIS,
            labelFontSize: 9,
            titleFontSize: 10,
          },
        },
        y: {
          field: 'value',
          type: 'quantitative',
          scale: { zero: false },
          axis: {
            format: yFormat,
            title: null,
            labelColor: AXIS,
            gridColor: GRID,
            domainColor: AXIS,
            tickColor: AXIS,
            labelFontSize: 9,
          },
        },
        color: multi
          ? {
              field: 'series',
              type: 'nominal',
              legend: { orient: 'bottom', direction: 'horizontal', labelColor: AXIS },
            }
          : { value: '#7EB26D' },
      },
    },
  ];
  if (objective !== null) {
    layers.push({
      data: { values: [{ y: objective }] },
      mark: { type: 'rule', color: '#E24D42', strokeDash: [4, 4], strokeWidth: 1 },
      encoding: { y: { field: 'y', type: 'quantitative' } },
    });
  }
  return {
    $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
    width: 720,
    height: 200,
    background: '#141414',
    title: {
      text: title,
      color: '#d0d0d0',
      fontSize: 13,
      fontWeight: 'normal',
      anchor: 'start',
      offset: 8,
    },
    layer: layers,
    config: { view: { stroke: 'transparent' } },
  };
}

async function toSvg(spec: object): Promise<string> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const vegaSpec = compile(spec as any).spec;
  const view = new vega.View(vega.parse(vegaSpec), { renderer: 'none' });
  await view.runAsync();
  const svg = await view.toSVG();
  await view.finalize();
  return svg;
}

export async function generateReliabilityCharts(
  data: CollectedData,
  analysis: ReliabilityAnalysis,
  outDir: string
): Promise<ChartRecord[]> {
  const chartsDir = path.join(outDir, 'charts');
  await mkdir(chartsDir, { recursive: true });
  const records: ChartRecord[] = [];

  const emit = async (
    slug: string,
    title: string,
    rows: Row[],
    objective: number | null,
    yFormat: string
  ) => {
    const relPath = `charts/${slug}.svg`;
    try {
      const svg = await toSvg(buildSpec(title, rows, objective, yFormat));
      await writeFile(path.join(chartsDir, `${slug}.svg`), svg, 'utf8');
      records.push({ slug, title, relPath, hasData: rows.length > 0 });
    } catch (err) {
      records.push({ slug, title, relPath, hasData: false });
      console.error(`  reliability chart ${slug} failed: ${String(err)}`);
    }
  };

  for (const s of data.slo) {
    if (s.kind === 'latency') {
      const rows: Row[] = [];
      for (const [q, snap] of Object.entries(s.rolling)) {
        for (const p of matrixToPoints(snap.result)) {
          rows.push({
            ts: new Date(p.t).toISOString(),
            value: p.v,
            series: `p${Math.round(Number(q) * 100)}`,
          });
        }
      }
      await emit(`slo-${s.id}`, `${s.id} — latency (s), rolling`, rows, s.objective, 's');
    } else {
      const rows = matrixToPoints(s.rolling.ratio?.result).map((p) => ({
        ts: new Date(p.t).toISOString(),
        value: p.v,
        series: s.id,
      }));
      await emit(`slo-${s.id}`, `${s.id} — success ratio, rolling`, rows, s.objective, '%');
    }
  }

  const up = data.supportingRange.find((r) => r.key === 'up');
  await emit(
    'availability',
    'Node exporter up (1 = scraped)',
    matrixToPoints(up?.result).map((p) => ({
      ts: new Date(p.t).toISOString(),
      value: p.v,
      series: 'up',
    })),
    null,
    'd'
  );

  const cadence = data.supportingRange.find((r) => r.key === 'commit_block_rate_5m');
  await emit(
    'block-cadence',
    'Committed blocks/s (rate over 5m)',
    matrixToPoints(cadence?.result).map((p) => ({
      ts: new Date(p.t).toISOString(),
      value: p.v,
      series: 'blocks/s',
    })),
    null,
    '.3~f'
  );

  void analysis;
  return records;
}
