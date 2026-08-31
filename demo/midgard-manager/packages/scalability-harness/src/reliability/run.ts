// Orchestrates a reliability report: collect -> analyze -> incidents -> charts
// -> render (internal md/html + public md) -> frozen evidence bundle.
// `regenReliabilityReport` re-runs everything after collect from a saved bundle.

import { execSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

import { analyze } from './analyze.js';
import { generateReliabilityCharts } from './charts.js';
import { collect, type CollectedData } from './collect.js';
import type { ReliabilityConfig } from './config.js';
import { writeBundle } from './evidence.js';
import { detectIncidents } from './incidents.js';
import { renderHtml, renderMarkdown } from './render.js';
import { loadSloConfig } from './slo.js';

function gitSha(): string {
  try {
    return execSync('git rev-parse --short HEAD', { encoding: 'utf8' }).trim();
  } catch {
    return 'unknown';
  }
}

function sloSourceLabel(absPath: string): string {
  const marker = `${path.sep}midgard-node${path.sep}`;
  const idx = absPath.indexOf(marker);
  return idx >= 0 ? `demo${absPath.slice(idx).replaceAll(path.sep, '/')}` : 'slo.json';
}

async function refreshManifest(outDir: string): Promise<void> {
  const { readdir } = await import('node:fs/promises');
  const walk = async (dir: string, prefix = ''): Promise<string[]> => {
    const entries = await readdir(dir, { withFileTypes: true });
    const acc: string[] = [];
    for (const e of entries) {
      const rel = prefix ? `${prefix}/${e.name}` : e.name;
      if (e.isDirectory()) acc.push(...(await walk(path.join(dir, e.name), rel)));
      else if (rel !== 'MANIFEST.sha256') acc.push(rel);
    }
    return acc;
  };
  const files = (await walk(outDir)).sort();
  const rows: string[] = [];
  for (const rel of files) {
    const buf = await readFile(path.join(outDir, rel));
    rows.push(`${createHash('sha256').update(buf).digest('hex')}  ${rel}`);
  }
  await writeFile(path.join(outDir, 'MANIFEST.sha256'), rows.join('\n') + '\n');
}

export async function runReliabilityReport(
  config: ReliabilityConfig,
  harnessVersion: string,
  deps: Parameters<typeof collect>[2] = {}
): Promise<{ outDir: string; disposition: string }> {
  await mkdir(config.outputDir, { recursive: true });
  const slo = loadSloConfig(config.sloPath);

  const data = await collect(config, slo, deps);
  const analysis = analyze(data, config.stepSeconds);
  const incidents = detectIncidents(analysis, data, config.stepSeconds);
  const charts = await generateReliabilityCharts(data, analysis, config.outputDir);

  const renderInput = {
    analysis,
    data,
    incidents,
    charts,
    manifestName: 'MANIFEST.sha256',
    evidenceFiles: [
      'collected.json',
      'analysis.json',
      'incidents.json',
      'slo.json',
      'run-manifest.json',
    ],
    sloSourceRelPath: sloSourceLabel(slo.sourcePath),
    rollingWindow: config.rollingWindow,
  };

  const internalMarkdown = renderMarkdown(renderInput, { public: false });
  const publicMarkdown = renderMarkdown(renderInput, { public: true });
  const internalHtml = renderHtml(
    internalMarkdown,
    `Sundial Node Reliability Report — ${config.environment}`
  );

  await writeBundle({
    outDir: config.outputDir,
    config,
    data,
    analysis,
    incidents,
    internalMarkdown,
    internalHtml,
    publicMarkdown,
    harnessVersion,
    gitSha: gitSha(),
  });

  return { outDir: config.outputDir, disposition: analysis.disposition };
}

export async function regenReliabilityReport(bundleDir: string): Promise<{ disposition: string }> {
  const dir = path.resolve(bundleDir);
  const data = JSON.parse(
    await readFile(path.join(dir, 'collected.json'), 'utf8')
  ) as CollectedData;
  const manifest = JSON.parse(await readFile(path.join(dir, 'run-manifest.json'), 'utf8')) as {
    stepSeconds: number;
    rollingWindow: string;
    environment: string;
    sloSourcePath: string;
  };

  const analysis = analyze(data, manifest.stepSeconds);
  const incidents = detectIncidents(analysis, data, manifest.stepSeconds);
  const charts = await generateReliabilityCharts(data, analysis, dir);

  const renderInput = {
    analysis,
    data,
    incidents,
    charts,
    manifestName: 'MANIFEST.sha256',
    evidenceFiles: [
      'collected.json',
      'analysis.json',
      'incidents.json',
      'slo.json',
      'run-manifest.json',
    ],
    sloSourceRelPath: sloSourceLabel(manifest.sloSourcePath ?? data.sloSourcePath),
    rollingWindow: manifest.rollingWindow,
  };

  const internalMarkdown = renderMarkdown(renderInput, { public: false });
  const publicMarkdown = renderMarkdown(renderInput, { public: true });
  const internalHtml = renderHtml(
    internalMarkdown,
    `Sundial Node Reliability Report — ${manifest.environment}`
  );

  await writeFile(path.join(dir, 'analysis.json'), JSON.stringify(analysis, null, 2));
  await writeFile(path.join(dir, 'incidents.json'), JSON.stringify(incidents, null, 2));
  await writeFile(path.join(dir, 'reliability-report.md'), internalMarkdown);
  await writeFile(path.join(dir, 'reliability-report.html'), internalHtml);
  await writeFile(path.join(dir, 'reliability-report.public.md'), publicMarkdown);
  await refreshManifest(dir);

  return { disposition: analysis.disposition };
}
