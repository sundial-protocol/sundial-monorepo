// Writes the frozen evidence bundle: raw collected series, SLO definitions,
// the rendered reports, charts, a run manifest and a SHA-256 checksum file.
// `regen-reliability-report` re-renders purely from this bundle.

import { createHash } from 'node:crypto';
import { readdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';

import type { ReliabilityAnalysis } from './analyze.js';
import type { CollectedData } from './collect.js';
import type { ReliabilityConfig } from './config.js';
import type { Incident } from './incidents.js';

export type ReliabilityRunManifest = {
  kind: 'reliability-report';
  generatedAt: string;
  harnessVersion: string;
  gitSha: string;
  environment: string;
  window: { from: string; to: string; label: string };
  prometheusEndpoint: string;
  lokiConfigured: boolean;
  sloSourcePath: string;
  rollingWindow: string;
  stepSeconds: number;
  disposition: ReliabilityAnalysis['disposition'];
};

async function sha256File(filePath: string): Promise<string> {
  const buf = await readFile(filePath);
  return createHash('sha256').update(buf).digest('hex');
}

export async function writeBundle(input: {
  outDir: string;
  config: ReliabilityConfig;
  data: CollectedData;
  analysis: ReliabilityAnalysis;
  incidents: Incident[];
  internalMarkdown: string;
  internalHtml: string;
  publicMarkdown: string;
  harnessVersion: string;
  gitSha: string;
}): Promise<{ manifestName: string; files: string[] }> {
  const { outDir } = input;

  // Copy the SLO source into the bundle so the report is fully self-contained.
  let sloContent = '{}';
  try {
    sloContent = await readFile(input.data.sloSourcePath, 'utf8');
  } catch {
    /* keep placeholder */
  }

  const manifest: ReliabilityRunManifest = {
    kind: 'reliability-report',
    generatedAt: new Date().toISOString(),
    harnessVersion: input.harnessVersion,
    gitSha: input.gitSha,
    environment: input.config.environment,
    window: input.data.window,
    prometheusEndpoint: input.config.prometheusEndpoint,
    lokiConfigured: input.config.lokiEndpoint !== undefined,
    sloSourcePath: input.data.sloSourcePath,
    rollingWindow: input.config.rollingWindow,
    stepSeconds: input.config.stepSeconds,
    disposition: input.analysis.disposition,
  };

  await writeFile(path.join(outDir, 'collected.json'), JSON.stringify(input.data, null, 2));
  await writeFile(path.join(outDir, 'analysis.json'), JSON.stringify(input.analysis, null, 2));
  await writeFile(path.join(outDir, 'incidents.json'), JSON.stringify(input.incidents, null, 2));
  await writeFile(path.join(outDir, 'slo.json'), sloContent);
  await writeFile(path.join(outDir, 'run-manifest.json'), JSON.stringify(manifest, null, 2));
  await writeFile(path.join(outDir, 'reliability-report.md'), input.internalMarkdown);
  await writeFile(path.join(outDir, 'reliability-report.html'), input.internalHtml);
  await writeFile(path.join(outDir, 'reliability-report.public.md'), input.publicMarkdown);

  // Checksum every file in the bundle (recursively, e.g. charts/).
  const files = await listFilesRecursive(outDir);
  const rows: string[] = [];
  for (const rel of files.sort()) {
    if (rel === 'MANIFEST.sha256') continue;
    rows.push(`${await sha256File(path.join(outDir, rel))}  ${rel}`);
  }
  await writeFile(path.join(outDir, 'MANIFEST.sha256'), rows.join('\n') + '\n');

  return { manifestName: 'MANIFEST.sha256', files: [...files, 'MANIFEST.sha256'].sort() };
}

async function listFilesRecursive(dir: string, prefix = ''): Promise<string[]> {
  const entries = await readdir(dir, { withFileTypes: true });
  const out: string[] = [];
  for (const e of entries) {
    const rel = prefix ? `${prefix}/${e.name}` : e.name;
    if (e.isDirectory()) out.push(...(await listFilesRecursive(path.join(dir, e.name), rel)));
    else out.push(rel);
  }
  return out;
}
