#!/usr/bin/env node
/**
 * Regenerates report.md and charts/ for an existing benchmark run directory.
 * Usage: node scripts/regen-report.mjs <run-dir>
 *
 * Reads: run-manifest.json, scenario.json, summary.json, prometheus-samples.json,
 *        loki-captures.json, tempo-captures.json (optional)
 * Writes: charts/*.svg, report.md
 */

import { readdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const distRoot = path.join(__dirname, '..', 'dist');

async function readJson(filePath) {
  const text = await readFile(filePath, 'utf8');
  return JSON.parse(text);
}

async function tryReadJson(filePath) {
  try {
    return await readJson(filePath);
  } catch {
    return null;
  }
}

const runDir = path.resolve(process.argv[2] ?? '.');

console.log(`Run dir: ${runDir}`);

const [manifest, scenario, summary, promSamples, lokiRaw, tempoRaw] = await Promise.all([
  readJson(path.join(runDir, 'run-manifest.json')),
  readJson(path.join(runDir, 'scenario.json')),
  readJson(path.join(runDir, 'summary.json')),
  readJson(path.join(runDir, 'prometheus-samples.json')),
  tryReadJson(path.join(runDir, 'loki-captures.json')),
  tryReadJson(path.join(runDir, 'tempo-captures.json')),
]);

const { generateCharts } = await import(`${distRoot}/report/charts.js`);
const { renderReport } = await import(`${distRoot}/report/markdown.js`);

const prometheusWindows = promSamples.tiers ?? [];
let chartRecords;
if (prometheusWindows.length > 0) {
  console.log(`Generating charts from ${prometheusWindows.length} tier window(s)...`);
  chartRecords = await generateCharts(prometheusWindows, runDir);
  console.log(`  ${chartRecords.length} charts written`);
}

const entries = await readdir(runDir, { withFileTypes: true });
let artifactFiles = entries
  .filter((e) => e.isFile())
  .map((e) => e.name)
  .sort();
if (!artifactFiles.includes('report.md')) {
  artifactFiles = [...artifactFiles, 'report.md'].sort();
}

const lokiCaptures = Array.isArray(lokiRaw) && lokiRaw.length > 0 ? lokiRaw : undefined;
const tempoCaptures = Array.isArray(tempoRaw) && tempoRaw.length > 0 ? tempoRaw : undefined;

const markdown = renderReport({
  manifest,
  scenario,
  tierSummaries: summary.tierSummaries,
  conclusion: summary.conclusion,
  artifactFiles,
  lokiCaptures,
  tempoCaptures,
  chartRecords,
});

const reportPath = path.join(runDir, 'report.md');
await writeFile(reportPath, markdown, 'utf8');
console.log(`Report written: ${reportPath}`);
