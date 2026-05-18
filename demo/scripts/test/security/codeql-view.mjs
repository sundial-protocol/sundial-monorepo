#!/usr/bin/env node
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve, join, basename } from 'node:path';
import process from 'node:process';

const CODEQL_DIR = resolve(process.cwd(), '.tmp/codeql');

function findSarifFiles(dir, nameFilter) {
  if (!existsSync(dir)) return [];

  if (nameFilter) {
    // Accept a bare project name (e.g. "midgard-manager") or a full SARIF path.
    const candidate = nameFilter.endsWith('.sarif')
      ? resolve(process.cwd(), nameFilter)
      : join(dir, `results-${nameFilter}.sarif`);
    return existsSync(candidate) ? [candidate] : [];
  }

  return readdirSync(dir)
    .filter((f) => f.startsWith('results-') && f.endsWith('.sarif'))
    .sort()
    .map((f) => join(dir, f));
}

function labelForFile(filePath) {
  const name = basename(filePath, '.sarif');
  return name.startsWith('results-') ? name.slice('results-'.length) : name;
}

function parseSarif(filePath) {
  const sarif = JSON.parse(readFileSync(filePath, 'utf8'));
  const run = sarif.runs?.[0];
  if (!run) return [];

  const rules = [
    ...(run.tool?.driver?.rules ?? []),
    ...((run.tool?.extensions ?? []).flatMap((ext) => ext.rules ?? [])),
  ];
  const ruleMap = new Map(rules.map((r) => [r.id, r]));
  const results = run.results ?? [];

  return results.map((result) => {
    const rule = ruleMap.get(result.ruleId) ?? {};
    const level = result.level ?? rule.defaultConfiguration?.level ?? 'warning';
    const securitySeverity = Number.parseFloat(rule.properties?.['security-severity'] ?? 'NaN');
    const loc = result.locations?.[0]?.physicalLocation;
    const uri = loc?.artifactLocation?.uri ?? 'unknown';
    const region = loc?.region ?? {};
    const message = result.message?.text?.replace(/\[([^\]]+)\]\(\d+\)/g, '$1') ?? '';

    return {
      ruleId: result.ruleId,
      level,
      securitySeverity,
      uri,
      startLine: region.startLine,
      startColumn: region.startColumn,
      message,
    };
  });
}

function formatSeverity(sec) {
  return Number.isFinite(sec) ? sec.toFixed(1) : 'n/a';
}

function printFindings(label, filePath) {
  const findings = parseSarif(filePath);
  console.log(`\n=== ${label} (${findings.length} finding${findings.length !== 1 ? 's' : ''}) ===`);

  if (findings.length === 0) {
    console.log('  No findings.');
    return 0;
  }

  for (const f of findings) {
    const loc = f.startLine != null ? `${f.uri}:${f.startLine}` : f.uri;
    const sev = formatSeverity(f.securitySeverity);
    console.log(`  [${f.level}] ${f.ruleId} (security_severity=${sev})`);
    console.log(`    location: ${loc}`);
    if (f.message) {
      const lines = f.message.split('\n');
      console.log(`    message:  ${lines[0]}`);
      for (const line of lines.slice(1)) {
        console.log(`              ${line}`);
      }
    }
  }

  return findings.length;
}

function main() {
  const arg = process.argv[2];
  const sarifFiles = findSarifFiles(CODEQL_DIR, arg);

  if (sarifFiles.length === 0) {
    const hint = arg
      ? `No SARIF file found for '${arg}' in ${CODEQL_DIR}.`
      : `No results-*.sarif files found in ${CODEQL_DIR}.`;
    console.error(`${hint} Run a security:codeql:check:<project> script first.`);
    process.exit(1);
  }

  let total = 0;
  for (const filePath of sarifFiles) {
    total += printFindings(labelForFile(filePath), filePath);
  }

  console.log(`\nTotal: ${total} finding${total !== 1 ? 's' : ''} across ${sarifFiles.length} project${sarifFiles.length !== 1 ? 's' : ''}.`);
}

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(1);
}
