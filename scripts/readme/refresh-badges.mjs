#!/usr/bin/env node
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(SCRIPT_DIR, '../..');
const README_PATH = resolve(REPO_ROOT, 'README.md');
const SHIELDS_BASE_URL = 'https://img.shields.io/badge';
const SHIELDS_STYLE = 'flat-square';
const BOOLEAN_SUCCESS_MESSAGE = 'passing';
const BOOLEAN_SUCCESS_COLOR = 'brightgreen';
const BOOLEAN_FAILURE_MESSAGE = 'failing';
const BOOLEAN_FAILURE_COLOR = 'red';
const UNKNOWN_MESSAGE = 'unknown';
const UNKNOWN_COLOR = 'lightgrey';

const BADGE_SPECS = {
  build: {
    altText: 'Build',
    label: 'build',
    startMarker: '<!-- badge:build:start -->',
    endMarker: '<!-- badge:build:end -->',
    type: 'boolean',
  },
  quality: {
    altText: 'Quality',
    label: 'quality',
    startMarker: '<!-- badge:quality:start -->',
    endMarker: '<!-- badge:quality:end -->',
    type: 'boolean',
  },
  security: {
    altText: 'Security',
    label: 'security',
    startMarker: '<!-- badge:security:start -->',
    endMarker: '<!-- badge:security:end -->',
    type: 'boolean',
  },
  infra: {
    altText: 'Infra',
    label: 'infra',
    startMarker: '<!-- badge:infra:start -->',
    endMarker: '<!-- badge:infra:end -->',
    type: 'boolean',
  },
  unit: {
    altText: 'Unit Tests',
    label: 'unit tests',
    startMarker: '<!-- badge:unit:start -->',
    endMarker: '<!-- badge:unit:end -->',
    type: 'boolean',
  },
  integration: {
    altText: 'Integration Tests',
    label: 'integration tests',
    startMarker: '<!-- badge:integration:start -->',
    endMarker: '<!-- badge:integration:end -->',
    type: 'boolean',
  },
  e2e: {
    altText: 'E2E Tests',
    label: 'e2e tests',
    startMarker: '<!-- badge:e2e:start -->',
    endMarker: '<!-- badge:e2e:end -->',
    type: 'boolean',
  },
  observability: {
    altText: 'Observability Tests',
    label: 'observability tests',
    startMarker: '<!-- badge:observability:start -->',
    endMarker: '<!-- badge:observability:end -->',
    type: 'boolean',
  },
  'api-coverage': {
    altText: 'API Coverage',
    label: 'api coverage',
    startMarker: '<!-- badge:api-coverage:start -->',
    endMarker: '<!-- badge:api-coverage:end -->',
    type: 'coverage',
    coveragePath: resolve(REPO_ROOT, 'coverage/api/coverage-summary.json'),
  },
  'indexer-coverage': {
    altText: 'Indexer Coverage',
    label: 'indexer coverage',
    startMarker: '<!-- badge:indexer-coverage:start -->',
    endMarker: '<!-- badge:indexer-coverage:end -->',
    type: 'coverage',
    coveragePath: resolve(REPO_ROOT, 'coverage/indexer/coverage-summary.json'),
  },
};

function encodeBadgeSegment(value) {
  return String(value).replaceAll('%', '%25').replaceAll(' ', '%20');
}

function buildBadge(altText, label, message, color) {
  const badgeUrl =
    `${SHIELDS_BASE_URL}/${encodeBadgeSegment(label)}` +
    `-${encodeBadgeSegment(message)}` +
    `-${encodeBadgeSegment(color)}?style=${SHIELDS_STYLE}`;
  return `<img alt="${altText}" src="${badgeUrl}" />`;
}

function buildUnknownBadge(spec) {
  return buildBadge(spec.altText, spec.label, UNKNOWN_MESSAGE, UNKNOWN_COLOR);
}

function coverageColor(pct) {
  if (pct < 60) {
    return 'red';
  }
  if (pct < 80) {
    return 'orange';
  }
  if (pct < 90) {
    return 'yellowgreen';
  }
  return 'brightgreen';
}

function readCoveragePct(filePath) {
  if (!existsSync(filePath)) {
    throw new Error(`Missing coverage summary file: ${filePath}`);
  }

  let parsed;
  try {
    parsed = JSON.parse(readFileSync(filePath, 'utf8'));
  } catch (error) {
    throw new Error(
      `Failed to parse coverage summary JSON at ${filePath}: ${String(error)}`,
    );
  }

  const pct = parsed?.total?.lines?.pct;
  if (typeof pct !== 'number' || !Number.isFinite(pct)) {
    throw new Error(
      `Invalid coverage summary format at ${filePath}: expected total.lines.pct number`,
    );
  }

  return Number(pct.toFixed(1));
}

function parseExitCode(exitCodeArg) {
  if (exitCodeArg === undefined) {
    throw new Error('Missing exit code argument');
  }

  const exitCode = Number.parseInt(exitCodeArg, 10);
  if (!Number.isInteger(exitCode) || exitCode < 0) {
    throw new Error(`Invalid exit code: ${exitCodeArg}`);
  }

  return exitCode;
}

function buildBooleanBadge(spec, exitCode) {
  const message =
    exitCode === 0 ? BOOLEAN_SUCCESS_MESSAGE : BOOLEAN_FAILURE_MESSAGE;
  const color = exitCode === 0 ? BOOLEAN_SUCCESS_COLOR : BOOLEAN_FAILURE_COLOR;

  return buildBadge(spec.altText, spec.label, message, color);
}

function buildCoverageBadge(spec, exitCode) {
  if (exitCode !== 0) {
    return buildBadge(
      spec.altText,
      spec.label,
      BOOLEAN_FAILURE_MESSAGE,
      BOOLEAN_FAILURE_COLOR,
    );
  }

  const pct = readCoveragePct(spec.coveragePath);
  return buildBadge(
    spec.altText,
    spec.label,
    `${pct.toFixed(1)}%`,
    coverageColor(pct),
  );
}

function replaceBadgeBlock(readme, spec, badgeLine, newline) {
  const startIndex = readme.indexOf(spec.startMarker);
  const endIndex = readme.indexOf(spec.endMarker);

  if (startIndex === -1 || endIndex === -1) {
    throw new Error(
      `README badge markers are missing for ${spec.label}: required ${spec.startMarker} and ${spec.endMarker}`,
    );
  }

  if (endIndex < startIndex) {
    throw new Error(`README badge markers are out of order for ${spec.label}`);
  }

  const replacementBlock =
    `${spec.startMarker}${newline}` +
    `${badgeLine}${newline}` +
    `${spec.endMarker}`;

  return (
    readme.slice(0, startIndex) +
    replacementBlock +
    readme.slice(endIndex + spec.endMarker.length)
  );
}

export function updateReadmeBadge(badgeName, exitCode) {
  const spec = BADGE_SPECS[badgeName];
  if (!spec) {
    throw new Error(
      `Unknown badge "${badgeName}". Expected one of: ${Object.keys(BADGE_SPECS).join(', ')}`,
    );
  }

  if (!existsSync(README_PATH)) {
    throw new Error(`Missing README file: ${README_PATH}`);
  }

  const readme = readFileSync(README_PATH, 'utf8');
  const newline = readme.includes('\r\n') ? '\r\n' : '\n';
  const badgeLine =
    spec.type === 'coverage'
      ? buildCoverageBadge(spec, exitCode)
      : buildBooleanBadge(spec, exitCode);
  const updatedReadme = replaceBadgeBlock(readme, spec, badgeLine, newline);

  writeFileSync(README_PATH, updatedReadme, 'utf8');
}

export function resetAllReadmeBadges() {
  if (!existsSync(README_PATH)) {
    throw new Error(`Missing README file: ${README_PATH}`);
  }

  const readme = readFileSync(README_PATH, 'utf8');
  const newline = readme.includes('\r\n') ? '\r\n' : '\n';
  let updatedReadme = readme;

  for (const spec of Object.values(BADGE_SPECS)) {
    updatedReadme = replaceBadgeBlock(
      updatedReadme,
      spec,
      buildUnknownBadge(spec),
      newline,
    );
  }

  writeFileSync(README_PATH, updatedReadme, 'utf8');
}

function isMainModule() {
  return import.meta.url === new URL(process.argv[1], 'file:').href;
}

if (isMainModule()) {
  try {
    const [badgeName, exitCodeArg] = process.argv.slice(2);
    if (!badgeName) {
      throw new Error(
        'Usage: node scripts/readme/refresh-badges.mjs <badge-name> <exit-code> | reset-all',
      );
    }

    if (badgeName === 'reset-all') {
      resetAllReadmeBadges();
      process.exit(0);
    }

    updateReadmeBadge(badgeName, parseExitCode(exitCodeArg));
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  }
}
