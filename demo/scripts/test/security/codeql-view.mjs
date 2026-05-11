#!/usr/bin/env node
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import process from 'node:process';
import { spawnSync } from 'node:child_process';

const DEFAULT_DATABASE_PATHS = ['.tmp/codeql/database', '.tmp/codeql-probe-db'];
const QUERY_PACK = 'codeql/javascript-queries';

function resolveDatabasePath(pathArg) {
  if (pathArg) {
    return resolve(process.cwd(), pathArg);
  }

  for (const candidate of DEFAULT_DATABASE_PATHS) {
    const resolved = resolve(process.cwd(), candidate);
    if (existsSync(resolved)) {
      return resolved;
    }
  }

  throw new Error(
    `No CodeQL database found. Tried: ${DEFAULT_DATABASE_PATHS.join(', ')}. Run npm run codeql:check first, or pass a database path explicitly.`,
  );
}

function main() {
  const databasePath = resolveDatabasePath(process.argv[2]);

  const result = spawnSync(
    'codeql',
    [
      'database',
      'interpret-results',
      '-q',
      '--no-print-diagnostics-summary',
      '--no-print-metrics-summary',
      '--format=csv',
      '--output=/dev/stdout',
      '--',
      databasePath,
      QUERY_PACK,
    ],
    {
      stdio: 'inherit',
    },
  );

  if (result.error) {
    throw result.error;
  }

  process.exit(result.status ?? 1);
}

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(1);
}
