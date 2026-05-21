import { execSync } from 'node:child_process';

let cachedRepoRoot: string | null | undefined;

function detectRepoRoot(): string | null {
  if (cachedRepoRoot !== undefined) {
    return cachedRepoRoot;
  }
  try {
    const root = execSync('git rev-parse --show-toplevel', { encoding: 'utf8' }).trim();
    cachedRepoRoot = root.length > 0 ? root : null;
  } catch {
    cachedRepoRoot = null;
  }
  return cachedRepoRoot;
}

export function sanitizePathLikeText(value: string): string {
  const repoRoot = detectRepoRoot();
  if (repoRoot === null) {
    return value;
  }

  if (value === repoRoot) {
    return '/';
  }

  const withSlash = repoRoot.endsWith('/') ? repoRoot : `${repoRoot}/`;
  return value.split(withSlash).join('/');
}

export function stringifyWithSanitizedPaths(value: unknown): string {
  return JSON.stringify(
    value,
    (_key, nestedValue) =>
      typeof nestedValue === 'string' ? sanitizePathLikeText(nestedValue) : nestedValue,
    2
  );
}

export function stringifyCompactWithSanitizedPaths(value: unknown): string {
  return JSON.stringify(value, (_key, nestedValue) =>
    typeof nestedValue === 'string' ? sanitizePathLikeText(nestedValue) : nestedValue
  );
}
