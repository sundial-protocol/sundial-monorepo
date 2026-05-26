import type { PrometheusVectorResult } from '../metrics/prometheus.js';

const STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT = 'stateQueue/root-unit-diagnostics';
const COMMIT_ENDPOINT = 'commit';

const DEFAULT_TIMEOUT_MS = 10 * 60_000;
const DEFAULT_POLL_INTERVAL_MS = 2_000;
const DEFAULT_PROGRESS_INTERVAL_MS = 5_000;
const DEFAULT_COMMIT_RETRY_INTERVAL_MS = 15_000;
const READY_STABLE_POLLS_REQUIRED = 2;
const REQUEST_TIMEOUT_MS = 5_000;

interface PrometheusClientLike {
  queryInstant(query: string, time?: Date): Promise<PrometheusVectorResult>;
}

interface RootUnitDiagnostics {
  status?: string;
  count?: number;
}

export interface SeedReadinessOptions {
  nodeEndpoint: string;
  prometheusClient: PrometheusClientLike;
  timeoutMs?: number;
  pollIntervalMs?: number;
  progressIntervalMs?: number;
  commitRetryIntervalMs?: number;
}

export interface SeedReadinessDependencies {
  fetcher?: (url: string, init?: RequestInit) => Promise<Response>;
  sleep?: (ms: number) => Promise<void>;
  now?: () => number;
  log?: (line: string) => void;
}

export interface SeedReadinessResult {
  outcome: 'seeded' | 'already_ready';
  elapsedMs: number;
  seedAttemptsDelta: number;
  seedSuccessDelta: number;
  seedFailuresDelta: number;
}

type SeedCounters = {
  attempts: number;
  success: number;
  failures: number;
};

const queryCounterSum = async (
  prometheusClient: PrometheusClientLike,
  query: string
): Promise<number> => {
  const series = await prometheusClient.queryInstant(query);
  if (series.length === 0) {
    return 0;
  }

  let total = 0;
  for (const sample of series) {
    const value = Number.parseFloat(sample.value[1]);
    if (!Number.isFinite(value)) {
      continue;
    }
    total += value;
  }

  return total;
};

const readSeedCounters = async (prometheusClient: PrometheusClientLike): Promise<SeedCounters> => {
  const [attempts, success, failures] = await Promise.all([
    queryCounterSum(prometheusClient, 'blocks_db_seed_attempts_total'),
    queryCounterSum(prometheusClient, 'blocks_db_seed_success_total'),
    queryCounterSum(prometheusClient, 'blocks_db_seed_failures_total'),
  ]);

  return { attempts, success, failures };
};

const fetchJsonWithTimeout = async (
  url: string,
  timeoutMs: number,
  fetcher: (url: string, init?: RequestInit) => Promise<Response>
): Promise<Response> => {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    return await fetcher(url, { signal: controller.signal });
  } finally {
    clearTimeout(timer);
  }
};

const triggerCommit = async (
  nodeEndpoint: string,
  fetcher: (url: string, init?: RequestInit) => Promise<Response>,
  log: (line: string) => void
): Promise<void> => {
  const commitUrl = `${nodeEndpoint.replace(/\/$/, '')}/${COMMIT_ENDPOINT}`;
  try {
    const response = await fetchJsonWithTimeout(commitUrl, REQUEST_TIMEOUT_MS, fetcher);
    if (!response.ok) {
      const body = (await response.text()).slice(0, 200);
      log(
        `  seed bootstrap: trigger /commit returned HTTP ${response.status}; continuing (body=${JSON.stringify(body)})`
      );
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    log(`  seed bootstrap: trigger /commit request failed; continuing (${message})`);
  }
};

const fetchRootUnitDiagnostics = async (
  nodeEndpoint: string,
  fetcher: (url: string, init?: RequestInit) => Promise<Response>
): Promise<RootUnitDiagnostics | null> => {
  const diagnosticsUrl = `${nodeEndpoint.replace(/\/$/, '')}/${STATE_QUEUE_ROOT_UNIT_DIAGNOSTICS_ENDPOINT}`;
  try {
    const response = await fetchJsonWithTimeout(diagnosticsUrl, REQUEST_TIMEOUT_MS, fetcher);
    if (!response.ok) {
      return null;
    }
    const data = (await response.json()) as RootUnitDiagnostics;
    return data;
  } catch {
    return null;
  }
};

export async function ensureSeedReadiness(
  options: SeedReadinessOptions,
  dependencies: SeedReadinessDependencies = {}
): Promise<SeedReadinessResult> {
  const {
    nodeEndpoint,
    prometheusClient,
    timeoutMs = DEFAULT_TIMEOUT_MS,
    pollIntervalMs = DEFAULT_POLL_INTERVAL_MS,
    progressIntervalMs = DEFAULT_PROGRESS_INTERVAL_MS,
    commitRetryIntervalMs = DEFAULT_COMMIT_RETRY_INTERVAL_MS,
  } = options;

  const fetcher = dependencies.fetcher ?? ((url: string, init?: RequestInit) => fetch(url, init));
  const sleep =
    dependencies.sleep ??
    ((ms: number) =>
      new Promise<void>((resolve) => {
        setTimeout(resolve, ms);
      }));
  const now = dependencies.now ?? (() => Date.now());
  const log = dependencies.log ?? ((line: string) => console.log(line));

  const startedAtMs = now();
  log(
    `  seed bootstrap: checking seed readiness ` +
      `(timeout=${Math.round(timeoutMs / 1000)}s poll=${Math.round(pollIntervalMs / 1000)}s ` +
      `progress=${Math.round(progressIntervalMs / 1000)}s retrigger=${Math.round(commitRetryIntervalMs / 1000)}s)...`
  );

  const baseline = await readSeedCounters(prometheusClient);
  log(
    `  seed bootstrap: baseline counters attempts=${baseline.attempts} success=${baseline.success} failures=${baseline.failures}`
  );
  await triggerCommit(nodeEndpoint, fetcher, log);

  let lastProgressLogAt = startedAtMs;
  let lastCommitTriggerAt = startedAtMs;
  let stableReadyPolls = 0;

  while (true) {
    await sleep(pollIntervalMs);
    const elapsedMs = now() - startedAtMs;
    if (elapsedMs > timeoutMs) {
      const timedOutCounters = await readSeedCounters(prometheusClient);
      throw new Error(
        `Seed readiness timed out after ${timeoutMs}ms ` +
          `(attempts_delta=${timedOutCounters.attempts - baseline.attempts}, ` +
          `success_delta=${timedOutCounters.success - baseline.success}, ` +
          `failures_delta=${timedOutCounters.failures - baseline.failures})`
      );
    }

    const [currentCounters, rootDiagnostics] = await Promise.all([
      readSeedCounters(prometheusClient),
      fetchRootUnitDiagnostics(nodeEndpoint, fetcher),
    ]);

    const seedAttemptsDelta = currentCounters.attempts - baseline.attempts;
    const seedSuccessDelta = currentCounters.success - baseline.success;
    const seedFailuresDelta = currentCounters.failures - baseline.failures;

    const diagnosticsStatus = rootDiagnostics?.status ?? 'unknown';
    const diagnosticsCount = rootDiagnostics?.count ?? -1;

    if (seedSuccessDelta > 0) {
      log(
        `  seed bootstrap: seeded successfully in ${(elapsedMs / 1000).toFixed(1)}s ` +
          `(attempts=${seedAttemptsDelta}, failures=${seedFailuresDelta}).`
      );
      return {
        outcome: 'seeded',
        elapsedMs,
        seedAttemptsDelta,
        seedSuccessDelta,
        seedFailuresDelta,
      };
    }

    const looksAlreadyReady =
      seedAttemptsDelta === 0 &&
      seedFailuresDelta === 0 &&
      diagnosticsStatus === 'ok' &&
      diagnosticsCount === 1;

    if (looksAlreadyReady) {
      stableReadyPolls += 1;
      if (stableReadyPolls >= READY_STABLE_POLLS_REQUIRED) {
        log(
          `  seed bootstrap: no seeding required (root unit already healthy) after ${(elapsedMs / 1000).toFixed(1)}s.`
        );
        return {
          outcome: 'already_ready',
          elapsedMs,
          seedAttemptsDelta,
          seedSuccessDelta,
          seedFailuresDelta,
        };
      }
    } else {
      stableReadyPolls = 0;
    }

    const shouldLogProgress = now() - lastProgressLogAt >= progressIntervalMs;
    if (shouldLogProgress) {
      log(
        `  seed bootstrap: waiting... elapsed=${(elapsedMs / 1000).toFixed(1)}s ` +
          `attempts=${seedAttemptsDelta} success=${seedSuccessDelta} failures=${seedFailuresDelta} ` +
          `root_status=${diagnosticsStatus} root_count=${diagnosticsCount >= 0 ? diagnosticsCount : 'n/a'}`
      );
      lastProgressLogAt = now();
    }

    if (now() - lastCommitTriggerAt >= commitRetryIntervalMs) {
      await triggerCommit(nodeEndpoint, fetcher, log);
      lastCommitTriggerAt = now();
    }
  }
}
