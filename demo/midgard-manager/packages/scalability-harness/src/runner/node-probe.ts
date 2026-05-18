import type { ArtifactWriter } from '../evidence/artifacts.js';
import type { NodeProbeEvent, StopConditionEvent } from '../evidence/load-events.js';
import { makeEvent } from '../evidence/load-events.js';

export const PROBE_INTERVAL_MS = 5_000;
export const PROBE_TIMEOUT_MS = 5_000;
// Matches MidgardNodeClient.isAvailable() — 404 means the node answered, not that the tx exists.
const PROBE_TX_HASH = '0'.repeat(64);

export interface ProbeResult {
  ok: boolean;
  statusCode?: number;
  latencyMs: number;
  error?: string;
}

export interface ProbeConfig {
  nodeEndpoint: string;
  runId: string;
  tierIndex?: number;
  maxConsecutiveFailures: number;
  intervalMs?: number;
  timeoutMs?: number;
}

export interface ProbeLoopResult {
  stopConditionTriggered: boolean;
  consecutiveFailures: number;
  totalProbes: number;
  failedProbes: number;
}

// Injected for testing; production uses globalThis.fetch.
export type Fetcher = (url: string, init: RequestInit) => Promise<Response>;

export async function probeNode(
  nodeEndpoint: string,
  timeoutMs: number,
  fetcher: Fetcher = (url, init) => fetch(url, init)
): Promise<ProbeResult> {
  const url = `${nodeEndpoint}/tx?tx_hash=${PROBE_TX_HASH}`;
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  const start = Date.now();

  try {
    const response = await fetcher(url, { signal: controller.signal });
    clearTimeout(timer);
    const latencyMs = Date.now() - start;

    // 404 is the expected "node is alive" response — same rule as MidgardNodeClient.isAvailable().
    const ok = response.status === 404;
    return { ok, statusCode: response.status, latencyMs };
  } catch (err) {
    clearTimeout(timer);
    const latencyMs = Date.now() - start;
    const isTimeout = err instanceof DOMException && err.name === 'AbortError';
    return {
      ok: false,
      latencyMs,
      error: isTimeout ? 'probe timed out' : String(err instanceof Error ? err.message : err),
    };
  }
}

// Resolves to true when the delay completes, false when the signal fires first.
function delayOrAbort(ms: number, signal: AbortSignal): Promise<boolean> {
  return new Promise((resolve) => {
    if (signal.aborted) {
      resolve(false);
      return;
    }

    const timer = setTimeout(() => {
      signal.removeEventListener('abort', onAbort);
      resolve(true);
    }, ms);

    const onAbort = () => {
      clearTimeout(timer);
      resolve(false);
    };

    signal.addEventListener('abort', onAbort, { once: true });
  });
}

export async function runProbeLoop(
  config: ProbeConfig,
  writer: ArtifactWriter,
  signal: AbortSignal,
  fetcher: Fetcher = (url, init) => fetch(url, init)
): Promise<ProbeLoopResult> {
  const {
    nodeEndpoint,
    runId,
    tierIndex,
    maxConsecutiveFailures,
    intervalMs = PROBE_INTERVAL_MS,
    timeoutMs = PROBE_TIMEOUT_MS,
  } = config;

  let consecutiveFailures = 0;
  let totalProbes = 0;
  let failedProbes = 0;
  let stopConditionTriggered = false;

  // Fire the first probe immediately, then repeat every intervalMs.
  // Uses sequential probing (next probe starts after previous completes) so slow
  // responses don't cause overlapping requests.
  while (!signal.aborted) {
    const result = await probeNode(nodeEndpoint, timeoutMs, fetcher);
    totalProbes++;

    await writer.appendLoadEvent(
      makeEvent<NodeProbeEvent>({
        event: 'node_probe',
        runId,
        tierIndex,
        ok: result.ok,
        latencyMs: result.latencyMs,
        statusCode: result.statusCode,
        errorMessage: result.error,
      })
    );

    if (result.ok) {
      consecutiveFailures = 0;
    } else {
      failedProbes++;
      consecutiveFailures++;

      if (consecutiveFailures >= maxConsecutiveFailures) {
        await writer.appendLoadEvent(
          makeEvent<StopConditionEvent>({
            event: 'stop_condition',
            runId,
            tierIndex,
            reason: 'max_consecutive_probe_failures',
            metricValues: {
              consecutiveFailures,
              latencyMs: result.latencyMs,
            },
          })
        );
        stopConditionTriggered = true;
        break;
      }
    }

    // Wait for the next interval or an external abort — whichever comes first.
    const completed = await delayOrAbort(intervalMs, signal);
    if (!completed) break;
  }

  return { stopConditionTriggered, consecutiveFailures, totalProbes, failedProbes };
}
