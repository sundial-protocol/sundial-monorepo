import type { MatrixSample, PrometheusVectorResult } from './prometheus.js';
import { PrometheusClient, stripLabelSelectors } from './prometheus.js';

// Matches demo/midgard-node/prometheus.yml scrape_interval.
export const RANGE_STEP_SECONDS = 15;

// PrometheusSeries is a range query result row — same shape as MatrixSample.
export type PrometheusSeries = MatrixSample;

export interface TierMetricWindow {
  tierIndex: number;
  targetTps: number;
  startedAt: string;
  stoppedAt: string;
  recoveryStartedAt: string;
  recoveryStoppedAt: string;
  // Instant scalar per query at each time point. null = metric unavailable (not 0).
  before: Record<string, number | null>;
  afterLoad: Record<string, number | null>;
  afterRecovery: Record<string, number | null>;
  // Range series per query over startedAt → recoveryStoppedAt.
  ranges: Record<string, PrometheusSeries[]>;
}

export interface CounterDelta {
  query: string;
  deltaLoad: number | null; // afterLoad - before; null if either endpoint missing
  deltaRecovery: number | null; // afterRecovery - before; null if either endpoint missing
}

export interface GaugeSummary {
  query: string;
  peak: number | null; // max(range); null if no range data
  final: number | null; // last value by timestamp; null if no range data
}

export interface TierWindowSummary {
  counterDeltas: CounterDelta[];
  gaugeSummaries: GaugeSummary[];
}

// ---------------------------------------------------------------------------
// Metric classification
// ---------------------------------------------------------------------------

// rate() applied to a counter yields a gauge-like derived metric — deltas do
// not apply. Check rate() first before inspecting the metric name suffix.
export function isCounter(query: string): boolean {
  if (/^rate\(/.test(query)) return false;
  return stripLabelSelectors(query).trim().endsWith('_total');
}

// ---------------------------------------------------------------------------
// Scalar extraction
// ---------------------------------------------------------------------------

// Sums all finite values across all series in a vector result.
// Multi-series queries (e.g. cAdvisor per-container) are aggregated into one number.
// Returns null when the result is absent or contains no finite values.
export function extractScalar(result: PrometheusVectorResult | null): number | null {
  if (!result || result.length === 0) return null;
  let sum = 0;
  let count = 0;
  for (const series of result) {
    const v = parseFloat(series.value[1]);
    if (isFinite(v)) {
      sum += v;
      count++;
    }
  }
  return count === 0 ? null : sum;
}

// ---------------------------------------------------------------------------
// Gauge peak / final helpers
// ---------------------------------------------------------------------------

export function computeGaugePeak(series: PrometheusSeries[]): number | null {
  let peak: number | null = null;
  for (const s of series) {
    for (const [, v] of s.values) {
      const n = parseFloat(v);
      if (isFinite(n) && (peak === null || n > peak)) {
        peak = n;
      }
    }
  }
  return peak;
}

// Returns the value with the highest timestamp across all series.
export function computeGaugeFinal(series: PrometheusSeries[]): number | null {
  let latestTs = -Infinity;
  let final: number | null = null;
  for (const s of series) {
    if (s.values.length === 0) continue;
    const [ts, v] = s.values[s.values.length - 1];
    if (ts > latestTs) {
      const n = parseFloat(v);
      if (isFinite(n)) {
        latestTs = ts;
        final = n;
      }
    }
  }
  return final;
}

// ---------------------------------------------------------------------------
// Counter delta helper
// ---------------------------------------------------------------------------

export function computeCounterDelta(after: number | null, before: number | null): number | null {
  if (after === null || before === null) return null;
  return after - before;
}

// ---------------------------------------------------------------------------
// Summary over a collected window
// ---------------------------------------------------------------------------

export function summarizeTierWindow(
  window: TierMetricWindow,
  queries: ReadonlyArray<string>
): TierWindowSummary {
  const counterDeltas: CounterDelta[] = [];
  const gaugeSummaries: GaugeSummary[] = [];

  for (const q of queries) {
    if (isCounter(q)) {
      counterDeltas.push({
        query: q,
        deltaLoad: computeCounterDelta(window.afterLoad[q] ?? null, window.before[q] ?? null),
        deltaRecovery: computeCounterDelta(
          window.afterRecovery[q] ?? null,
          window.before[q] ?? null
        ),
      });
    } else {
      const series = window.ranges[q] ?? [];
      gaugeSummaries.push({
        query: q,
        peak: computeGaugePeak(series),
        final: computeGaugeFinal(series),
      });
    }
  }

  return { counterDeltas, gaugeSummaries };
}

// ---------------------------------------------------------------------------
// Collection helpers (internal)
// ---------------------------------------------------------------------------

async function safeInstantScalar(
  client: PrometheusClient,
  query: string,
  time: Date
): Promise<number | null> {
  try {
    return extractScalar(await client.queryInstant(query, time));
  } catch {
    return null;
  }
}

async function safeRangeQuery(
  client: PrometheusClient,
  query: string,
  start: Date,
  end: Date,
  stepSeconds: number
): Promise<PrometheusSeries[]> {
  try {
    return await client.queryRange(query, start, end, stepSeconds);
  } catch {
    return [];
  }
}

// ---------------------------------------------------------------------------
// collectTierWindow
// ---------------------------------------------------------------------------

export async function collectTierWindow(
  client: PrometheusClient,
  tierIndex: number,
  targetTps: number,
  startedAt: Date,
  stoppedAt: Date,
  recoveryStartedAt: Date,
  recoveryStoppedAt: Date,
  queries: ReadonlyArray<string>,
  stepSeconds: number = RANGE_STEP_SECONDS
): Promise<TierMetricWindow> {
  // Fire instant snapshots at the three time points and range queries in parallel.
  const [beforeValues, afterLoadValues, afterRecoveryValues, rangeValues] = await Promise.all([
    Promise.all(queries.map((q) => safeInstantScalar(client, q, startedAt))),
    Promise.all(queries.map((q) => safeInstantScalar(client, q, stoppedAt))),
    Promise.all(queries.map((q) => safeInstantScalar(client, q, recoveryStoppedAt))),
    Promise.all(
      queries.map((q) => safeRangeQuery(client, q, startedAt, recoveryStoppedAt, stepSeconds))
    ),
  ]);

  const before: Record<string, number | null> = {};
  const afterLoad: Record<string, number | null> = {};
  const afterRecovery: Record<string, number | null> = {};
  const ranges: Record<string, PrometheusSeries[]> = {};

  for (let i = 0; i < queries.length; i++) {
    const q = queries[i];
    before[q] = beforeValues[i];
    afterLoad[q] = afterLoadValues[i];
    afterRecovery[q] = afterRecoveryValues[i];
    ranges[q] = rangeValues[i];
  }

  return {
    tierIndex,
    targetTps,
    startedAt: startedAt.toISOString(),
    stoppedAt: stoppedAt.toISOString(),
    recoveryStartedAt: recoveryStartedAt.toISOString(),
    recoveryStoppedAt: recoveryStoppedAt.toISOString(),
    before,
    afterLoad,
    afterRecovery,
    ranges,
  };
}
