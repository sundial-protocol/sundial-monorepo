// Turns collected series into the reliability report model: per-SLI compliance
// and error-budget accounting, availability, restarts, block-cadence stability
// and a resource-headroom summary.

import type { CollectedData, RangeSnapshot, SloSeries } from './collect.js';
import {
  coefficientOfVariation,
  counterIncrease,
  distinctChangePoints,
  instantScalar,
  matrixToPoints,
  mean,
  type Point,
} from './series.js';

export type Verdict = 'met' | 'missed' | 'no-data';

export type SloResult = {
  id: string;
  kind: SloSeries['kind'];
  description: string;
  objective: number;
  /** Achieved value over the whole window (ratio 0..1, or seconds for latency). */
  achieved: number | null;
  verdict: Verdict;
  /** Ratio SLIs only. */
  errorBudget?: {
    budget: number; // 1 - objective
    consumedFraction: number; // >1 means budget exhausted
    remainingFraction: number;
  };
  /** Latency SLIs: achieved per quantile. */
  quantiles?: Record<string, number | null>;
  /** Intervals where the rolling value breached the objective. */
  breachIntervals: { startMs: number; endMs: number }[];
};

export type AvailabilityResult = {
  fraction: number | null;
  downtimeSeconds: number;
  scrapeGaps: { startMs: number; endMs: number }[];
  restarts: number;
  restartTimestamps: number[];
  builds: { version: string; commit: string; firstSeen: string }[];
};

export type CadenceResult = {
  meanBlocksPerSecond: number | null;
  coefficientOfVariation: number | null;
  samples: number;
};

export type ResourceResult = {
  cpuCoresMean: number | null;
  cpuCoresPeak: number | null;
  memoryMiBMean: number | null;
  memoryMiBPeak: number | null;
};

export type CounterTotals = Record<string, number>;

export type ReliabilityAnalysis = {
  window: CollectedData['window'];
  environment: string;
  slos: SloResult[];
  availability: AvailabilityResult;
  cadence: CadenceResult;
  resources: ResourceResult;
  counterTotals: CounterTotals;
  l1FeesLovelace: number | null;
  disposition: 'Passed' | 'Passed with Observations' | 'Failed';
};

const rangePoints = (snaps: RangeSnapshot[], key: string): Point[] => {
  const snap = snaps.find((s) => s.key === key);
  return snap ? matrixToPoints(snap.result) : [];
};

function analyzeSlo(s: SloSeries, stepSeconds: number): SloResult {
  if (s.kind === 'latency') {
    const quantiles: Record<string, number | null> = {};
    for (const [q, snap] of Object.entries(s.aggregate)) {
      quantiles[q] = instantScalar(snap.result);
    }
    const p95 = quantiles['0.95'] ?? null;
    const rolling95 = matrixToPoints(s.rolling['0.95']?.result);
    const breaches = collapseIntervals(
      rolling95.filter((p) => p.v > s.objective).map((p) => p.t),
      stepSeconds * 1000 * 4
    );
    return {
      id: s.id,
      kind: s.kind,
      description: s.description,
      objective: s.objective,
      achieved: p95,
      verdict: p95 === null ? 'no-data' : p95 <= s.objective ? 'met' : 'missed',
      quantiles,
      breachIntervals: breaches,
    };
  }

  const achieved = instantScalar(s.aggregate.ratio?.result ?? null);
  const budget = 1 - s.objective;
  const rolling = matrixToPoints(s.rolling.ratio?.result);
  const breaches = collapseIntervals(
    rolling.filter((p) => p.v < s.objective).map((p) => p.t),
    stepSeconds * 1000 * 4
  );
  return {
    id: s.id,
    kind: s.kind,
    description: s.description,
    objective: s.objective,
    achieved,
    verdict: achieved === null ? 'no-data' : achieved >= s.objective ? 'met' : 'missed',
    errorBudget:
      achieved === null
        ? undefined
        : {
            budget,
            consumedFraction: budget === 0 ? 0 : (1 - achieved) / budget,
            remainingFraction: budget === 0 ? 1 : 1 - (1 - achieved) / budget,
          },
    breachIntervals: breaches,
  };
}

function collapseIntervals(
  timestamps: number[],
  maxGapMs: number
): { startMs: number; endMs: number }[] {
  if (timestamps.length === 0) return [];
  const sorted = [...timestamps].sort((a, b) => a - b);
  const out: { startMs: number; endMs: number }[] = [{ startMs: sorted[0], endMs: sorted[0] }];
  for (const t of sorted.slice(1)) {
    const last = out[out.length - 1];
    if (t - last.endMs <= maxGapMs) last.endMs = t;
    else out.push({ startMs: t, endMs: t });
  }
  return out;
}

function analyzeAvailability(data: CollectedData, stepSeconds: number): AvailabilityResult {
  const up = rangePoints(data.supportingRange, 'up');
  const stepMs = stepSeconds * 1000;

  const total = up.length;
  const good = up.filter((p) => p.v >= 1).length;
  const fraction = total > 0 ? good / total : null;

  const gaps = collapseIntervals(
    up.filter((p) => p.v < 1).map((p) => p.t),
    stepMs * 3
  );
  const downtimeSeconds = gaps.reduce(
    (acc, g) => acc + Math.max(stepSeconds, (g.endMs - g.startMs) / 1000),
    0
  );

  const startTime = rangePoints(data.supportingRange, 'start_time');
  const restartTimestamps = distinctChangePoints(startTime);

  const buildInfo = data.supportingInstant.find((s) => s.key === 'build_info');
  const builds = (buildInfo?.result ?? []).map((sample) => ({
    version: sample.metric.version ?? 'unknown',
    commit: sample.metric.commit ?? 'unknown',
    firstSeen: new Date(sample.value[0] * 1000).toISOString(),
  }));

  return {
    fraction,
    downtimeSeconds,
    scrapeGaps: gaps,
    restarts: restartTimestamps.length,
    restartTimestamps,
    builds,
  };
}

function analyzeCadence(data: CollectedData): CadenceResult {
  const pts = rangePoints(data.supportingRange, 'commit_block_rate_5m')
    .map((p) => p.v)
    .filter((v) => v > 0);
  return {
    meanBlocksPerSecond: mean(pts),
    coefficientOfVariation: coefficientOfVariation(pts),
    samples: pts.length,
  };
}

function analyzeResources(data: CollectedData): ResourceResult {
  const cpu = rangePoints(data.supportingRange, 'container_cpu').map((p) => p.v);
  const mem = rangePoints(data.supportingRange, 'container_memory_mib').map((p) => p.v);
  return {
    cpuCoresMean: mean(cpu),
    cpuCoresPeak: cpu.length ? Math.max(...cpu) : null,
    memoryMiBMean: mean(mem),
    memoryMiBPeak: mem.length ? Math.max(...mem) : null,
  };
}

const COUNTER_KEYS = [
  'commit_commitment_failures',
  'merge_failures',
  'submit_block_failures',
  'tx_stream_fail',
  'tx_stream_dead_letter',
] as const;

export function analyze(data: CollectedData, stepSeconds: number): ReliabilityAnalysis {
  const slos = data.slo.map((s) => analyzeSlo(s, stepSeconds));
  const availability = analyzeAvailability(data, stepSeconds);
  const cadence = analyzeCadence(data);
  const resources = analyzeResources(data);

  const counterTotals: CounterTotals = {};
  for (const key of COUNTER_KEYS) {
    counterTotals[key] = Math.round(counterIncrease(rangePoints(data.supportingRange, key)));
  }
  const feePts = rangePoints(data.supportingRange, 'l1_commitment_fees_lovelace');
  const l1FeesLovelace = feePts.length ? Math.round(counterIncrease(feePts)) : null;

  const missed = slos.filter((s) => s.verdict === 'missed');
  const noData = slos.filter((s) => s.verdict === 'no-data');
  const disposition: ReliabilityAnalysis['disposition'] =
    missed.length > 0 ? 'Failed' : noData.length > 0 ? 'Passed with Observations' : 'Passed';

  return {
    window: data.window,
    environment: data.environment,
    slos,
    availability,
    cadence,
    resources,
    counterTotals,
    l1FeesLovelace,
    disposition,
  };
}
