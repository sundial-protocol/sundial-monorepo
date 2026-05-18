import type { TierMetricWindow } from '../metrics/window.js';

export const ACCEPTED_COUNTER_QUERY = 'tx_submissions_mempool_accepted_total';
export const COMMITTED_COUNTER_QUERY = 'commit_block_tx_count_total';
export const ACCEPTED_TO_COMMITTED_METHOD = 'cohort_counter_alignment_v1';

type ConfidenceLevel = 'high' | 'medium' | 'low' | 'insufficient_data';

interface WeightedLatency {
  latencyMs: number;
  weight: number;
}

interface CounterPoint {
  tsSeconds: number;
  value: number;
}

export interface AcceptedToCommittedLatencyEstimate {
  method: typeof ACCEPTED_TO_COMMITTED_METHOD;
  confidence: ConfidenceLevel;
  confidenceNotes: string[];
  queryStepSeconds: number | null;
  acceptedCohortCount: number;
  resolvedCohortCount: number;
  acceptedTxCount: number;
  resolvedTxCount: number;
  resolvedRatio: number | null;
  percentilesMs: {
    p50: number | null;
    p95: number | null;
    p99: number | null;
  };
}

function toFiniteNumber(raw: string): number | null {
  const parsed = Number.parseFloat(raw);
  return Number.isFinite(parsed) ? parsed : null;
}

function aggregateCounterSeries(
  series: TierMetricWindow['ranges'][string] | undefined
): CounterPoint[] {
  if (series === undefined || series.length === 0) {
    return [];
  }

  const sumsByTimestamp = new Map<number, number>();
  for (const sample of series) {
    for (const [ts, rawValue] of sample.values) {
      const value = toFiniteNumber(rawValue);
      if (value === null) {
        continue;
      }
      sumsByTimestamp.set(ts, (sumsByTimestamp.get(ts) ?? 0) + value);
    }
  }

  const points = Array.from(sumsByTimestamp.entries())
    .sort((a, b) => a[0] - b[0])
    .map(([tsSeconds, value]) => ({ tsSeconds, value }));

  // Clamp to monotonic non-decreasing to handle restarts/scrape churn.
  let last = -Infinity;
  for (const point of points) {
    if (point.value < last) {
      point.value = last;
      continue;
    }
    last = point.value;
  }

  return points;
}

function inferStepSeconds(...seriesList: CounterPoint[][]): number | null {
  const deltas: number[] = [];
  for (const series of seriesList) {
    for (let index = 1; index < series.length; index += 1) {
      const delta = series[index].tsSeconds - series[index - 1].tsSeconds;
      if (Number.isFinite(delta) && delta > 0) {
        deltas.push(delta);
      }
    }
  }

  if (deltas.length === 0) {
    return null;
  }

  deltas.sort((a, b) => a - b);
  return deltas[Math.floor(deltas.length / 2)];
}

function percentileWeighted(samples: WeightedLatency[], percentile: number): number | null {
  if (samples.length === 0) {
    return null;
  }

  const ordered = [...samples].sort((a, b) => a.latencyMs - b.latencyMs);
  const totalWeight = ordered.reduce((total, sample) => total + sample.weight, 0);
  if (totalWeight <= 0) {
    return null;
  }

  const targetWeight = Math.ceil((percentile / 100) * totalWeight);
  let runningWeight = 0;
  for (const sample of ordered) {
    runningWeight += sample.weight;
    if (runningWeight >= targetWeight) {
      return sample.latencyMs;
    }
  }

  return ordered[ordered.length - 1].latencyMs;
}

export function estimateAcceptedToCommittedLatency(
  window: TierMetricWindow | null
): AcceptedToCommittedLatencyEstimate {
  const base: AcceptedToCommittedLatencyEstimate = {
    method: ACCEPTED_TO_COMMITTED_METHOD,
    confidence: 'insufficient_data',
    confidenceNotes: [],
    queryStepSeconds: null,
    acceptedCohortCount: 0,
    resolvedCohortCount: 0,
    acceptedTxCount: 0,
    resolvedTxCount: 0,
    resolvedRatio: null,
    percentilesMs: {
      p50: null,
      p95: null,
      p99: null,
    },
  };

  if (window === null) {
    return {
      ...base,
      confidenceNotes: [
        'No Prometheus tier window is available, so accepted-to-committed latency cannot be estimated.',
      ],
    };
  }

  const acceptedSeries = aggregateCounterSeries(window.ranges[ACCEPTED_COUNTER_QUERY]);
  const committedSeries = aggregateCounterSeries(window.ranges[COMMITTED_COUNTER_QUERY]);
  const queryStepSeconds = inferStepSeconds(acceptedSeries, committedSeries);

  if (acceptedSeries.length < 2 || committedSeries.length === 0) {
    return {
      ...base,
      queryStepSeconds,
      confidenceNotes: ['Insufficient counter range samples for accepted or committed series.'],
    };
  }

  const weightedLatencies: WeightedLatency[] = [];
  let acceptedCohortCount = 0;
  let resolvedCohortCount = 0;
  let acceptedTxCount = 0;
  let resolvedTxCount = 0;
  let commitIndex = 0;

  for (let index = 1; index < acceptedSeries.length; index += 1) {
    const previous = acceptedSeries[index - 1];
    const current = acceptedSeries[index];
    const acceptedDelta = current.value - previous.value;

    if (!Number.isFinite(acceptedDelta) || acceptedDelta <= 0) {
      continue;
    }

    acceptedCohortCount += 1;
    acceptedTxCount += acceptedDelta;

    while (
      commitIndex < committedSeries.length &&
      committedSeries[commitIndex].value < current.value
    ) {
      commitIndex += 1;
    }

    if (commitIndex >= committedSeries.length) {
      continue;
    }

    resolvedCohortCount += 1;
    resolvedTxCount += acceptedDelta;

    const latencyMs = Math.max(
      0,
      Math.round((committedSeries[commitIndex].tsSeconds - current.tsSeconds) * 1000)
    );
    weightedLatencies.push({ latencyMs, weight: acceptedDelta });
  }

  const resolvedRatio = acceptedTxCount > 0 ? resolvedTxCount / acceptedTxCount : null;
  const notes: string[] = [];

  if (queryStepSeconds !== null) {
    notes.push(
      `Scrape step is approximately ${queryStepSeconds.toFixed(0)}s; latency resolution is bounded by this interval.`
    );
  } else {
    notes.push('Scrape step could not be inferred from the available counter samples.');
  }

  if (resolvedRatio === null) {
    notes.push('No accepted cohorts were observed in this tier window.');
  } else if (resolvedRatio < 1) {
    notes.push(
      `Only ${(resolvedRatio * 100).toFixed(1)}% of accepted transactions were matched to committed progress before window end.`
    );
  } else {
    notes.push('All accepted cohorts observed in this tier were matched to committed progress.');
  }

  if (weightedLatencies.length === 0) {
    return {
      ...base,
      queryStepSeconds,
      acceptedCohortCount,
      resolvedCohortCount,
      acceptedTxCount,
      resolvedTxCount,
      resolvedRatio,
      confidenceNotes: notes,
    };
  }

  let confidence: ConfidenceLevel = 'low';
  if (resolvedRatio !== null && resolvedRatio >= 0.95 && resolvedCohortCount >= 5) {
    confidence = 'high';
  } else if (resolvedRatio !== null && resolvedRatio >= 0.8 && resolvedCohortCount >= 3) {
    confidence = 'medium';
  }

  if (resolvedTxCount < 100) {
    confidence = confidence === 'high' ? 'medium' : 'low';
    notes.push(
      `Resolved transaction volume is low (${resolvedTxCount.toFixed(0)}), so percentile confidence is reduced.`
    );
  }

  return {
    method: ACCEPTED_TO_COMMITTED_METHOD,
    confidence,
    confidenceNotes: notes,
    queryStepSeconds,
    acceptedCohortCount,
    resolvedCohortCount,
    acceptedTxCount,
    resolvedTxCount,
    resolvedRatio,
    percentilesMs: {
      p50: percentileWeighted(weightedLatencies, 50),
      p95: percentileWeighted(weightedLatencies, 95),
      p99: percentileWeighted(weightedLatencies, 99),
    },
  };
}
