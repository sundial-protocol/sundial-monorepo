// Small numeric helpers for turning Prometheus matrix/vector results into the
// aggregates the reliability report needs.

import type { MatrixSample, VectorSample } from '../metrics/prometheus.js';

export type Point = { t: number; v: number }; // t = unix ms

export function matrixToPoints(series: MatrixSample[] | null | undefined): Point[] {
  if (!series || series.length === 0) return [];
  // Reliability queries aggregate away labels, so at most one series is
  // expected; if several come back, take the one with the most samples.
  const chosen = series.reduce((a, b) => (b.values.length > a.values.length ? b : a));
  return chosen.values
    .map(([ts, raw]) => ({ t: ts * 1000, v: Number.parseFloat(raw) }))
    .filter((p) => Number.isFinite(p.v));
}

export function instantScalar(result: VectorSample[] | null | undefined): number | null {
  if (!result || result.length === 0) return null;
  const v = Number.parseFloat(result[0].value[1]);
  return Number.isFinite(v) ? v : null;
}

export function mean(xs: number[]): number | null {
  if (xs.length === 0) return null;
  return xs.reduce((a, b) => a + b, 0) / xs.length;
}

export function stddev(xs: number[]): number | null {
  if (xs.length < 2) return null;
  const m = mean(xs)!;
  const variance = xs.reduce((a, b) => a + (b - m) ** 2, 0) / (xs.length - 1);
  return Math.sqrt(variance);
}

/** Coefficient of variation (stddev / mean); null when mean is 0 or undefined. */
export function coefficientOfVariation(xs: number[]): number | null {
  const m = mean(xs);
  const s = stddev(xs);
  if (m === null || s === null || m === 0) return null;
  return s / m;
}

export function quantile(xs: number[], q: number): number | null {
  if (xs.length === 0) return null;
  const sorted = [...xs].sort((a, b) => a - b);
  const idx = (sorted.length - 1) * q;
  const lo = Math.floor(idx);
  const hi = Math.ceil(idx);
  if (lo === hi) return sorted[lo];
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (idx - lo);
}

export type Interval = { startMs: number; endMs: number };

/**
 * Contiguous runs of points where `predicate` holds, plus runs of missing data
 * longer than `maxGapMs` (treated as "bad" when `treatGapsAsBad`).
 */
export function findIntervals(
  points: Point[],
  predicate: (v: number) => boolean,
  opts: { stepMs: number; maxGapMs?: number; treatGapsAsBad?: boolean } = { stepMs: 15_000 }
): Interval[] {
  const intervals: Interval[] = [];
  let open: Interval | null = null;
  const maxGapMs = opts.maxGapMs ?? opts.stepMs * 3;

  const closeOpen = () => {
    if (open) {
      intervals.push(open);
      open = null;
    }
  };

  for (let i = 0; i < points.length; i++) {
    const p = points[i];
    const prev = points[i - 1];
    if (prev && opts.treatGapsAsBad && p.t - prev.t > maxGapMs) {
      // A data gap counts as bad time.
      if (!open) open = { startMs: prev.t, endMs: p.t };
      else open.endMs = p.t;
    }
    if (predicate(p.v)) {
      if (!open) open = { startMs: p.t, endMs: p.t };
      else open.endMs = p.t;
    } else {
      closeOpen();
    }
  }
  closeOpen();
  return intervals.filter((iv) => iv.endMs > iv.startMs);
}

/** Total counter increase across a monotonically-increasing (with resets) series. */
export function counterIncrease(points: Point[]): number {
  let total = 0;
  for (let i = 1; i < points.length; i++) {
    const delta = points[i].v - points[i - 1].v;
    total += delta >= 0 ? delta : points[i].v; // reset -> count from 0
  }
  return total;
}

/** Distinct step-up events in a value that changes rarely (e.g. start_time). */
export function distinctChangePoints(points: Point[]): number[] {
  const changes: number[] = [];
  for (let i = 1; i < points.length; i++) {
    if (points[i].v !== points[i - 1].v) changes.push(points[i].t);
  }
  return changes;
}
