// Loads demo/midgard-node/slo/slo.json (the SLO source of truth shared with the
// Prometheus recording/alert rules) and turns each SLI into PromQL evaluated
// over an arbitrary reporting window. Report-time queries are built here
// independently of the deployed recording rules so a reliability report can be
// produced (and can cross-check the rules) even where the rules are not loaded.

import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';

export type RatioSlo = {
  id: string;
  kind: 'ratio';
  objective: number;
  description: string;
  good: string;
  total: string;
  ratioClampMax?: number;
};

export type LatencySlo = {
  id: string;
  kind: 'latency';
  objectiveSeconds: number;
  description: string;
  bucketMetric: string;
};

export type GaugeAvgSlo = {
  id: string;
  kind: 'gauge_avg';
  objective: number;
  alertThreshold?: number;
  description: string;
  expr: string;
};

export type Slo = RatioSlo | LatencySlo | GaugeAvgSlo;

export type SloConfig = {
  job: string;
  errorBudgetWindow: string;
  latencyQuantiles: number[];
  slos: Slo[];
  sourcePath: string;
};

export class SloConfigError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'SloConfigError';
  }
}

const CANDIDATE_RELATIVE_PATHS = [
  'demo/midgard-node/slo/slo.json',
  '../midgard-node/slo/slo.json',
  '../../midgard-node/slo/slo.json',
  '../../../midgard-node/slo/slo.json',
];

/** Resolves the slo.json path from an explicit flag or well-known locations. */
export function resolveSloPath(explicit: string | undefined, cwd = process.cwd()): string {
  if (explicit !== undefined) {
    const resolved = path.resolve(cwd, explicit);
    if (!existsSync(resolved)) {
      throw new SloConfigError(`--slo path does not exist: ${explicit}`);
    }
    return resolved;
  }
  for (const rel of CANDIDATE_RELATIVE_PATHS) {
    const candidate = path.resolve(cwd, rel);
    if (existsSync(candidate)) return candidate;
  }
  throw new SloConfigError(
    'Could not locate slo.json. Pass --slo <path> (usually demo/midgard-node/slo/slo.json).'
  );
}

export function loadSloConfig(sloPath: string): SloConfig {
  let raw: unknown;
  try {
    raw = JSON.parse(readFileSync(sloPath, 'utf8'));
  } catch (err) {
    throw new SloConfigError(`Failed to read/parse slo.json: ${String(err)}`);
  }
  const o = raw as Record<string, unknown>;
  if (!Array.isArray(o.slos) || typeof o.job !== 'string') {
    throw new SloConfigError('slo.json missing required fields (job, slos)');
  }
  const slos = o.slos as Slo[];
  for (const s of slos) {
    if (!s.id || !s.kind)
      throw new SloConfigError(`slo entry missing id/kind: ${JSON.stringify(s)}`);
  }
  return {
    job: o.job,
    errorBudgetWindow: typeof o.errorBudgetWindow === 'string' ? o.errorBudgetWindow : '30d',
    latencyQuantiles: Array.isArray(o.latencyQuantiles)
      ? (o.latencyQuantiles as number[])
      : [0.5, 0.95, 0.99],
    slos,
    sourcePath: sloPath,
  };
}

// --- PromQL builders (report-time, whole-window aggregates) ---

const subWindow = (expr: string, w: string): string => expr.replaceAll('$w', w);

/** Instant PromQL for the achieved ratio of a ratio SLI over the whole window. */
export function ratioAggregateExpr(slo: RatioSlo, windowRange: string): string {
  const ratio = `(${subWindow(slo.good, windowRange)}) / (${subWindow(slo.total, windowRange)})`;
  return slo.ratioClampMax !== undefined ? `clamp_max(${ratio}, ${slo.ratioClampMax})` : ratio;
}

/** Rolling ratio timeseries used for charts and SLO-breach detection. */
export function ratioRollingExpr(slo: RatioSlo, rolling = '1h'): string {
  return ratioAggregateExpr(slo, rolling);
}

export function latencyQuantileExpr(
  slo: LatencySlo,
  quantile: number,
  windowRange: string
): string {
  return `histogram_quantile(${quantile}, sum by (le) (rate(${slo.bucketMetric}[${windowRange}])))`;
}

export function latencyRollingExpr(slo: LatencySlo, quantile: number, rolling = '1h'): string {
  return latencyQuantileExpr(slo, quantile, rolling);
}

export function gaugeAvgExpr(slo: GaugeAvgSlo, windowRange: string): string {
  return subWindow(slo.expr, windowRange);
}
