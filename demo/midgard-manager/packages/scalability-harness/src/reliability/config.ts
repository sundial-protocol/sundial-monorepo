// Reliability-report configuration: a closed UTC reporting window plus the
// endpoints and SLO source needed to produce a retrospective report.

export type ReportWindow = {
  from: Date;
  to: Date;
  label: string;
};

export type ReliabilityConfig = {
  window: ReportWindow;
  environment: string;
  prometheusEndpoint: string;
  lokiEndpoint?: string;
  grafanaBaseUrl?: string;
  sloPath: string;
  /** Rolling window for chart/breach timeseries (e.g. "1h"). */
  rollingWindow: string;
  /** Range-query step in seconds for timeseries. */
  stepSeconds: number;
  outputDir: string;
};

export class ReliabilityConfigError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'ReliabilityConfigError';
  }
}

const ISO_DATE = /^\d{4}-\d{2}-\d{2}$/;
const ISO_MONTH = /^\d{4}-\d{2}$/;

function parseInstant(value: string, label: string): Date {
  const d = new Date(ISO_DATE.test(value) ? `${value}T00:00:00.000Z` : value);
  if (Number.isNaN(d.getTime())) {
    throw new ReliabilityConfigError(`${label} is not a valid ISO-8601 timestamp: ${value}`);
  }
  return d;
}

/**
 * Resolves a reporting window from CLI options. Either `--month YYYY-MM`
 * (calendar month, UTC) or `--from`/`--to` (ISO-8601 or YYYY-MM-DD, UTC).
 */
export function resolveWindow(opts: { month?: string; from?: string; to?: string }): ReportWindow {
  if (opts.month !== undefined) {
    if (!ISO_MONTH.test(opts.month)) {
      throw new ReliabilityConfigError(`--month must be YYYY-MM, got: ${opts.month}`);
    }
    const [y, m] = opts.month.split('-').map((s) => Number.parseInt(s, 10));
    const from = new Date(Date.UTC(y, m - 1, 1));
    const to = new Date(Date.UTC(y, m, 1));
    return { from, to, label: opts.month };
  }
  if (opts.from === undefined || opts.to === undefined) {
    throw new ReliabilityConfigError('Provide --month YYYY-MM, or both --from and --to.');
  }
  const from = parseInstant(opts.from, '--from');
  const to = parseInstant(opts.to, '--to');
  if (to.getTime() <= from.getTime()) {
    throw new ReliabilityConfigError('--to must be after --from.');
  }
  const label = `${from.toISOString()} .. ${to.toISOString()}`;
  return { from, to, label };
}

/** Whole-window duration as a PromQL range string (seconds). */
export function windowRangeString(window: ReportWindow): string {
  const seconds = Math.round((window.to.getTime() - window.from.getTime()) / 1000);
  return `${seconds}s`;
}
