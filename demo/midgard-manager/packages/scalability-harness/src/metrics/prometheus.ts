// Raw Prometheus HTTP API wire shapes
interface RawVectorData {
  resultType: 'vector';
  result: Array<{
    metric: Record<string, string>;
    value: [number, string];
  }>;
}

interface RawMatrixData {
  resultType: 'matrix';
  result: Array<{
    metric: Record<string, string>;
    values: Array<[number, string]>;
  }>;
}

interface RawApiResponse<T> {
  status: 'success' | 'error';
  data?: T;
  errorType?: string;
  error?: string;
  warnings?: string[];
}

// Public result types
export interface VectorSample {
  metric: Record<string, string>;
  value: [number, string]; // [unix_timestamp_seconds, string_value]
}

export interface MatrixSample {
  metric: Record<string, string>;
  values: Array<[number, string]>; // [[unix_timestamp_seconds, string_value], ...]
}

export type PrometheusVectorResult = VectorSample[];
export type PrometheusMatrixResult = MatrixSample[];

export class PrometheusQueryError extends Error {
  readonly query: string;
  readonly errorType?: string;

  constructor(message: string, query: string, errorType?: string) {
    super(message);
    this.name = 'PrometheusQueryError';
    this.query = query;
    this.errorType = errorType;
  }
}

export type Fetcher = (url: string, init: RequestInit) => Promise<Response>;

// ---------------------------------------------------------------------------
// Node metrics — primary evidence. Missing ones mark tier evidence-incomplete.
// ---------------------------------------------------------------------------

export const NODE_METRICS = [
  'tx_submissions_enqueued_total',
  'tx_submissions_rejected_total',
  'tx_submissions_mempool_accepted_total',
  'tx_stream_fail_total',
  'tx_stream_depth',
  'tx_stream_depth_peak',
  'mempool_tx_count',
  'unsubmitted_block_backlog',
  'commit_block_count_total',
  'submit_block_count_total',
  'commit_block_tx_count_total',
  'commit_block_l1_user_events',
  'commit_block_txs_per_block',
  'commit_block_events_size_bytes',
  'commit_block_duration_seconds_sum',
  'commit_block_duration_seconds_count',
  'l1_commitment_fees_lovelace_total',
  'l1_commitment_fee_lovelace_last',
  'commit_block_commitment_failures_total',
  'merge_block_count_total',
  'merge_block_failures_total',
  'up{job="sundial_nodes"}',
] as const;

export type NodeMetric = (typeof NODE_METRICS)[number];

// Metrics that are always present on a running node regardless of tx activity:
// the scrape target health metric and fiber-emitted gauges.
// Used by the preflight check to distinguish "node running but idle" from
// "node not running / telemetry pipeline broken".
export const ALWAYS_PRESENT_NODE_METRICS = [
  'up{job="sundial_nodes"}',
  'tx_stream_depth',
  'tx_stream_depth_peak',
  'mempool_tx_count',
  'unsubmitted_block_backlog',
] as const;

export type AlwaysPresentNodeMetric = (typeof ALWAYS_PRESENT_NODE_METRICS)[number];

// Counter metrics that only appear in Prometheus after the first matching event.
// Their absence on a freshly-started idle node is expected and should not block
// the preflight check.
export const COUNTER_NODE_METRICS = NODE_METRICS.filter(
  (m): m is Exclude<NodeMetric, AlwaysPresentNodeMetric> =>
    !(ALWAYS_PRESENT_NODE_METRICS as readonly string[]).includes(m)
);

// ---------------------------------------------------------------------------
// cAdvisor metrics — optional. Missing ones do not fail the benchmark.
// ---------------------------------------------------------------------------

export const CADVISOR_METRICS = [
  'container_memory_usage_bytes{image!=""}',
  'container_last_seen{image!=""}',
  'rate(container_cpu_user_seconds_total{image!=""}[1m])',
  'rate(container_network_receive_bytes_total{image!=""}[1m])',
  'rate(container_network_transmit_bytes_total{image!=""}[1m])',
] as const;

export type CadvisorMetric = (typeof CADVISOR_METRICS)[number];

// ---------------------------------------------------------------------------
// PrometheusSamples — written to prometheus-samples.json
// ---------------------------------------------------------------------------

export interface MetricSnapshot {
  query: string;
  capturedAt: string;
  result: PrometheusVectorResult | null;
  error?: string;
}

export interface PrometheusSamples {
  capturedAt: string;
  tierIndex?: number;
  nodeMetrics: MetricSnapshot[];
  cadvisorMetrics: MetricSnapshot[];
  evidenceIncomplete: boolean;
  missingPrimaryMetrics: string[];
}

// Removes Prometheus label-selector blocks {…} from a query string.
// Character scan avoids the O(n²) worst case that a regex like /\{[^}]*\}/g
// exhibits on strings with many unmatched '{' and no closing '}'.
export function stripLabelSelectors(query: string): string {
  let result = '';
  let depth = 0;
  for (let i = 0; i < query.length; i++) {
    const ch = query[i];
    if (ch === '{') {
      depth++;
    } else if (ch === '}') {
      if (depth > 0) depth--;
    } else if (depth === 0) {
      result += ch;
    }
  }
  return result;
}

// Flat scalar map used in PrometheusSnapshotEvent.metrics.
// Takes the first series' value from each snapshot; skips unparseable entries.
export function flattenToScalars(snapshots: MetricSnapshot[]): Record<string, number> {
  const out: Record<string, number> = {};
  for (const snap of snapshots) {
    if (!snap.result || snap.result.length === 0) continue;
    const raw = snap.result[0].value[1];
    const num = parseFloat(raw);
    if (isNaN(num)) continue;
    const key = stripLabelSelectors(snap.query).replace(/^rate\(([^[]+)\[.*\]\)$/, '$1');
    out[key] = num;
  }
  return out;
}

// ---------------------------------------------------------------------------
// PrometheusClient
// ---------------------------------------------------------------------------

export class PrometheusClient {
  private readonly baseUrl: string;
  private readonly fetcher: Fetcher;

  constructor(baseUrl: string, fetcher: Fetcher = (url, init) => fetch(url, init)) {
    this.baseUrl = baseUrl.replace(/\/$/, '');
    this.fetcher = fetcher;
  }

  async queryInstant(query: string, time?: Date): Promise<PrometheusVectorResult> {
    const params = new URLSearchParams({ query });
    if (time !== undefined) {
      params.set('time', String(time.getTime() / 1000));
    }

    const url = `${this.baseUrl}/api/v1/query?${params.toString()}`;
    const body = await this.request<RawVectorData>(url, query);

    if (body.resultType !== 'vector') {
      throw new PrometheusQueryError(`Expected vector result, got ${body.resultType}`, query);
    }

    return body.result;
  }

  async queryRange(
    query: string,
    start: Date,
    end: Date,
    stepSeconds: number
  ): Promise<PrometheusMatrixResult> {
    const params = new URLSearchParams({
      query,
      start: String(start.getTime() / 1000),
      end: String(end.getTime() / 1000),
      step: String(stepSeconds),
    });

    const url = `${this.baseUrl}/api/v1/query_range?${params.toString()}`;
    const body = await this.request<RawMatrixData>(url, query);

    if (body.resultType !== 'matrix') {
      throw new PrometheusQueryError(`Expected matrix result, got ${body.resultType}`, query);
    }

    return body.result;
  }

  private async request<T extends RawVectorData | RawMatrixData>(
    url: string,
    query: string
  ): Promise<T> {
    let response: Response;
    try {
      response = await this.fetcher(url, {});
    } catch (err) {
      throw new PrometheusQueryError(
        `Network error querying Prometheus: ${err instanceof Error ? err.message : String(err)}`,
        query
      );
    }

    if (!response.ok) {
      throw new PrometheusQueryError(
        `Prometheus HTTP ${response.status} for query: ${query}`,
        query
      );
    }

    let parsed: RawApiResponse<T>;
    try {
      parsed = (await response.json()) as RawApiResponse<T>;
    } catch (err) {
      throw new PrometheusQueryError(
        `Failed to parse Prometheus response: ${err instanceof Error ? err.message : String(err)}`,
        query
      );
    }

    if (parsed.status === 'error') {
      throw new PrometheusQueryError(
        parsed.error ?? 'Prometheus returned status:error',
        query,
        parsed.errorType
      );
    }

    if (parsed.data === undefined) {
      throw new PrometheusQueryError('Prometheus response missing data field', query);
    }

    return parsed.data;
  }
}

// ---------------------------------------------------------------------------
// Snapshot helper — queries all metrics and assembles PrometheusSamples
// ---------------------------------------------------------------------------

async function captureSnapshot(
  client: PrometheusClient,
  query: string,
  time?: Date
): Promise<MetricSnapshot> {
  const capturedAt = (time ?? new Date()).toISOString();
  try {
    const result = await client.queryInstant(query, time);
    return { query, capturedAt, result };
  } catch (err) {
    return {
      query,
      capturedAt,
      result: null,
      error: err instanceof Error ? err.message : String(err),
    };
  }
}

export async function snapshotNodeMetrics(
  client: PrometheusClient,
  tierIndex?: number,
  time?: Date
): Promise<PrometheusSamples> {
  const capturedAt = (time ?? new Date()).toISOString();

  const [nodeSnapshots, cadvisorSnapshots] = await Promise.all([
    Promise.all(NODE_METRICS.map((q) => captureSnapshot(client, q, time))),
    Promise.all(CADVISOR_METRICS.map((q) => captureSnapshot(client, q, time))),
  ]);

  const missingPrimaryMetrics = nodeSnapshots.filter((s) => s.result === null).map((s) => s.query);

  return {
    capturedAt,
    tierIndex,
    nodeMetrics: nodeSnapshots,
    cadvisorMetrics: cadvisorSnapshots,
    evidenceIncomplete: missingPrimaryMetrics.length > 0,
    missingPrimaryMetrics,
  };
}
