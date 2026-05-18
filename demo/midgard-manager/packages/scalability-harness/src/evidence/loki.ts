export type Fetcher = (url: string, init: RequestInit) => Promise<Response>;

export class LokiQueryError extends Error {
  readonly endpoint: string;
  readonly query: string;

  constructor(message: string, endpoint: string, query: string) {
    super(message);
    this.name = 'LokiQueryError';
    this.endpoint = endpoint;
    this.query = query;
  }
}

export interface LokiLogEntry {
  timestampNs: string;
  line: string;
}

export interface LokiStream {
  labels: Record<string, string>;
  entries: LokiLogEntry[];
}

export interface LokiQueryResult {
  streams: LokiStream[];
  totalEntries: number;
  // True when returned entry count equals limit — response may be incomplete.
  truncated: boolean;
}

export interface LokiTierCapture {
  tierIndex: number;
  targetTps: number;
  capturedAt: string;
  query: string;
  startedAt: string;
  recoveryStoppedAt: string;
  result: LokiQueryResult | null;
  error: string | null;
}

export const DEFAULT_LOKI_QUERY = '{job="containerlogs"}';
const DEFAULT_LIMIT = 5_000;

// Loki query_range HTTP API wire shapes
interface RawLokiStream {
  stream: Record<string, string>;
  values: Array<[string, string]>; // [nanosecond_timestamp_string, line]
}

interface RawLokiData {
  resultType: 'streams';
  result: RawLokiStream[];
}

interface RawLokiResponse {
  status: 'success' | 'error';
  data?: RawLokiData;
  errorType?: string;
  error?: string;
}

export class LokiClient {
  private readonly endpoint: string;
  private readonly fetcher: Fetcher;

  constructor(endpoint: string, fetcher?: Fetcher) {
    this.endpoint = endpoint.replace(/\/$/, '');
    this.fetcher = fetcher ?? (globalThis.fetch as Fetcher);
  }

  async queryRange(
    query: string = DEFAULT_LOKI_QUERY,
    start: Date,
    end: Date,
    limit: number = DEFAULT_LIMIT
  ): Promise<LokiQueryResult> {
    // Loki query_range expects nanosecond Unix timestamps.
    const startNs = (start.getTime() * 1_000_000).toString();
    const endNs = (end.getTime() * 1_000_000).toString();

    const params = new URLSearchParams({
      query,
      start: startNs,
      end: endNs,
      limit: String(limit),
      direction: 'forward',
    });

    const url = `${this.endpoint}/loki/api/v1/query_range?${params.toString()}`;

    let raw: Response;
    try {
      raw = await this.fetcher(url, { method: 'GET' });
    } catch (err) {
      throw new LokiQueryError(
        `Loki request failed: ${err instanceof Error ? err.message : String(err)}`,
        this.endpoint,
        query
      );
    }

    if (!raw.ok) {
      throw new LokiQueryError(
        `Loki query_range returned HTTP ${raw.status}`,
        this.endpoint,
        query
      );
    }

    let body: RawLokiResponse;
    try {
      body = (await raw.json()) as RawLokiResponse;
    } catch (err) {
      throw new LokiQueryError(
        `Failed to parse Loki response: ${err instanceof Error ? err.message : String(err)}`,
        this.endpoint,
        query
      );
    }

    if (body.status !== 'success' || body.data === undefined) {
      throw new LokiQueryError(
        `Loki query_range error: ${body.error ?? body.errorType ?? 'unknown'}`,
        this.endpoint,
        query
      );
    }

    const streams: LokiStream[] = body.data.result.map((s) => ({
      labels: s.stream,
      entries: s.values.map(([ts, line]) => ({ timestampNs: ts, line })),
    }));

    const totalEntries = streams.reduce((acc, s) => acc + s.entries.length, 0);
    const truncated = totalEntries >= limit;

    return { streams, totalEntries, truncated };
  }
}
