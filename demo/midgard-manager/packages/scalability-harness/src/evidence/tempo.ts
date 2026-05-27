export type Fetcher = (url: string, init: RequestInit) => Promise<Response>;

export class TempoQueryError extends Error {
  readonly endpoint: string;
  readonly serviceName: string;

  constructor(message: string, endpoint: string, serviceName: string) {
    super(message);
    this.name = 'TempoQueryError';
    this.endpoint = endpoint;
    this.serviceName = serviceName;
  }
}

export interface TempoTraceSummary {
  traceId: string;
  rootName: string;
  rootServiceName: string;
  startTimeUnixNano: string;
  durationMs: number;
}

export interface TempoSearchResult {
  traces: TempoTraceSummary[];
  inspectedTraces: number | null;
  // True when returned trace count equals limit — response may be incomplete.
  truncated: boolean;
}

export interface TempoTierCapture {
  tierIndex: number;
  targetTps: number;
  capturedAt: string;
  serviceName: string;
  startedAt: string;
  recoveryStoppedAt: string;
  result: TempoSearchResult | null;
  error: string | null;
}

export const DEFAULT_TEMPO_SERVICE_NAME = 'midgard-node';
const DEFAULT_LIMIT = 500;

// Tempo search HTTP API wire shapes
interface RawTempoTrace {
  traceID: string;
  rootName: string;
  rootServiceName: string;
  startTimeUnixNano: string;
  durationMs: number;
}

interface RawTempoResponse {
  traces?: RawTempoTrace[];
  metrics?: {
    inspectedTraces?: number;
    inspectedBytes?: number;
  };
}

export class TempoClient {
  private readonly endpoint: string;
  private readonly fetcher: Fetcher;

  constructor(endpoint: string, fetcher?: Fetcher) {
    this.endpoint = endpoint.replace(/\/$/, '');
    this.fetcher = fetcher ?? (globalThis.fetch as Fetcher);
  }

  async searchTraces(
    serviceName: string = DEFAULT_TEMPO_SERVICE_NAME,
    start: Date,
    end: Date,
    limit: number = DEFAULT_LIMIT
  ): Promise<TempoSearchResult> {
    // Tempo search expects Unix timestamps in seconds.
    const startSec = Math.floor(start.getTime() / 1000).toString();
    const endSec = Math.ceil(end.getTime() / 1000).toString();

    const params = new URLSearchParams({
      'service.name': serviceName,
      start: startSec,
      end: endSec,
      limit: String(limit),
    });

    const url = `${this.endpoint}/api/search?${params.toString()}`;

    let raw: Response;
    try {
      raw = await this.fetcher(url, { method: 'GET' });
    } catch (err) {
      throw new TempoQueryError(
        `Tempo request failed: ${err instanceof Error ? err.message : String(err)}`,
        this.endpoint,
        serviceName
      );
    }

    if (!raw.ok) {
      throw new TempoQueryError(
        `Tempo search returned HTTP ${raw.status}`,
        this.endpoint,
        serviceName
      );
    }

    let body: RawTempoResponse;
    try {
      body = (await raw.json()) as RawTempoResponse;
    } catch (err) {
      throw new TempoQueryError(
        `Failed to parse Tempo response: ${err instanceof Error ? err.message : String(err)}`,
        this.endpoint,
        serviceName
      );
    }

    const traces: TempoTraceSummary[] = (body.traces ?? []).map((t) => ({
      traceId: t.traceID,
      rootName: t.rootName,
      rootServiceName: t.rootServiceName,
      startTimeUnixNano: t.startTimeUnixNano,
      durationMs: t.durationMs,
    }));

    const inspectedTraces = body.metrics?.inspectedTraces ?? null;
    const truncated = traces.length >= limit;

    return { traces, inspectedTraces, truncated };
  }
}
