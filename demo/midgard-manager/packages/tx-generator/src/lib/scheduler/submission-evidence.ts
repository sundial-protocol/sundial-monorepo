export const REQUEST_EVENT_MODES = ['off', 'sampled', 'all'] as const;
export type RequestEventsMode = (typeof REQUEST_EVENT_MODES)[number];

export const REQUEST_EVENTS_SAMPLE_RATE = 0.1;
export const LATENCY_BUCKET_UPPER_BOUNDS_MS = [
  50, 100, 250, 500, 1_000, 2_000, 5_000, 10_000,
] as const;

export type SubmissionOutcome =
  | 'submitted'
  | 'rejected'
  | 'node_unavailable'
  | 'timed_out'
  | 'error';

export interface SubmissionCounters {
  generated: number;
  attempted: number;
  submitted: number;
  rejected: number;
  node_unavailable: number;
  timed_out: number;
  error: number;
}

export interface SubmissionLatencyHistogram {
  boundsMs: number[];
  counts: number[];
  overflowCount: number;
  count: number;
  sumMs: number;
  minMs: number | null;
  maxMs: number | null;
}

export interface SubmissionAggregate {
  counters: SubmissionCounters;
  retries: {
    totalRetries: number;
    submissionsRetried: number;
    maxRetryCount: number;
  };
  latencyMs: {
    submitted: SubmissionLatencyHistogram;
    rejected: SubmissionLatencyHistogram;
    node_unavailable: SubmissionLatencyHistogram;
    timed_out: SubmissionLatencyHistogram;
    error: SubmissionLatencyHistogram;
  };
  schedulerMetrics: {
    prepared_queue_depth: {
      current: number;
      max: number;
    };
    in_flight_submits: {
      current: number;
      max: number;
    };
    send_tokens_late_total: number;
    generation_latency: SubmissionLatencyHistogram;
    submit_latency: SubmissionLatencyHistogram;
    queue_backpressure_wait_latency: SubmissionLatencyHistogram;
    token_wait_latency: SubmissionLatencyHistogram;
    lucid_pool_wait_latency: SubmissionLatencyHistogram;
  };
}

export interface SubmissionRequestEvent {
  ts: string;
  txId: string;
  finalOutcome: SubmissionOutcome;
  outcome: SubmissionOutcome;
  responseClass: string;
  httpStatusCode: number | null;
  errorClass: string | null;
  latencyMs: number | null;
  retryCount: number;
  transactionType: string;
  transactionProfile: string;
  cborByteSize: number | null;
  midgardByteSize: number | null;
  error?: string;
}

export interface SubmissionAggregateWithPercentiles extends SubmissionAggregate {
  percentilesMs: {
    submitted: { p50: number | null; p95: number | null; p99: number | null };
    rejected: { p50: number | null; p95: number | null; p99: number | null };
    node_unavailable: { p50: number | null; p95: number | null; p99: number | null };
    timed_out: { p50: number | null; p95: number | null; p99: number | null };
    error: { p50: number | null; p95: number | null; p99: number | null };
    schedulerMetrics: {
      generation_latency: { p50: number | null; p95: number | null; p99: number | null };
      submit_latency: { p50: number | null; p95: number | null; p99: number | null };
      queue_backpressure_wait_latency: { p50: number | null; p95: number | null; p99: number | null };
      token_wait_latency: { p50: number | null; p95: number | null; p99: number | null };
      lucid_pool_wait_latency: { p50: number | null; p95: number | null; p99: number | null };
    };
  };
}

function createHistogram(): SubmissionLatencyHistogram {
  return {
    boundsMs: [...LATENCY_BUCKET_UPPER_BOUNDS_MS],
    counts: LATENCY_BUCKET_UPPER_BOUNDS_MS.map(() => 0),
    overflowCount: 0,
    count: 0,
    sumMs: 0,
    minMs: null,
    maxMs: null,
  };
}

function recordHistogramLatency(histogram: SubmissionLatencyHistogram, latencyMs: number): void {
  histogram.count += 1;
  histogram.sumMs += latencyMs;
  histogram.minMs = histogram.minMs === null ? latencyMs : Math.min(histogram.minMs, latencyMs);
  histogram.maxMs = histogram.maxMs === null ? latencyMs : Math.max(histogram.maxMs, latencyMs);

  const bucketIndex = histogram.boundsMs.findIndex((upperBound) => latencyMs <= upperBound);
  if (bucketIndex === -1) {
    histogram.overflowCount += 1;
    return;
  }
  histogram.counts[bucketIndex] += 1;
}

function percentileFromHistogram(
  histogram: SubmissionLatencyHistogram,
  percentile: number
): number | null {
  if (histogram.count === 0) {
    return null;
  }
  const target = Math.ceil((percentile / 100) * histogram.count);
  let runningCount = 0;
  for (let index = 0; index < histogram.boundsMs.length; index += 1) {
    runningCount += histogram.counts[index];
    if (runningCount >= target) {
      return histogram.boundsMs[index];
    }
  }
  return histogram.maxMs;
}

export function createEmptySubmissionAggregate(): SubmissionAggregate {
  return {
    counters: {
      generated: 0,
      attempted: 0,
      submitted: 0,
      rejected: 0,
      node_unavailable: 0,
      timed_out: 0,
      error: 0,
    },
    retries: {
      totalRetries: 0,
      submissionsRetried: 0,
      maxRetryCount: 0,
    },
    latencyMs: {
      submitted: createHistogram(),
      rejected: createHistogram(),
      node_unavailable: createHistogram(),
      timed_out: createHistogram(),
      error: createHistogram(),
    },
    schedulerMetrics: {
      prepared_queue_depth: {
        current: 0,
        max: 0,
      },
      in_flight_submits: {
        current: 0,
        max: 0,
      },
      send_tokens_late_total: 0,
      generation_latency: createHistogram(),
      submit_latency: createHistogram(),
      queue_backpressure_wait_latency: createHistogram(),
      token_wait_latency: createHistogram(),
      lucid_pool_wait_latency: createHistogram(),
    },
  };
}

export function recordGeneratedTransactions(aggregate: SubmissionAggregate, count: number): void {
  aggregate.counters.generated += count;
}

export function recordAttemptedSubmission(aggregate: SubmissionAggregate): void {
  aggregate.counters.attempted += 1;
}

export function recordSubmissionObservation(
  aggregate: SubmissionAggregate,
  outcome: SubmissionOutcome,
  latencyMs: number | null,
  retryCount: number
): void {
  aggregate.counters[outcome] += 1;
  aggregate.retries.totalRetries += retryCount;
  if (retryCount > 0) {
    aggregate.retries.submissionsRetried += 1;
    aggregate.retries.maxRetryCount = Math.max(aggregate.retries.maxRetryCount, retryCount);
  }
  if (latencyMs !== null) {
    recordHistogramLatency(aggregate.latencyMs[outcome], latencyMs);
  }
}

export function recordPreparedQueueDepth(aggregate: SubmissionAggregate, depth: number): void {
  aggregate.schedulerMetrics.prepared_queue_depth.current = depth;
  aggregate.schedulerMetrics.prepared_queue_depth.max = Math.max(
    aggregate.schedulerMetrics.prepared_queue_depth.max,
    depth
  );
}

export function recordInFlightSubmits(aggregate: SubmissionAggregate, inFlight: number): void {
  aggregate.schedulerMetrics.in_flight_submits.current = inFlight;
  aggregate.schedulerMetrics.in_flight_submits.max = Math.max(
    aggregate.schedulerMetrics.in_flight_submits.max,
    inFlight
  );
}

export function recordTokenLate(aggregate: SubmissionAggregate): void {
  aggregate.schedulerMetrics.send_tokens_late_total += 1;
}

export function recordGenerationLatency(aggregate: SubmissionAggregate, latencyMs: number): void {
  recordHistogramLatency(aggregate.schedulerMetrics.generation_latency, latencyMs);
}

export function recordSubmitLatency(aggregate: SubmissionAggregate, latencyMs: number): void {
  recordHistogramLatency(aggregate.schedulerMetrics.submit_latency, latencyMs);
}

export function recordQueueBackpressureWaitLatency(
  aggregate: SubmissionAggregate,
  latencyMs: number
): void {
  recordHistogramLatency(aggregate.schedulerMetrics.queue_backpressure_wait_latency, latencyMs);
}

export function recordTokenWaitLatency(aggregate: SubmissionAggregate, latencyMs: number): void {
  recordHistogramLatency(aggregate.schedulerMetrics.token_wait_latency, latencyMs);
}

export function recordLucidPoolWaitLatency(aggregate: SubmissionAggregate, latencyMs: number): void {
  recordHistogramLatency(aggregate.schedulerMetrics.lucid_pool_wait_latency, latencyMs);
}

export function toSubmissionAggregateWithPercentiles(
  aggregate: SubmissionAggregate
): SubmissionAggregateWithPercentiles {
  return {
    ...aggregate,
    percentilesMs: {
      submitted: {
        p50: percentileFromHistogram(aggregate.latencyMs.submitted, 50),
        p95: percentileFromHistogram(aggregate.latencyMs.submitted, 95),
        p99: percentileFromHistogram(aggregate.latencyMs.submitted, 99),
      },
      rejected: {
        p50: percentileFromHistogram(aggregate.latencyMs.rejected, 50),
        p95: percentileFromHistogram(aggregate.latencyMs.rejected, 95),
        p99: percentileFromHistogram(aggregate.latencyMs.rejected, 99),
      },
      node_unavailable: {
        p50: percentileFromHistogram(aggregate.latencyMs.node_unavailable, 50),
        p95: percentileFromHistogram(aggregate.latencyMs.node_unavailable, 95),
        p99: percentileFromHistogram(aggregate.latencyMs.node_unavailable, 99),
      },
      timed_out: {
        p50: percentileFromHistogram(aggregate.latencyMs.timed_out, 50),
        p95: percentileFromHistogram(aggregate.latencyMs.timed_out, 95),
        p99: percentileFromHistogram(aggregate.latencyMs.timed_out, 99),
      },
      error: {
        p50: percentileFromHistogram(aggregate.latencyMs.error, 50),
        p95: percentileFromHistogram(aggregate.latencyMs.error, 95),
        p99: percentileFromHistogram(aggregate.latencyMs.error, 99),
      },
      schedulerMetrics: {
        generation_latency: {
          p50: percentileFromHistogram(aggregate.schedulerMetrics.generation_latency, 50),
          p95: percentileFromHistogram(aggregate.schedulerMetrics.generation_latency, 95),
          p99: percentileFromHistogram(aggregate.schedulerMetrics.generation_latency, 99),
        },
        submit_latency: {
          p50: percentileFromHistogram(aggregate.schedulerMetrics.submit_latency, 50),
          p95: percentileFromHistogram(aggregate.schedulerMetrics.submit_latency, 95),
          p99: percentileFromHistogram(aggregate.schedulerMetrics.submit_latency, 99),
        },
        queue_backpressure_wait_latency: {
          p50: percentileFromHistogram(aggregate.schedulerMetrics.queue_backpressure_wait_latency, 50),
          p95: percentileFromHistogram(aggregate.schedulerMetrics.queue_backpressure_wait_latency, 95),
          p99: percentileFromHistogram(aggregate.schedulerMetrics.queue_backpressure_wait_latency, 99),
        },
        token_wait_latency: {
          p50: percentileFromHistogram(aggregate.schedulerMetrics.token_wait_latency, 50),
          p95: percentileFromHistogram(aggregate.schedulerMetrics.token_wait_latency, 95),
          p99: percentileFromHistogram(aggregate.schedulerMetrics.token_wait_latency, 99),
        },
        lucid_pool_wait_latency: {
          p50: percentileFromHistogram(aggregate.schedulerMetrics.lucid_pool_wait_latency, 50),
          p95: percentileFromHistogram(aggregate.schedulerMetrics.lucid_pool_wait_latency, 95),
          p99: percentileFromHistogram(aggregate.schedulerMetrics.lucid_pool_wait_latency, 99),
        },
      },
    },
  };
}
