import { describe, expect, it } from 'vitest';

import {
  createEmptySubmissionAggregate,
  recordGenerationLatency,
  recordInFlightSubmits,
  recordPreparedQueueDepth,
  recordSubmitLatency,
  recordTokenLate,
  toSubmissionAggregateWithPercentiles,
} from '../../src/lib/scheduler/submission-evidence.js';

describe('submission evidence scheduler metrics', () => {
  it('tracks queue depth, in-flight submissions, token lag, and latency percentiles', () => {
    const aggregate = createEmptySubmissionAggregate();

    recordPreparedQueueDepth(aggregate, 2);
    recordPreparedQueueDepth(aggregate, 7);
    recordInFlightSubmits(aggregate, 1);
    recordInFlightSubmits(aggregate, 3);
    recordTokenLate(aggregate);
    recordTokenLate(aggregate);
    recordGenerationLatency(aggregate, 40);
    recordGenerationLatency(aggregate, 80);
    recordSubmitLatency(aggregate, 120);
    recordSubmitLatency(aggregate, 260);

    const withPercentiles = toSubmissionAggregateWithPercentiles(aggregate);

    expect(withPercentiles.schedulerMetrics.prepared_queue_depth.current).toBe(7);
    expect(withPercentiles.schedulerMetrics.prepared_queue_depth.max).toBe(7);
    expect(withPercentiles.schedulerMetrics.in_flight_submits.current).toBe(3);
    expect(withPercentiles.schedulerMetrics.in_flight_submits.max).toBe(3);
    expect(withPercentiles.schedulerMetrics.send_tokens_late_total).toBe(2);
    expect(withPercentiles.percentilesMs.schedulerMetrics.generation_latency.p50).toBe(50);
    expect(withPercentiles.percentilesMs.schedulerMetrics.submit_latency.p95).toBe(500);
  });
});
