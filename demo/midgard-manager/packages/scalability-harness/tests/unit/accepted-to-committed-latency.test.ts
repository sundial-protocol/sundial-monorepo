import { describe, expect, it } from 'vitest';

import {
  ACCEPTED_COUNTER_QUERY,
  COMMITTED_COUNTER_QUERY,
  estimateAcceptedToCommittedLatency,
} from '../../src/analysis/accepted-to-committed-latency.js';
import type { TierMetricWindow } from '../../src/metrics/window.js';

function makeWindow(
  acceptedValues: Array<[number, number]>,
  committedValues: Array<[number, number]>
): TierMetricWindow {
  return {
    tierIndex: 0,
    targetTps: 100,
    startedAt: '2025-01-01T00:00:00.000Z',
    stoppedAt: '2025-01-01T00:01:00.000Z',
    recoveryStartedAt: '2025-01-01T00:01:00.000Z',
    recoveryStoppedAt: '2025-01-01T00:01:30.000Z',
    before: {},
    afterLoad: {},
    afterRecovery: {},
    ranges: {
      [ACCEPTED_COUNTER_QUERY]: [
        {
          metric: {},
          values: acceptedValues.map(([ts, value]) => [ts, String(value)]),
        },
      ],
      [COMMITTED_COUNTER_QUERY]: [
        {
          metric: {},
          values: committedValues.map(([ts, value]) => [ts, String(value)]),
        },
      ],
    },
  };
}

describe('estimateAcceptedToCommittedLatency', () => {
  it('computes weighted p50/p95/p99 from cohort alignment', () => {
    const window = makeWindow(
      [
        [0, 0],
        [15, 100],
        [30, 200],
        [45, 300],
      ],
      [
        [0, 0],
        [15, 50],
        [30, 150],
        [45, 250],
        [60, 350],
      ]
    );

    const estimate = estimateAcceptedToCommittedLatency(window);
    expect(estimate.percentilesMs.p50).toBe(15_000);
    expect(estimate.percentilesMs.p95).toBe(15_000);
    expect(estimate.percentilesMs.p99).toBe(15_000);
    expect(estimate.resolvedRatio).toBe(1);
    expect(estimate.confidence).toBe('medium');
  });

  it('marks confidence low when unresolved cohorts reduce coverage', () => {
    const window = makeWindow(
      [
        [0, 0],
        [15, 100],
        [30, 200],
        [45, 300],
      ],
      [
        [0, 0],
        [15, 0],
        [30, 50],
        [45, 100],
      ]
    );

    const estimate = estimateAcceptedToCommittedLatency(window);
    expect(estimate.percentilesMs.p95).toBe(30_000);
    expect(estimate.resolvedRatio).toBeCloseTo(1 / 3, 4);
    expect(estimate.confidence).toBe('low');
    expect(estimate.confidenceNotes.join(' ')).toContain('matched to committed progress');
  });

  it('returns insufficient_data when no window is available', () => {
    const estimate = estimateAcceptedToCommittedLatency(null);
    expect(estimate.percentilesMs.p95).toBeNull();
    expect(estimate.confidence).toBe('insufficient_data');
    expect(estimate.confidenceNotes.join(' ')).toContain('cannot be estimated');
  });
});
