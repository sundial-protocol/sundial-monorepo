import { describe, expect, it } from 'vitest';

import type { CollapseResult } from '../../src/analysis/collapse.js';
import type { TierSummaryInput } from '../../src/analysis/tier-summary.js';
import { buildTierSummary } from '../../src/analysis/tier-summary.js';
import type { CounterDelta, GaugeSummary, TierWindowSummary } from '../../src/metrics/window.js';
import type { LoadDriverResourceEvidence } from '../../src/runner/host-resources.js';
import type { SubmissionAggregate } from '../../src/runner/tx-generator.js';

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const STARTED_AT = '2025-01-01T00:00:00.000Z';
const STOPPED_AT = '2025-01-01T00:01:00.000Z'; // 60 s later — used as both load end and full window end in default fixture

function makeCounterDelta(query: string, deltaLoad: number | null): CounterDelta {
  return { query, deltaLoad, deltaRecovery: null };
}

function makeGaugeSummary(query: string, peak: number | null, final: number | null): GaugeSummary {
  return { query, peak, final };
}

function makeWindowSummary(
  overrides: Partial<{
    enqueuedDelta: number | null;
    rejectedDelta: number | null;
    mempoolAcceptedDelta: number | null;
    processingFailedDelta: number | null;
    committedTxDelta: number | null;
    committedBlockDelta: number | null;
    submittedBlockDelta: number | null;
    mergeFailureDelta: number | null;
    commitmentFailureDelta: number | null;
    l1CommitmentFeesDeltaLovelace: number | null;
    queuePeak: number | null;
    queueFinal: number | null;
    mempoolPeak: number | null;
    mempoolFinal: number | null;
  }> = {}
): TierWindowSummary {
  const o = {
    enqueuedDelta: 600,
    rejectedDelta: 6,
    mempoolAcceptedDelta: 540,
    processingFailedDelta: 0,
    committedTxDelta: 480,
    committedBlockDelta: 8,
    submittedBlockDelta: 8,
    mergeFailureDelta: 0,
    commitmentFailureDelta: 0,
    l1CommitmentFeesDeltaLovelace: 19_200_000,
    queuePeak: 50,
    queueFinal: 2,
    mempoolPeak: 120,
    mempoolFinal: 10,
    ...overrides,
  };

  return {
    counterDeltas: [
      makeCounterDelta('tx_submissions_enqueued_total', o.enqueuedDelta),
      makeCounterDelta('tx_submissions_rejected_total', o.rejectedDelta),
      makeCounterDelta('tx_submissions_mempool_accepted_total', o.mempoolAcceptedDelta),
      makeCounterDelta('tx_submissions_processing_failed_total', o.processingFailedDelta),
      makeCounterDelta('commit_block_tx_count_total', o.committedTxDelta),
      makeCounterDelta('commit_block_count_total', o.committedBlockDelta),
      makeCounterDelta('submit_block_count_total', o.submittedBlockDelta),
      makeCounterDelta('merge_block_failures_total', o.mergeFailureDelta),
      makeCounterDelta('commit_block_commitment_failures_total', o.commitmentFailureDelta),
      makeCounterDelta('l1_commitment_fees_lovelace_total', o.l1CommitmentFeesDeltaLovelace),
    ],
    gaugeSummaries: [
      makeGaugeSummary('tx_stream_depth', o.queuePeak, o.queueFinal),
      makeGaugeSummary('mempool_tx_count', o.mempoolPeak, o.mempoolFinal),
    ],
  };
}

function makeInput(overrides: Partial<TierSummaryInput> = {}): TierSummaryInput {
  return {
    tierIndex: 0,
    targetTps: 10,
    startedAt: STARTED_AT,
    loadStoppedAt: STOPPED_AT,
    stoppedAt: STOPPED_AT,
    metricWindow: null,
    windowSummary: makeWindowSummary(),
    submissionAggregate: null,
    loadDriverResourceEvidence: null,
    collapse: null,
    evidenceIncomplete: false,
    ...overrides,
  };
}

function makeResourceEvidence(): LoadDriverResourceEvidence {
  return {
    beforeLoad: {
      capturedAt: STARTED_AT,
      capturedAtMs: new Date(STARTED_AT).getTime(),
      systemCpuTotalMs: 10_000,
      systemCpuIdleMs: 7_000,
      processCpuUserMicros: 10_000,
      processCpuSystemMicros: 2_000,
      processCpuTotalMicros: 12_000,
      processRssBytes: 100_000_000,
      processHeapUsedBytes: 40_000_000,
      processHeapTotalBytes: 70_000_000,
      systemTotalMemoryBytes: 1_000_000_000,
      systemFreeMemoryBytes: 600_000_000,
      eventLoopLagP95Ms: 2,
      eventLoopLagMaxMs: 5,
      eventLoopLagMeanMs: 2.5,
      networkRxBytes: 1_000_000,
      networkTxBytes: 2_000_000,
    },
    afterLoad: {
      capturedAt: STOPPED_AT,
      capturedAtMs: new Date(STOPPED_AT).getTime(),
      systemCpuTotalMs: 15_000,
      systemCpuIdleMs: 9_000,
      processCpuUserMicros: 80_000,
      processCpuSystemMicros: 20_000,
      processCpuTotalMicros: 100_000,
      processRssBytes: 820_000_000,
      processHeapUsedBytes: 300_000_000,
      processHeapTotalBytes: 360_000_000,
      systemTotalMemoryBytes: 1_000_000_000,
      systemFreeMemoryBytes: 100_000_000,
      eventLoopLagP95Ms: 120,
      eventLoopLagMaxMs: 300,
      eventLoopLagMeanMs: 70,
      networkRxBytes: 40_000_000,
      networkTxBytes: 50_000_000,
    },
    afterRecovery: {
      capturedAt: STOPPED_AT,
      capturedAtMs: new Date(STOPPED_AT).getTime() + 30_000,
      systemCpuTotalMs: 17_000,
      systemCpuIdleMs: 10_500,
      processCpuUserMicros: 100_000,
      processCpuSystemMicros: 30_000,
      processCpuTotalMicros: 130_000,
      processRssBytes: 780_000_000,
      processHeapUsedBytes: 250_000_000,
      processHeapTotalBytes: 360_000_000,
      systemTotalMemoryBytes: 1_000_000_000,
      systemFreeMemoryBytes: 160_000_000,
      eventLoopLagP95Ms: 80,
      eventLoopLagMaxMs: 200,
      eventLoopLagMeanMs: 40,
      networkRxBytes: 60_000_000,
      networkTxBytes: 70_000_000,
    },
    loadPhase: {
      elapsedMs: 60_000,
      hostCpuUtilizationPercent: 92,
      processCpuPercent: 12,
      processRssBytes: 820_000_000,
      processRssDeltaBytes: 720_000_000,
      processRssToSystemMemoryRatio: 0.82,
      eventLoopLagP95Ms: 120,
      eventLoopLagMaxMs: 300,
      eventLoopLagMeanMs: 70,
      networkRxDeltaBytes: 39_000_000,
      networkTxDeltaBytes: 48_000_000,
      networkRxBytesPerSec: 650_000,
      networkTxBytesPerSec: 800_000,
      networkTotalBytesPerSec: 1_450_000,
    },
    recoveryPhase: {
      elapsedMs: 30_000,
      hostCpuUtilizationPercent: 60,
      processCpuPercent: 5,
      processRssBytes: 780_000_000,
      processRssDeltaBytes: -40_000_000,
      processRssToSystemMemoryRatio: 0.78,
      eventLoopLagP95Ms: 80,
      eventLoopLagMaxMs: 200,
      eventLoopLagMeanMs: 40,
      networkRxDeltaBytes: 20_000_000,
      networkTxDeltaBytes: 20_000_000,
      networkRxBytesPerSec: 666_667,
      networkTxBytesPerSec: 666_667,
      networkTotalBytesPerSec: 1_333_334,
    },
    totalPhase: {
      elapsedMs: 90_000,
      hostCpuUtilizationPercent: 84,
      processCpuPercent: 8,
      processRssBytes: 780_000_000,
      processRssDeltaBytes: 680_000_000,
      processRssToSystemMemoryRatio: 0.78,
      eventLoopLagP95Ms: 110,
      eventLoopLagMaxMs: 300,
      eventLoopLagMeanMs: 60,
      networkRxDeltaBytes: 59_000_000,
      networkTxDeltaBytes: 68_000_000,
      networkRxBytesPerSec: 655_556,
      networkTxBytesPerSec: 755_556,
      networkTotalBytesPerSec: 1_411_112,
    },
    saturationFlags: {
      cpuSaturated: true,
      memorySaturated: true,
      eventLoopLagSaturated: true,
      networkIoSaturated: false,
      anySaturation: true,
      reasons: ['cpu saturated', 'memory saturated', 'event loop lag saturated'],
      thresholds: {
        cpuPercent: 90,
        memoryRssToSystemRatio: 0.8,
        eventLoopLagP95Ms: 100,
        networkTotalBytesPerSec: 100_000_000,
      },
    },
  };
}

function makeSubmissionAggregate(): SubmissionAggregate {
  return {
    counters: {
      generated: 600,
      attempted: 543,
      submitted: 540,
      rejected: 60,
      node_unavailable: 0,
      timed_out: 1,
      error: 3,
    },
    retries: {
      totalRetries: 12,
      submissionsRetried: 5,
      maxRetryCount: 3,
    },
    latencyMs: {
      submitted: {
        boundsMs: [50, 100],
        counts: [100, 440],
        overflowCount: 0,
        count: 540,
        sumMs: 32_400,
        minMs: 10,
        maxMs: 100,
      },
      rejected: {
        boundsMs: [50, 100],
        counts: [0, 0],
        overflowCount: 0,
        count: 0,
        sumMs: 0,
        minMs: null,
        maxMs: null,
      },
      node_unavailable: {
        boundsMs: [50, 100],
        counts: [0, 0],
        overflowCount: 0,
        count: 0,
        sumMs: 0,
        minMs: null,
        maxMs: null,
      },
      timed_out: {
        boundsMs: [50, 100],
        counts: [0, 1],
        overflowCount: 0,
        count: 1,
        sumMs: 100,
        minMs: 100,
        maxMs: 100,
      },
      error: {
        boundsMs: [50, 100],
        counts: [1, 2],
        overflowCount: 0,
        count: 3,
        sumMs: 240,
        minMs: 50,
        maxMs: 100,
      },
    },
    percentilesMs: {
      submitted: { p50: 100, p95: 100, p99: 100 },
      rejected: { p50: null, p95: null, p99: null },
      node_unavailable: { p50: null, p95: null, p99: null },
      timed_out: { p50: 100, p95: 100, p99: 100 },
      error: { p50: 100, p95: 100, p99: 100 },
    },
  };
}

const COLLAPSE_NODE_UNAVAILABLE: CollapseResult = {
  reason: 'node_unavailable',
  values: { consecutiveProbeFailures: 3 },
};

// ---------------------------------------------------------------------------
// Normal tier
// ---------------------------------------------------------------------------

describe('buildTierSummary — normal tier', () => {
  it('sets result to completed when no collapse and no evidence gaps', () => {
    const s = buildTierSummary(makeInput());
    expect(s.result).toBe('completed');
  });

  it('does not include collapseReason on a completed tier', () => {
    const s = buildTierSummary(makeInput());
    expect(s.collapseReason).toBeUndefined();
  });

  it('carries through tier metadata fields', () => {
    const s = buildTierSummary(makeInput({ tierIndex: 2, targetTps: 400 }));
    expect(s.tierIndex).toBe(2);
    expect(s.targetTps).toBe(400);
    expect(s.startedAt).toBe(STARTED_AT);
    expect(s.stoppedAt).toBe(STOPPED_AT);
  });

  it('computes durationSeconds (full window) and loadDurationSeconds (load phase) from ISO timestamps', () => {
    const s = buildTierSummary(makeInput());
    // Default fixture has loadStoppedAt == stoppedAt == 60s after start
    expect(s.durationSeconds).toBe(60);
    expect(s.loadDurationSeconds).toBe(60);
  });

  it('uses load phase duration, not full tier window, for TPS computation', () => {
    // load = 60s, recovery = 30s → total = 90s
    const RECOVERY_STOPPED_AT = '2025-01-01T00:01:30.000Z';
    const s = buildTierSummary(
      makeInput({
        loadStoppedAt: STOPPED_AT,
        stoppedAt: RECOVERY_STOPPED_AT,
        windowSummary: makeWindowSummary({
          enqueuedDelta: 600,
          mempoolAcceptedDelta: 540,
          committedTxDelta: 480,
        }),
      })
    );
    expect(s.durationSeconds).toBe(90);
    expect(s.loadDurationSeconds).toBe(60);
    // TPS must use 60s (load), not 90s (full window)
    expect(s.observedEnqueuedTps).toBeCloseTo(600 / 60);
    expect(s.observedMempoolAcceptedTps).toBeCloseTo(540 / 60);
    expect(s.observedCommittedTps).toBeCloseTo(480 / 60);
  });

  it('extracts enqueuedDelta from tx_submissions_enqueued_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ enqueuedDelta: 600 }) })
    );
    expect(s.enqueuedDelta).toBe(600);
  });

  it('extracts rejectedDelta from tx_submissions_rejected_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ rejectedDelta: 6 }) })
    );
    expect(s.rejectedDelta).toBe(6);
  });

  it('extracts mempoolAcceptedDelta from tx_submissions_mempool_accepted_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolAcceptedDelta: 540 }) })
    );
    expect(s.mempoolAcceptedDelta).toBe(540);
  });

  it('extracts processingFailedDelta from tx_submissions_processing_failed_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ processingFailedDelta: 3 }) })
    );
    expect(s.processingFailedDelta).toBe(3);
  });

  it('extracts committedTxDelta from commit_block_tx_count_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ committedTxDelta: 480 }) })
    );
    expect(s.committedTxDelta).toBe(480);
  });

  it('extracts committedBlockDelta from commit_block_count_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ committedBlockDelta: 8 }) })
    );
    expect(s.committedBlockDelta).toBe(8);
  });

  it('extracts submittedBlockDelta from submit_block_count_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ submittedBlockDelta: 8 }) })
    );
    expect(s.submittedBlockDelta).toBe(8);
  });

  it('falls back to afterLoad-baseline(0) when before sample is missing for merged block counter', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: {
          counterDeltas: [
            { query: 'merge_block_count_total', deltaLoad: null, deltaRecovery: null },
          ],
          gaugeSummaries: [],
        },
        metricWindow: {
          tierIndex: 0,
          targetTps: 10,
          startedAt: STARTED_AT,
          stoppedAt: STOPPED_AT,
          recoveryStartedAt: STOPPED_AT,
          recoveryStoppedAt: STOPPED_AT,
          before: { merge_block_count_total: null },
          afterLoad: { merge_block_count_total: 4 },
          afterRecovery: {},
          ranges: {},
        },
      })
    );
    expect(s.mergedBlockDelta).toBe(4);
  });

  it('extracts mergeFailureDelta from merge_block_failures_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mergeFailureDelta: 2 }) })
    );
    expect(s.mergeFailureDelta).toBe(2);
  });

  it('extracts commitmentFailureDelta from commit_block_commitment_failures_total', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ commitmentFailureDelta: 1 }) })
    );
    expect(s.commitmentFailureDelta).toBe(1);
  });

  it('extracts l1CommitmentFeesDeltaLovelace from l1_commitment_fees_lovelace_total', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({
          l1CommitmentFeesDeltaLovelace: 25_000_000,
        }),
      })
    );
    expect(s.l1CommitmentFeesDeltaLovelace).toBe(25_000_000);
  });

  it('extracts l1CommitmentFeeLastLovelace from afterLoad instant metrics', () => {
    const s = buildTierSummary(
      makeInput({
        metricWindow: {
          tierIndex: 0,
          targetTps: 10,
          startedAt: STARTED_AT,
          stoppedAt: STOPPED_AT,
          recoveryStartedAt: STOPPED_AT,
          recoveryStoppedAt: STOPPED_AT,
          before: {},
          afterLoad: { l1_commitment_fee_lovelace_last: 2_400_000 },
          afterRecovery: {},
          ranges: {},
        },
      })
    );
    expect(s.l1CommitmentFeeLastLovelace).toBe(2_400_000);
  });

  it('computes l1FeePerCommittedL2TxLovelace from fee delta and committed tx delta', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({
          l1CommitmentFeesDeltaLovelace: 24_000_000,
          committedTxDelta: 480,
        }),
      })
    );
    expect(s.l1FeePerCommittedL2TxLovelace).toBe(50_000);
  });

  it('computes observedEnqueuedTps as enqueuedDelta / durationSeconds', () => {
    // 600 enqueued over 60 s = 10 TPS
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ enqueuedDelta: 600 }) })
    );
    expect(s.observedEnqueuedTps).toBeCloseTo(10);
  });

  it('computes observedMempoolAcceptedTps as mempoolAcceptedDelta / durationSeconds', () => {
    // 540 accepted over 60 s = 9 TPS
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolAcceptedDelta: 540 }) })
    );
    expect(s.observedMempoolAcceptedTps).toBeCloseTo(9);
  });

  it('computes observedCommittedTps as committedTxDelta / durationSeconds', () => {
    // 480 committed over 60 s = 8 TPS
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ committedTxDelta: 480 }) })
    );
    expect(s.observedCommittedTps).toBeCloseTo(8);
  });

  it('mempoolAcceptedTps is independent of enqueuedTps — does not conflate queue with durable', () => {
    // enqueuedDelta is 1000 (HTTP boundary) but mempoolAcceptedDelta is 400 (durable)
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({ enqueuedDelta: 1000, mempoolAcceptedDelta: 400 }),
      })
    );
    expect(s.observedEnqueuedTps).toBeCloseTo(1000 / 60);
    expect(s.observedMempoolAcceptedTps).toBeCloseTo(400 / 60);
    expect(s.observedEnqueuedTps).not.toBeCloseTo(s.observedMempoolAcceptedTps as number, 2);
  });

  it('extracts peakQueueSize from tx_stream_depth gauge peak', () => {
    const s = buildTierSummary(makeInput({ windowSummary: makeWindowSummary({ queuePeak: 50 }) }));
    expect(s.peakQueueSize).toBe(50);
  });

  it('extracts finalQueueSizeAfterRecovery from tx_stream_depth gauge final', () => {
    const s = buildTierSummary(makeInput({ windowSummary: makeWindowSummary({ queueFinal: 2 }) }));
    expect(s.finalQueueSizeAfterRecovery).toBe(2);
  });

  it('derives finalQueueDeltaAfterRecovery from metricWindow (afterRecovery - before)', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({ queueFinal: 15 }),
        metricWindow: {
          tierIndex: 0,
          targetTps: 10,
          startedAt: STARTED_AT,
          stoppedAt: STOPPED_AT,
          recoveryStartedAt: STOPPED_AT,
          recoveryStoppedAt: STOPPED_AT,
          before: { tx_stream_depth: 4 },
          afterLoad: {},
          afterRecovery: { tx_stream_depth: 15 },
          ranges: {},
        },
      })
    );
    expect(s.finalQueueDeltaAfterRecovery).toBe(11);
  });

  it('extracts peakMempoolSize from mempool_tx_count gauge peak', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolPeak: 120 }) })
    );
    expect(s.peakMempoolSize).toBe(120);
  });

  it('extracts finalMempoolSizeAfterRecovery from mempool_tx_count gauge final', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolFinal: 10 }) })
    );
    expect(s.finalMempoolSizeAfterRecovery).toBe(10);
  });

  it('derives finalMempoolDeltaAfterRecovery from metricWindow (afterRecovery - before)', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({ mempoolFinal: 12 }),
        metricWindow: {
          tierIndex: 0,
          targetTps: 10,
          startedAt: STARTED_AT,
          stoppedAt: STOPPED_AT,
          recoveryStartedAt: STOPPED_AT,
          recoveryStoppedAt: STOPPED_AT,
          before: { mempool_tx_count: 3 },
          afterLoad: {},
          afterRecovery: { mempool_tx_count: 12 },
          ranges: {},
        },
      })
    );
    expect(s.finalMempoolDeltaAfterRecovery).toBe(9);
  });

  it('maps client submission aggregate counters and retry evidence', () => {
    const s = buildTierSummary(
      makeInput({
        submissionAggregate: makeSubmissionAggregate(),
      })
    );
    expect(s.clientSubmittedCount).toBe(540);
    expect(s.clientRejectedCount).toBe(60);
    expect(s.clientNodeUnavailableCount).toBe(0);
    expect(s.clientErrorCount).toBe(3);
    expect(s.clientTotalRetries).toBe(12);
    expect(s.clientRetriedSubmissionCount).toBe(5);
    expect(s.clientSubmittedLatencyP95Ms).toBe(100);
  });

  it('includes load-driver resource evidence when provided', () => {
    const evidence = makeResourceEvidence();
    const s = buildTierSummary(makeInput({ loadDriverResourceEvidence: evidence }));
    expect(s.loadDriverResourceEvidence).toEqual(evidence);
  });

  it('includes load-driver saturation flags derived from resource evidence', () => {
    const evidence = makeResourceEvidence();
    const s = buildTierSummary(makeInput({ loadDriverResourceEvidence: evidence }));
    expect(s.loadDriverSaturationFlags).toEqual(evidence.saturationFlags);
    expect(s.loadDriverSaturationFlags?.anySaturation).toBe(true);
  });

  it('includes accepted-to-committed latency method and confidence fields', () => {
    const s = buildTierSummary(makeInput());
    expect(s.acceptedToCommittedLatencyMethod).toBe('cohort_counter_alignment_v1');
    expect(s.acceptedToCommittedLatencyConfidence).toBe('insufficient_data');
    expect(s.acceptedToCommittedLatencyP95Ms).toBeNull();
    expect(s.acceptedToCommittedLatencyConfidenceNotes.length).toBeGreaterThan(0);
  });
});

// ---------------------------------------------------------------------------
// Missing metrics
// ---------------------------------------------------------------------------

describe('buildTierSummary — missing metrics', () => {
  it('sets result to evidence_incomplete when evidenceIncomplete is true and no collapse', () => {
    const s = buildTierSummary(makeInput({ evidenceIncomplete: true }));
    expect(s.result).toBe('evidence_incomplete');
  });

  it('returns null for enqueuedDelta when metric is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ enqueuedDelta: null }) })
    );
    expect(s.enqueuedDelta).toBeNull();
  });

  it('returns null for mempoolAcceptedDelta when metric is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolAcceptedDelta: null }) })
    );
    expect(s.mempoolAcceptedDelta).toBeNull();
  });

  it('returns null for committedTxDelta when metric is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ committedTxDelta: null }) })
    );
    expect(s.committedTxDelta).toBeNull();
  });

  it('returns null for l1CommitmentFeesDeltaLovelace when metric is missing', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({
          l1CommitmentFeesDeltaLovelace: null,
        }),
      })
    );
    expect(s.l1CommitmentFeesDeltaLovelace).toBeNull();
  });

  it('returns null for l1FeePerCommittedL2TxLovelace when committedTxDelta is zero', () => {
    const s = buildTierSummary(
      makeInput({
        windowSummary: makeWindowSummary({
          l1CommitmentFeesDeltaLovelace: 24_000_000,
          committedTxDelta: 0,
        }),
      })
    );
    expect(s.l1FeePerCommittedL2TxLovelace).toBeNull();
  });

  it('returns null for observedEnqueuedTps when enqueuedDelta is null', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ enqueuedDelta: null }) })
    );
    expect(s.observedEnqueuedTps).toBeNull();
  });

  it('returns null for observedMempoolAcceptedTps when mempoolAcceptedDelta is null', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolAcceptedDelta: null }) })
    );
    expect(s.observedMempoolAcceptedTps).toBeNull();
  });

  it('returns null for observedCommittedTps when committedTxDelta is null', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ committedTxDelta: null }) })
    );
    expect(s.observedCommittedTps).toBeNull();
  });

  it('returns null for peakQueueSize when gauge data is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ queuePeak: null }) })
    );
    expect(s.peakQueueSize).toBeNull();
  });

  it('returns null for finalQueueSizeAfterRecovery when gauge data is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ queueFinal: null }) })
    );
    expect(s.finalQueueSizeAfterRecovery).toBeNull();
  });

  it('returns null for peakMempoolSize when gauge data is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolPeak: null }) })
    );
    expect(s.peakMempoolSize).toBeNull();
  });

  it('returns null for finalMempoolSizeAfterRecovery when gauge data is missing', () => {
    const s = buildTierSummary(
      makeInput({ windowSummary: makeWindowSummary({ mempoolFinal: null }) })
    );
    expect(s.finalMempoolSizeAfterRecovery).toBeNull();
    expect(s.clientSubmittedCount).toBeNull();
    expect(s.clientRejectedCount).toBeNull();
    expect(s.clientNodeUnavailableCount).toBeNull();
    expect(s.clientErrorCount).toBeNull();
    expect(s.clientTotalRetries).toBeNull();
    expect(s.clientRetriedSubmissionCount).toBeNull();
    expect(s.clientSubmittedLatencyP95Ms).toBeNull();
  });

  it('returns all null metric fields when windowSummary is null', () => {
    const s = buildTierSummary(makeInput({ windowSummary: null }));
    expect(s.enqueuedDelta).toBeNull();
    expect(s.rejectedDelta).toBeNull();
    expect(s.mempoolAcceptedDelta).toBeNull();
    expect(s.processingFailedDelta).toBeNull();
    expect(s.committedTxDelta).toBeNull();
    expect(s.committedBlockDelta).toBeNull();
    expect(s.submittedBlockDelta).toBeNull();
    expect(s.mergeFailureDelta).toBeNull();
    expect(s.commitmentFailureDelta).toBeNull();
    expect(s.l1CommitmentFeesDeltaLovelace).toBeNull();
    expect(s.l1CommitmentFeeLastLovelace).toBeNull();
    expect(s.l1FeePerCommittedL2TxLovelace).toBeNull();
    expect(s.observedEnqueuedTps).toBeNull();
    expect(s.observedMempoolAcceptedTps).toBeNull();
    expect(s.observedCommittedTps).toBeNull();
    expect(s.peakQueueSize).toBeNull();
    expect(s.finalQueueSizeAfterRecovery).toBeNull();
    expect(s.peakMempoolSize).toBeNull();
    expect(s.finalMempoolSizeAfterRecovery).toBeNull();
    expect(s.acceptedToCommittedLatencyP50Ms).toBeNull();
    expect(s.acceptedToCommittedLatencyP95Ms).toBeNull();
    expect(s.acceptedToCommittedLatencyP99Ms).toBeNull();
  });

  it('does not trigger division by zero when load duration is zero', () => {
    const s = buildTierSummary(
      makeInput({ startedAt: STARTED_AT, loadStoppedAt: STARTED_AT, stoppedAt: STARTED_AT })
    );
    expect(s.loadDurationSeconds).toBe(0);
    expect(s.observedEnqueuedTps).toBeNull();
    expect(s.observedMempoolAcceptedTps).toBeNull();
    expect(s.observedCommittedTps).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Collapsed tier
// ---------------------------------------------------------------------------

describe('buildTierSummary — collapsed tier', () => {
  it('sets result to collapsed when collapse is provided', () => {
    const s = buildTierSummary(makeInput({ collapse: COLLAPSE_NODE_UNAVAILABLE }));
    expect(s.result).toBe('collapsed');
  });

  it('sets collapseReason from the CollapseResult reason', () => {
    const s = buildTierSummary(makeInput({ collapse: COLLAPSE_NODE_UNAVAILABLE }));
    expect(s.collapseReason).toBe('node_unavailable');
  });

  it('sets collapseReason for each collapse reason variant', () => {
    const reasons = [
      'node_unavailable',
      'prometheus_down',
      'commitment_failures',
      'merge_failures',
      'queue_not_recovered',
      'mempool_not_recovered',
      'unsubmitted_backlog_growth',
      'tx_generator_failed',
      'useful_throughput_below_threshold',
    ] as const;

    for (const reason of reasons) {
      const s = buildTierSummary(makeInput({ collapse: { reason, values: {} } }));
      expect(s.result).toBe('collapsed');
      expect(s.collapseReason).toBe(reason);
    }
  });

  it('collapse takes priority over evidence_incomplete', () => {
    const s = buildTierSummary(
      makeInput({ collapse: COLLAPSE_NODE_UNAVAILABLE, evidenceIncomplete: true })
    );
    expect(s.result).toBe('collapsed');
  });

  it('still extracts metric deltas when collapse is set and metrics are available', () => {
    const s = buildTierSummary(
      makeInput({
        collapse: COLLAPSE_NODE_UNAVAILABLE,
        windowSummary: makeWindowSummary({ enqueuedDelta: 300, mempoolAcceptedDelta: 200 }),
      })
    );
    expect(s.enqueuedDelta).toBe(300);
    expect(s.mempoolAcceptedDelta).toBe(200);
  });

  it('metrics are null when windowSummary is null even on a collapsed tier', () => {
    const s = buildTierSummary(
      makeInput({ collapse: COLLAPSE_NODE_UNAVAILABLE, windowSummary: null })
    );
    expect(s.enqueuedDelta).toBeNull();
    expect(s.mempoolAcceptedDelta).toBeNull();
    expect(s.committedTxDelta).toBeNull();
  });
});
