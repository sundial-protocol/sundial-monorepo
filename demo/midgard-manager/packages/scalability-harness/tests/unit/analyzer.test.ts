import { describe, expect, it } from 'vitest';

import type { BenchmarkConclusion } from '../../src/analysis/analyzer.js';
import { analyzeTiers } from '../../src/analysis/analyzer.js';
import type { TierSummary } from '../../src/analysis/tier-summary.js';

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

function makeTier(overrides: Partial<TierSummary> = {}): TierSummary {
  return {
    tierIndex: 0,
    targetTps: 100,
    startedAt: '2025-01-01T00:00:00.000Z',
    stoppedAt: '2025-01-01T00:01:00.000Z',
    durationSeconds: 60,
    result: 'completed',
    collapseReason: undefined,
    enqueuedDelta: 6000,
    rejectedDelta: 0,
    mempoolAcceptedDelta: 5800,
    processingFailedDelta: 0,
    committedTxDelta: 5600,
    committedBlockDelta: 10,
    submittedBlockDelta: 10,
    mergeFailureDelta: 0,
    commitmentFailureDelta: 0,
    l1CommitmentFeesDeltaLovelace: 24_000_000,
    l1CommitmentFeeLastLovelace: 2_400_000,
    l1FeePerCommittedL2TxLovelace: 4_285.71,
    observedEnqueuedTps: 100,
    observedMempoolAcceptedTps: 96.67,
    observedCommittedTps: 93.33,
    peakQueueSize: 50,
    finalQueueSizeAfterRecovery: 0,
    peakMempoolSize: 200,
    finalMempoolSizeAfterRecovery: 0,
    clientSubmittedCount: 5800,
    clientRejectedCount: 50,
    clientNodeUnavailableCount: 0,
    clientErrorCount: 0,
    clientTotalRetries: 10,
    clientRetriedSubmissionCount: 8,
    clientSubmittedLatencyP95Ms: 250,
    acceptedToCommittedLatencyMethod: 'cohort_counter_alignment_v1',
    acceptedToCommittedLatencyConfidence: 'medium',
    acceptedToCommittedLatencyConfidenceNotes: [],
    acceptedToCommittedLatencyP50Ms: 30_000,
    acceptedToCommittedLatencyP95Ms: 45_000,
    acceptedToCommittedLatencyP99Ms: 45_000,
    acceptedToCommittedResolvedRatio: 0.95,
    acceptedToCommittedResolvedTxCount: 5_320,
    acceptedToCommittedAcceptedTxCount: 5_600,
    loadDriverResourceEvidence: null,
    loadDriverSaturationFlags: null,
    ...overrides,
  };
}

function makeCollapsed(
  tierIndex: number,
  targetTps: number,
  collapseReason: string,
  metricOverrides: Partial<TierSummary> = {}
): TierSummary {
  return makeTier({
    tierIndex,
    targetTps,
    result: 'collapsed',
    collapseReason,
    ...metricOverrides,
  });
}

// ---------------------------------------------------------------------------
// Empty input
// ---------------------------------------------------------------------------

describe('analyzeTiers — empty input', () => {
  it('returns null for all tier fields', () => {
    const c = analyzeTiers([]);
    expect(c.highestCompletedTier).toBeNull();
    expect(c.highestCompletedTargetTps).toBeNull();
    expect(c.firstCollapsedTier).toBeNull();
    expect(c.firstCollapsedTargetTps).toBeNull();
  });

  it('returns "no data" as primaryBottleneck', () => {
    const c = analyzeTiers([]);
    expect(c.primaryBottleneck).toBe('no data');
  });

  it('includes a note that no tiers were executed', () => {
    const c = analyzeTiers([]);
    expect(c.notes.some((n) => n.includes('No tiers executed'))).toBe(true);
  });

  it('classifies empty runs as Blocked', () => {
    const c = analyzeTiers([]);
    expect(c.classification).toBe('Blocked');
  });
});

// ---------------------------------------------------------------------------
// All completed — no collapse
// ---------------------------------------------------------------------------

describe('analyzeTiers — all completed', () => {
  it('returns the highest tierIndex as highestCompletedTier', () => {
    const tiers = [
      makeTier({ tierIndex: 0, targetTps: 100 }),
      makeTier({ tierIndex: 1, targetTps: 200 }),
      makeTier({ tierIndex: 2, targetTps: 400 }),
    ];
    const c = analyzeTiers(tiers);
    expect(c.highestCompletedTier).toBe(2);
    expect(c.highestCompletedTargetTps).toBe(400);
  });

  it('returns null for firstCollapsedTier', () => {
    const c = analyzeTiers([makeTier({ tierIndex: 0 })]);
    expect(c.firstCollapsedTier).toBeNull();
    expect(c.firstCollapsedTargetTps).toBeNull();
  });

  it('returns "none detected" as primaryBottleneck', () => {
    const c = analyzeTiers([makeTier()]);
    expect(c.primaryBottleneck).toBe('none detected');
  });

  it('includes a note that no collapse was detected', () => {
    const c = analyzeTiers([makeTier()]);
    expect(c.notes.some((n) => n.toLowerCase().includes('no collapse'))).toBe(true);
  });

  it('classifies completed runs as Passed when no criteria are violated', () => {
    const c = analyzeTiers([makeTier()]);
    expect(c.classification).toBe('Passed');
  });
});

// ---------------------------------------------------------------------------
// First collapsed tier identification
// ---------------------------------------------------------------------------

describe('analyzeTiers — firstCollapsedTier', () => {
  it('picks the lowest tierIndex among collapsed tiers', () => {
    const tiers = [
      makeTier({ tierIndex: 0, result: 'completed' }),
      makeCollapsed(1, 200, 'node_unavailable'),
      makeCollapsed(2, 400, 'node_unavailable'),
    ];
    const c = analyzeTiers(tiers);
    expect(c.firstCollapsedTier).toBe(1);
    expect(c.firstCollapsedTargetTps).toBe(200);
  });

  it('returns correct targetTps for the first collapsed tier', () => {
    const tiers = [
      makeCollapsed(3, 800, 'commitment_failures'),
      makeCollapsed(1, 200, 'commitment_failures'),
      makeCollapsed(2, 400, 'commitment_failures'),
    ];
    const c = analyzeTiers(tiers);
    expect(c.firstCollapsedTier).toBe(1);
    expect(c.firstCollapsedTargetTps).toBe(200);
  });

  it('highestCompletedTier is null when no tier completed', () => {
    const tiers = [makeCollapsed(0, 100, 'node_unavailable')];
    const c = analyzeTiers(tiers);
    expect(c.highestCompletedTier).toBeNull();
    expect(c.highestCompletedTargetTps).toBeNull();
  });

  it('highestCompletedTier reflects completed tiers even when some collapsed', () => {
    const tiers = [
      makeTier({ tierIndex: 0, targetTps: 100 }),
      makeTier({ tierIndex: 1, targetTps: 200 }),
      makeCollapsed(2, 400, 'node_unavailable'),
    ];
    const c = analyzeTiers(tiers);
    expect(c.highestCompletedTier).toBe(1);
    expect(c.highestCompletedTargetTps).toBe(200);
  });
});

// ---------------------------------------------------------------------------
// Direct collapse-reason bottleneck mapping
// ---------------------------------------------------------------------------

describe('analyzeTiers — direct bottleneck from collapse reason', () => {
  it('maps node_unavailable to API/runtime availability', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'node_unavailable')]);
    expect(c.primaryBottleneck).toContain('API/runtime availability');
    expect(c.primaryBottleneck).toContain('node probe');
  });

  it('maps prometheus_down to runtime or observability availability', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'prometheus_down')]);
    expect(c.primaryBottleneck).toContain('observability');
    expect(c.primaryBottleneck).toContain('Prometheus');
  });

  it('maps commitment_failures to block commitment', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'commitment_failures')]);
    expect(c.primaryBottleneck).toContain('block commitment');
  });

  it('maps merge_failures to merge path', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'merge_failures')]);
    expect(c.primaryBottleneck).toContain('merge path');
  });

  it('maps queue_not_recovered to queue processor or transaction parsing', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'queue_not_recovered')]);
    expect(c.primaryBottleneck).toContain('queue processor or transaction parsing');
  });

  it('maps mempool_not_recovered to block commitment', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'mempool_not_recovered')]);
    expect(c.primaryBottleneck).toContain('block commitment');
  });

  it('maps tx_generator_failed to load generation', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'tx_generator_failed')]);
    expect(c.primaryBottleneck).toContain('load generation');
  });

  it('does not add a heuristic note for direct collapse-reason mappings', () => {
    const directReasons = [
      'node_unavailable',
      'prometheus_down',
      'commitment_failures',
      'merge_failures',
      'queue_not_recovered',
      'mempool_not_recovered',
      'tx_generator_failed',
    ] as const;

    for (const reason of directReasons) {
      const c = analyzeTiers([makeCollapsed(0, 100, reason)]);
      const hasHeuristicNote = c.notes.some((n) => n.toLowerCase().includes('heuristic'));
      expect(hasHeuristicNote, `${reason} should not produce a heuristic note`).toBe(false);
    }
  });
});

// ---------------------------------------------------------------------------
// Metric gap heuristics (useful_throughput_below_threshold)
// ---------------------------------------------------------------------------

describe('analyzeTiers — metric gap heuristics', () => {
  it('identifies queue processor bottleneck: enqueued > 0 but mempool accepted is 0', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('queue processor or transaction parsing');
  });

  it('identifies queue processor bottleneck: enqueued > 0 but mempool accepted is null', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: null,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('queue processor or transaction parsing');
  });

  it('identifies block commitment bottleneck: mempool accepted > 0 but committed tx is 0', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('block commitment');
  });

  it('identifies block commitment bottleneck: mempool accepted > 0 but committed tx is null', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: null,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('block commitment');
  });

  it('identifies L1 submission bottleneck: committed blocks > 0 but submitted blocks is 0', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 4000,
      committedBlockDelta: 8,
      submittedBlockDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('L1 submission');
  });

  it('identifies L1 submission bottleneck: committed blocks > 0 but submitted blocks is null', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 4000,
      committedBlockDelta: 8,
      submittedBlockDelta: null,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('L1 submission');
  });

  it('identifies merge path bottleneck: submitted blocks > 0 and merge failures > 0', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 4000,
      committedBlockDelta: 8,
      submittedBlockDelta: 8,
      mergeFailureDelta: 3,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('merge path');
  });

  it('returns undetermined when no metric gap matches', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 0,
      mempoolAcceptedDelta: 0,
      committedTxDelta: 0,
      committedBlockDelta: 0,
      submittedBlockDelta: 0,
      mergeFailureDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toBe('undetermined');
  });

  it('adds a heuristic note for useful_throughput_below_threshold collapse reason', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.notes.some((n) => n.toLowerCase().includes('heuristic'))).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Pipeline stage priority ordering
// ---------------------------------------------------------------------------

describe('analyzeTiers — metric gap priority order', () => {
  it('reports queue bottleneck over block commitment when enqueued gap appears first', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 0, // gap at stage 1
      committedTxDelta: 0, // also a gap at stage 2 — stage 1 should win
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('queue processor or transaction parsing');
  });

  it('reports block commitment over L1 submission when mempool gap appears first', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 0, // gap at stage 2
      committedBlockDelta: 0, // also gap at stage 3 — stage 2 should win
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('block commitment');
  });

  it('reports L1 submission over merge path when committed-block gap appears first', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 5000,
      committedTxDelta: 4000,
      committedBlockDelta: 8,
      submittedBlockDelta: 0, // gap at stage 3
      mergeFailureDelta: 3, // also a signal at stage 4 — stage 3 should win
    });
    const c = analyzeTiers([tier]);
    expect(c.primaryBottleneck).toContain('L1 submission');
  });
});

// ---------------------------------------------------------------------------
// Evidence incomplete tiers
// ---------------------------------------------------------------------------

describe('analyzeTiers — evidence_incomplete tiers', () => {
  it('adds a note when evidence_incomplete tiers exist', () => {
    const tiers = [
      makeTier({ tierIndex: 0, result: 'evidence_incomplete' }),
      makeTier({ tierIndex: 1, result: 'completed' }),
    ];
    const c = analyzeTiers(tiers);
    expect(c.notes.some((n) => n.includes('incomplete metric evidence'))).toBe(true);
  });

  it('does not count evidence_incomplete tiers as completed', () => {
    const tiers = [
      makeTier({ tierIndex: 0, result: 'evidence_incomplete', targetTps: 100 }),
      makeTier({ tierIndex: 1, result: 'completed', targetTps: 200 }),
    ];
    const c = analyzeTiers(tiers);
    expect(c.highestCompletedTier).toBe(1);
  });

  it('does not count evidence_incomplete tiers as collapsed', () => {
    const tiers = [makeTier({ tierIndex: 0, result: 'evidence_incomplete' })];
    const c = analyzeTiers(tiers);
    expect(c.firstCollapsedTier).toBeNull();
  });

  it('notes the count of evidence_incomplete tiers when multiple exist', () => {
    const tiers = [
      makeTier({ tierIndex: 0, result: 'evidence_incomplete' }),
      makeTier({ tierIndex: 1, result: 'evidence_incomplete' }),
      makeTier({ tierIndex: 2, result: 'completed' }),
    ];
    const c = analyzeTiers(tiers);
    const incompleteNote = c.notes.find((n) => n.includes('incomplete metric evidence'));
    expect(incompleteNote).toContain('2');
  });

  it('adds incomplete-evidence note when first collapsed tier has null metrics', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: null,
      mempoolAcceptedDelta: null,
    });
    const c = analyzeTiers([tier]);
    expect(c.notes.some((n) => n.includes('incomplete'))).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Heuristic labeling — acceptance criteria
// ---------------------------------------------------------------------------

describe('analyzeTiers — heuristic labeling', () => {
  it('does not label direct collapse reasons as heuristic', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'node_unavailable')]);
    expect(c.notes.some((n) => n.toLowerCase().includes('heuristic'))).toBe(false);
  });

  it('labels metric-gap analysis conclusions as heuristic', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 6000,
      mempoolAcceptedDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.notes.some((n) => n.toLowerCase().includes('heuristic'))).toBe(true);
  });

  it('labels undetermined conclusion as heuristic', () => {
    const tier = makeCollapsed(0, 100, 'useful_throughput_below_threshold', {
      enqueuedDelta: 0,
      mempoolAcceptedDelta: 0,
      committedTxDelta: 0,
      committedBlockDelta: 0,
      submittedBlockDelta: 0,
      mergeFailureDelta: 0,
    });
    const c = analyzeTiers([tier]);
    expect(c.notes.some((n) => n.toLowerCase().includes('heuristic'))).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// Return shape
// ---------------------------------------------------------------------------

describe('analyzeTiers — return shape', () => {
  it('always returns an array for notes', () => {
    const c: BenchmarkConclusion = analyzeTiers([]);
    expect(Array.isArray(c.notes)).toBe(true);
  });

  it('always returns a string for primaryBottleneck', () => {
    const c = analyzeTiers([makeTier()]);
    expect(typeof c.primaryBottleneck).toBe('string');
  });

  it('carries the correct tierIndex when firstCollapsedTier is 0', () => {
    const c = analyzeTiers([makeCollapsed(0, 50, 'node_unavailable')]);
    expect(c.firstCollapsedTier).toBe(0);
    expect(c.firstCollapsedTargetTps).toBe(50);
  });

  it('always returns criteriaChecks and violatedChecks arrays', () => {
    const c = analyzeTiers([makeTier()]);
    expect(Array.isArray(c.criteriaChecks)).toBe(true);
    expect(Array.isArray(c.violatedChecks)).toBe(true);
  });
});

describe('analyzeTiers — formal classification', () => {
  it('classifies runs as Failed when failure-severity checks are violated', () => {
    const c = analyzeTiers([makeCollapsed(0, 100, 'node_unavailable')]);
    expect(c.classification).toBe('Failed');
    expect(c.violatedChecks.some((check) => check.severity === 'failure')).toBe(true);
  });

  it('classifies runs as Passed with Observations when only observation checks are violated', () => {
    const c = analyzeTiers([makeTier({ result: 'evidence_incomplete' })], {
      policy: { maxEvidenceIncompleteTiers: 0, minCompletedTiers: 0 },
    });
    expect(c.classification).toBe('Passed with Observations');
    expect(
      c.violatedChecks.some(
        (check) => check.id === 'max_evidence_incomplete_tiers' && check.severity === 'observation'
      )
    ).toBe(true);
  });

  it('classifies as Blocked when harnessErrorOccurred is true', () => {
    const c = analyzeTiers([makeTier()], {
      harnessErrorOccurred: true,
      harnessErrorMessage: 'synthetic harness error',
    });
    expect(c.classification).toBe('Blocked');
    expect(
      c.classificationReasons.some((reason) => reason.includes('synthetic harness error'))
    ).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// p95 inclusion latency check (gap 6)
// ---------------------------------------------------------------------------

describe('analyzeTiers — p95 inclusion latency check', () => {
  it('is not_evaluable when policy threshold is not set', () => {
    const c = analyzeTiers([makeTier({ acceptedToCommittedLatencyP95Ms: 10_000 })]);
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.outcome).toBe('not_evaluable');
    expect(check?.expected).toBe('disabled');
  });

  it('passes when observed p95 is below threshold', () => {
    const c = analyzeTiers([makeTier({ acceptedToCommittedLatencyP95Ms: 18_000 })], {
      policy: { maxP95InclusionLatencyMs: 20_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.outcome).toBe('passed');
  });

  it('violates when observed p95 exceeds threshold', () => {
    const c = analyzeTiers([makeTier({ acceptedToCommittedLatencyP95Ms: 25_000 })], {
      policy: { maxP95InclusionLatencyMs: 20_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.outcome).toBe('violated');
    expect(check?.severity).toBe('observation');
  });

  it('is not_evaluable when p95 latency data is null', () => {
    const c = analyzeTiers([makeTier({ acceptedToCommittedLatencyP95Ms: null })], {
      policy: { maxP95InclusionLatencyMs: 20_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.outcome).toBe('not_evaluable');
  });

  it('uses the maximum p95 across tiers', () => {
    const tiers = [
      makeTier({ tierIndex: 0, acceptedToCommittedLatencyP95Ms: 12_000 }),
      makeTier({ tierIndex: 1, acceptedToCommittedLatencyP95Ms: 22_000 }),
    ];
    const c = analyzeTiers(tiers, { policy: { maxP95InclusionLatencyMs: 20_000 } });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.outcome).toBe('violated');
    expect(check?.observed).toContain('22000');
  });

  it('violation produces observation-severity, not failure', () => {
    const c = analyzeTiers([makeTier({ acceptedToCommittedLatencyP95Ms: 30_000 })], {
      policy: { maxP95InclusionLatencyMs: 20_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_p95_inclusion_latency');
    expect(check?.severity).toBe('observation');
    // Should be "Passed with Observations", not "Failed"
    expect(c.classification).toBe('Passed with Observations');
  });
});

// ---------------------------------------------------------------------------
// L1 fee per committed tx check (gap 6)
// ---------------------------------------------------------------------------

describe('analyzeTiers — L1 fee per committed tx check', () => {
  it('is not_evaluable when policy threshold is not set', () => {
    const c = analyzeTiers([makeTier({ l1FeePerCommittedL2TxLovelace: 5_000 })]);
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_l1_fee_per_committed_tx');
    expect(check?.outcome).toBe('not_evaluable');
  });

  it('passes when observed fee is below threshold', () => {
    const c = analyzeTiers([makeTier({ l1FeePerCommittedL2TxLovelace: 3_000 })], {
      policy: { maxL1FeePerCommittedTxLovelace: 10_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_l1_fee_per_committed_tx');
    expect(check?.outcome).toBe('passed');
  });

  it('violates when observed fee exceeds threshold', () => {
    const c = analyzeTiers([makeTier({ l1FeePerCommittedL2TxLovelace: 15_000 })], {
      policy: { maxL1FeePerCommittedTxLovelace: 10_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_l1_fee_per_committed_tx');
    expect(check?.outcome).toBe('violated');
    expect(check?.severity).toBe('observation');
  });

  it('is not_evaluable when fee data is null (emulator environment)', () => {
    const c = analyzeTiers([makeTier({ l1FeePerCommittedL2TxLovelace: null })], {
      policy: { maxL1FeePerCommittedTxLovelace: 10_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_l1_fee_per_committed_tx');
    expect(check?.outcome).toBe('not_evaluable');
    expect(check?.observed).toBe('n/a');
  });

  it('violation produces observation-severity, not failure', () => {
    const c = analyzeTiers([makeTier({ l1FeePerCommittedL2TxLovelace: 50_000 })], {
      policy: { maxL1FeePerCommittedTxLovelace: 10_000 },
    });
    const check = c.criteriaChecks.find((ch) => ch.id === 'max_l1_fee_per_committed_tx');
    expect(check?.severity).toBe('observation');
    expect(c.classification).toBe('Passed with Observations');
  });
});
