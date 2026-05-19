import { describe, expect, it } from 'vitest';

import type { BenchmarkConclusion } from '../../src/analysis/analyzer.js';
import type { TierSummary } from '../../src/analysis/tier-summary.js';
import type { ScalabilityScenario } from '../../src/config/scenario.js';
import type { RunManifest } from '../../src/evidence/artifacts.js';
import type { ReportInput } from '../../src/report/markdown.js';
import { renderReport } from '../../src/report/markdown.js';

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

function makeManifest(overrides: Partial<RunManifest> = {}): RunManifest {
  return {
    runId: 'test-run-001',
    startedAt: '2025-01-01T00:00:00.000Z',
    gitSha: 'abc1234',
    nodeEndpoint: 'http://localhost:3000',
    prometheusEndpoint: 'http://localhost:9090',
    scenarioPath: '/runs/scenario.json',
    harnessVersion: '0.1.0',
    replayCorpusPath: null,
    replayCorpusSha256: null,
    host: {
      hostname: 'bench-host',
      platform: 'linux',
      arch: 'x64',
      cpus: 8,
      totalMemoryBytes: 17_179_869_184, // 16 GB
    },
    ...overrides,
  };
}

function makeScenario(overrides: Partial<ScalabilityScenario> = {}): ScalabilityScenario {
  return {
    runId: 'test-run-001',
    nodeEndpoint: 'http://localhost:3000',
    prometheusEndpoint: 'http://localhost:9090',
    outputDir: '/tmp/runs',
    seed: 'test-seed',
    transactionType: 'one-to-one',
    tierDurationSeconds: 60,
    recoverySeconds: 30,
    startTps: 100,
    maxTps: 800,
    stepMultiplier: 2,
    txGeneratorTaskCostSeconds: 0.2,
    retryAttempts: 3,
    retryDelayMs: 500,
    stopConditions: {
      maxConsecutiveNodeProbeFailures: 3,
      stopOnPrometheusDown: true,
      stopOnCommitmentFailure: true,
      stopOnMergeFailure: true,
    },
    ...overrides,
  };
}

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
    rejectedDelta: 10,
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
    observedEnqueuedTps: 100.0,
    observedMempoolAcceptedTps: 96.67,
    observedCommittedTps: 93.33,
    peakQueueSize: 50,
    finalQueueSizeAfterRecovery: 0,
    peakMempoolSize: 200,
    finalMempoolSizeAfterRecovery: 0,
    clientSubmittedCount: 5800,
    clientRejectedCount: 10,
    clientNodeUnavailableCount: 0,
    clientErrorCount: 0,
    clientTotalRetries: 20,
    clientRetriedSubmissionCount: 12,
    clientSubmittedLatencyP95Ms: 250,
    acceptedToCommittedLatencyMethod: 'cohort_counter_alignment_v1',
    acceptedToCommittedLatencyConfidence: 'medium',
    acceptedToCommittedLatencyConfidenceNotes: [
      'Scrape step is approximately 15s; latency resolution is bounded by this interval.',
    ],
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

function makeCollapsedTier(overrides: Partial<TierSummary> = {}): TierSummary {
  return makeTier({
    tierIndex: 1,
    targetTps: 200,
    result: 'collapsed',
    collapseReason: 'commitment_failures',
    enqueuedDelta: 3000,
    mempoolAcceptedDelta: 2800,
    committedTxDelta: 100,
    committedBlockDelta: 2,
    submittedBlockDelta: 1,
    commitmentFailureDelta: 5,
    mergeFailureDelta: 0,
    l1CommitmentFeesDeltaLovelace: 4_000_000,
    l1CommitmentFeeLastLovelace: 2_000_000,
    l1FeePerCommittedL2TxLovelace: 40_000,
    observedEnqueuedTps: 50.0,
    observedMempoolAcceptedTps: 46.67,
    observedCommittedTps: 1.67,
    ...overrides,
  });
}

function makeConclusion(overrides: Partial<BenchmarkConclusion> = {}): BenchmarkConclusion {
  return {
    highestCompletedTier: 0,
    highestCompletedTargetTps: 100,
    firstCollapsedTier: 1,
    firstCollapsedTargetTps: 200,
    primaryBottleneck: 'block commitment (commitment failures detected)',
    classification: 'Failed',
    classificationReasons: ['Collapsed tiers are within threshold: 1 (expected <= 0)'],
    violatedChecks: [
      {
        id: 'max_collapsed_tiers',
        name: 'Collapsed tiers are within threshold',
        severity: 'failure',
        outcome: 'violated',
        expected: '<= 0',
        observed: '1',
        details: 'Collapsed tier count exceeds threshold by 1.',
      },
    ],
    criteriaChecks: [
      {
        id: 'max_collapsed_tiers',
        name: 'Collapsed tiers are within threshold',
        severity: 'failure',
        outcome: 'violated',
        expected: '<= 0',
        observed: '1',
        details: 'Collapsed tier count exceeds threshold by 1.',
      },
    ],
    policy: {
      maxCollapsedTiers: 0,
      minCompletedTiers: 1,
      maxEvidenceIncompleteTiers: 0,
      minDurableThroughputRatio: 0.5,
      maxRejectedRatio: 0.02,
      maxProcessingFailedRatio: 0.01,
    },
    notes: [],
    ...overrides,
  };
}

function makeInput(overrides: Partial<ReportInput> = {}): ReportInput {
  return {
    manifest: makeManifest(),
    scenario: makeScenario(),
    tierSummaries: [makeTier(), makeCollapsedTier()],
    conclusion: makeConclusion(),
    artifactFiles: [
      'scenario.json',
      'run-manifest.json',
      'load-events.jsonl',
      'tier-summaries.jsonl',
      'prometheus-samples.json',
      'summary.json',
      'report.md',
      'stdout.log',
      'stderr.log',
    ],
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// Section headers
// ---------------------------------------------------------------------------

describe('renderReport — section headers', () => {
  it('includes the top-level title', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('# Scalability Baseline Report');
  });

  it('includes ## Run Metadata', () => {
    expect(renderReport(makeInput())).toContain('## Run Metadata');
  });

  it('includes ## Scenario', () => {
    expect(renderReport(makeInput())).toContain('## Scenario');
  });

  it('includes ## Tier Results', () => {
    expect(renderReport(makeInput())).toContain('## Tier Results');
  });

  it('includes ## Formal Run Classification', () => {
    expect(renderReport(makeInput())).toContain('## Formal Run Classification');
  });

  it('includes ## Collapse Point', () => {
    expect(renderReport(makeInput())).toContain('## Collapse Point');
  });

  it('includes ## Throughput Stage Deltas', () => {
    expect(renderReport(makeInput())).toContain('## Throughput Stage Deltas');
  });

  it('includes ## Queue and Mempool Behavior', () => {
    expect(renderReport(makeInput())).toContain('## Queue and Mempool Behavior');
  });

  it('includes ## Client Submission Evidence', () => {
    expect(renderReport(makeInput())).toContain('## Client Submission Evidence');
  });

  it('includes ## Accepted-to-Committed Latency Evidence', () => {
    expect(renderReport(makeInput())).toContain('## Accepted-to-Committed Latency Evidence');
  });

  it('includes ## Commit/Submit/Merge Progress', () => {
    expect(renderReport(makeInput())).toContain('## Commit/Submit/Merge Progress');
  });

  it('includes ## Failure Signals', () => {
    expect(renderReport(makeInput())).toContain('## Failure Signals');
  });

  it('includes ## Primary Bottleneck Hypothesis', () => {
    expect(renderReport(makeInput())).toContain('## Primary Bottleneck Hypothesis');
  });

  it('includes ## Artifact Index', () => {
    expect(renderReport(makeInput())).toContain('## Artifact Index');
  });

  it('includes ## Limitations', () => {
    expect(renderReport(makeInput())).toContain('## Limitations');
  });
});

// ---------------------------------------------------------------------------
// Required wording
// ---------------------------------------------------------------------------

describe('renderReport — required wording', () => {
  it('states that the run measures current behavior without remediation', () => {
    const out = renderReport(makeInput());
    expect(out.toLowerCase()).toContain('without remediation');
  });

  it('does not claim production scalability', () => {
    const out = renderReport(makeInput());
    // Must not contain affirmative production scalability claims.
    // The only allowed mentions are disclaimers ("does not claim/imply production scalability").
    const lower = out.toLowerCase();
    // Verify the disclaimer is present.
    expect(lower).toContain('production scalability');
    // Verify it is framed as a negative (does not claim / not claim / not imply).
    const idx = lower.indexOf('production scalability');
    const context = lower.slice(Math.max(0, idx - 60), idx + 40);
    expect(context).toMatch(/not claim|does not|do not/);
  });

  it('distinguishes enqueued TPS from mempool accepted TPS', () => {
    const out = renderReport(makeInput());
    expect(out.toLowerCase()).toContain('enqueued tps');
    expect(out.toLowerCase()).toContain('mempool accepted tps');
  });

  it('distinguishes mempool accepted TPS from committed TPS', () => {
    const out = renderReport(makeInput());
    expect(out.toLowerCase()).toContain('mempool accepted tps');
    expect(out.toLowerCase()).toContain('committed tps');
  });

  it('uses the word distinct or separate when describing TPS stages', () => {
    const out = renderReport(makeInput());
    expect(out.toLowerCase()).toMatch(/distinct|separate/);
  });

  it('describes the accepted-to-committed cohort method and confidence notes', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('cohort_counter_alignment_v1');
    expect(out).toContain('Confidence Notes');
  });
});

// ---------------------------------------------------------------------------
// Tier Results table — columns
// ---------------------------------------------------------------------------

describe('renderReport — Tier Results table columns', () => {
  const requiredColumns = [
    'Tier',
    'Target TPS',
    'Result',
    'Enqueued Δ',
    'Mempool Accepted Δ',
    'Committed Tx Δ',
    'Submitted Blocks Δ',
    'Peak Queue',
    'Peak Mempool',
    'Commit Failures Δ',
    'Merge Failures Δ',
    'Collapse Reason',
  ];

  for (const col of requiredColumns) {
    it(`includes column header "${col}"`, () => {
      expect(renderReport(makeInput())).toContain(col);
    });
  }

  it('shows the collapse reason for a collapsed tier', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('commitment_failures');
  });

  it('shows n/a for collapse reason on a completed tier', () => {
    const out = renderReport(makeInput());
    // The completed tier has no collapseReason, so "n/a" must appear in the table.
    expect(out).toContain('n/a');
  });
});

describe('renderReport — L1 commitment fee reporting', () => {
  it('includes L1 fee metric names and derived formula description', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('l1_commitment_fees_lovelace_total');
    expect(out).toContain('l1_commitment_fee_lovelace_last');
    expect(out).toContain('L1 Fee / Committed L2 Tx');
  });

  it('renders n/a when derived L1 fee per committed tx is unavailable', () => {
    const tier = makeTier({
      l1CommitmentFeesDeltaLovelace: 1_000_000,
      committedTxDelta: 0,
      l1FeePerCommittedL2TxLovelace: null,
    });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });
});

// ---------------------------------------------------------------------------
// Missing metrics → n/a
// ---------------------------------------------------------------------------

describe('renderReport — n/a for missing metrics', () => {
  it('renders n/a for null enqueuedDelta', () => {
    const tier = makeTier({ enqueuedDelta: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null mempoolAcceptedDelta', () => {
    const tier = makeTier({ mempoolAcceptedDelta: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null committedTxDelta', () => {
    const tier = makeTier({ committedTxDelta: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null submittedBlockDelta', () => {
    const tier = makeTier({ submittedBlockDelta: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null peakQueueSize', () => {
    const tier = makeTier({ peakQueueSize: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null peakMempoolSize', () => {
    const tier = makeTier({ peakMempoolSize: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null observedEnqueuedTps in throughput section', () => {
    const tier = makeTier({ observedEnqueuedTps: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null observedMempoolAcceptedTps in throughput section', () => {
    const tier = makeTier({ observedMempoolAcceptedTps: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });

  it('renders n/a for null observedCommittedTps in throughput section', () => {
    const tier = makeTier({ observedCommittedTps: null });
    const out = renderReport(makeInput({ tierSummaries: [tier] }));
    expect(out).toContain('n/a');
  });
});

// ---------------------------------------------------------------------------
// Artifact Index
// ---------------------------------------------------------------------------

describe('renderReport — Artifact Index', () => {
  it('lists every provided artifact file', () => {
    const files = ['scenario.json', 'run-manifest.json', 'load-events.jsonl', 'report.md'];
    const out = renderReport(makeInput({ artifactFiles: files }));
    for (const f of files) {
      expect(out).toContain(f);
    }
  });

  it('renders all nine standard artifact files', () => {
    const out = renderReport(makeInput());
    const files = [
      'scenario.json',
      'run-manifest.json',
      'load-events.jsonl',
      'tier-summaries.jsonl',
      'prometheus-samples.json',
      'summary.json',
      'report.md',
      'stdout.log',
      'stderr.log',
    ];
    for (const f of files) {
      expect(out).toContain(f);
    }
  });

  it('shows placeholder message when no artifact files are provided', () => {
    const out = renderReport(makeInput({ artifactFiles: [] }));
    expect(out).toContain('No artifact files recorded');
  });
});

// ---------------------------------------------------------------------------
// Collapse Point section
// ---------------------------------------------------------------------------

describe('renderReport — Collapse Point', () => {
  it('shows the first collapsed tier index and TPS', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('First collapsed tier');
    expect(out).toContain('200 TPS');
  });

  it('shows the highest completed tier index and TPS', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('Highest completed tier');
    expect(out).toContain('100 TPS');
  });

  it('shows "No collapse detected" when firstCollapsedTier is null', () => {
    const conclusion = makeConclusion({
      firstCollapsedTier: null,
      firstCollapsedTargetTps: null,
    });
    const out = renderReport(makeInput({ conclusion }));
    expect(out).toContain('No collapse detected');
  });

  it('shows "none" for highest completed tier when no tier completed', () => {
    const conclusion = makeConclusion({
      highestCompletedTier: null,
      highestCompletedTargetTps: null,
    });
    const out = renderReport(makeInput({ conclusion }));
    expect(out).toContain('none');
  });
});

// ---------------------------------------------------------------------------
// Primary Bottleneck Hypothesis
// ---------------------------------------------------------------------------

describe('renderReport — Primary Bottleneck Hypothesis', () => {
  it('includes the identified bottleneck string', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('block commitment (commitment failures detected)');
  });

  it('includes analyzer notes when present', () => {
    const conclusion = makeConclusion({
      notes: ['Some tier metrics were incomplete.', 'Bottleneck is heuristic.'],
    });
    const out = renderReport(makeInput({ conclusion }));
    expect(out).toContain('Some tier metrics were incomplete.');
    expect(out).toContain('Bottleneck is heuristic.');
  });

  it('omits the Notes block when conclusion has no notes', () => {
    const conclusion = makeConclusion({ notes: [] });
    const out = renderReport(makeInput({ conclusion }));
    // Still has the bottleneck, but no bullet notes from analyzer
    expect(out).toContain('block commitment');
    // Should not have a stray "**Notes:**" header
    expect(out).not.toContain('**Notes:**');
  });
});

describe('renderReport — Formal Run Classification', () => {
  it('renders the formal classification label', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('**Classification:** Failed');
  });

  it('renders classification reasons when present', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('**Reasons:**');
    expect(out).toContain('Collapsed tiers are within threshold: 1 (expected <= 0)');
  });

  it('renders violated checks table', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('**Violated checks:**');
    expect(out).toContain('Collapsed tiers are within threshold');
  });

  it('renders no-violations placeholder when no checks are violated', () => {
    const conclusion = makeConclusion({
      classification: 'Passed',
      classificationReasons: ['All configured formal run criteria passed.'],
      violatedChecks: [],
      criteriaChecks: [],
    });
    const out = renderReport(makeInput({ conclusion }));
    expect(out).toContain('_No violated checks._');
  });
});

// ---------------------------------------------------------------------------
// Run Metadata
// ---------------------------------------------------------------------------

describe('renderReport — Run Metadata', () => {
  it('includes the run ID', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('test-run-001');
  });

  it('includes the git SHA', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('abc1234');
  });

  it('includes the node endpoint', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('http://localhost:3000');
  });

  it('includes the prometheus endpoint', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('http://localhost:9090');
  });

  it('includes the harness version', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('0.1.0');
  });

  it('shows replay corpus metadata as n/a when replay is not configured', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('Replay Corpus Path');
    expect(out).toContain('Replay Corpus SHA256');
    expect(out).toContain('n/a');
  });
});

// ---------------------------------------------------------------------------
// Scenario section
// ---------------------------------------------------------------------------

describe('renderReport — Scenario', () => {
  it('includes the start TPS', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('100');
  });

  it('includes the max TPS', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('800');
  });

  it('includes the step multiplier', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('2');
  });

  it('includes the tier duration with unit', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('60 s');
  });

  it('includes the seed', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('test-seed');
  });

  it('renders mixed transaction type with ratio', () => {
    const scenario = makeScenario({ transactionType: 'mixed', oneToOneRatio: 70 });
    const out = renderReport(makeInput({ scenario }));
    expect(out).toContain('mixed (70% one-to-one)');
  });

  it('renders optional stop condition thresholds when present', () => {
    const scenario = makeScenario({
      stopConditions: {
        maxConsecutiveNodeProbeFailures: 3,
        stopOnPrometheusDown: true,
        stopOnCommitmentFailure: true,
        stopOnMergeFailure: true,
        maxRecoveryQueueSize: 500,
        maxRecoveryMempoolSize: 1000,
        minUsefulThroughputRatio: 0.5,
      },
    });
    const out = renderReport(makeInput({ scenario }));
    expect(out).toContain('Max Recovery Queue Size');
    expect(out).toContain('500');
    expect(out).toContain('Max Recovery Mempool Size');
    expect(out).toContain('1000');
    expect(out).toContain('Min Useful Throughput Ratio');
    expect(out).toContain('0.5');
  });
});

// ---------------------------------------------------------------------------
// Empty tier list
// ---------------------------------------------------------------------------

describe('renderReport — empty tier list', () => {
  it('still renders all sections when tierSummaries is empty', () => {
    const out = renderReport(makeInput({ tierSummaries: [] }));
    expect(out).toContain('## Tier Results');
    expect(out).toContain('## Throughput Stage Deltas');
    expect(out).toContain('## Queue and Mempool Behavior');
    expect(out).toContain('## Commit/Submit/Merge Progress');
    expect(out).toContain('## Failure Signals');
  });

  it('shows a no-tiers placeholder in the Tier Results section', () => {
    const out = renderReport(makeInput({ tierSummaries: [] }));
    expect(out).toContain('No tiers were executed');
  });
});

// ---------------------------------------------------------------------------
// Collapsed run
// ---------------------------------------------------------------------------

describe('renderReport — collapsed run', () => {
  it('generates a report for a run with a collapsed tier', () => {
    const tiers = [makeTier(), makeCollapsedTier()];
    const conclusion = makeConclusion({
      firstCollapsedTier: 1,
      firstCollapsedTargetTps: 200,
      primaryBottleneck: 'block commitment (commitment failures detected)',
    });
    const out = renderReport(makeInput({ tierSummaries: tiers, conclusion }));
    expect(out).toContain('collapsed');
    expect(out).toContain('commitment_failures');
    expect(out).toContain('block commitment');
  });
});

// ---------------------------------------------------------------------------
// All-completed run
// ---------------------------------------------------------------------------

describe('renderReport — all-completed run', () => {
  it('generates a report when all tiers completed', () => {
    const tiers = [
      makeTier({ tierIndex: 0, targetTps: 100 }),
      makeTier({ tierIndex: 1, targetTps: 200 }),
    ];
    const conclusion = makeConclusion({
      highestCompletedTier: 1,
      highestCompletedTargetTps: 200,
      firstCollapsedTier: null,
      firstCollapsedTargetTps: null,
      primaryBottleneck: 'none detected',
    });
    const out = renderReport(makeInput({ tierSummaries: tiers, conclusion }));
    expect(out).toContain('No collapse detected');
    expect(out).toContain('none detected');
  });
});

// ---------------------------------------------------------------------------
// Return type and structure
// ---------------------------------------------------------------------------

describe('renderReport — return type', () => {
  it('returns a non-empty string', () => {
    const out = renderReport(makeInput());
    expect(typeof out).toBe('string');
    expect(out.length).toBeGreaterThan(0);
  });

  it('returns a string ending with a newline', () => {
    const out = renderReport(makeInput());
    expect(out.endsWith('\n')).toBe(true);
  });

  it('contains markdown table separators', () => {
    const out = renderReport(makeInput());
    expect(out).toContain('| --- |');
  });
});
