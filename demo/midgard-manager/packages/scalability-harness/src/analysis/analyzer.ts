import type { TierSummary } from './tier-summary.js';

export type FormalRunClassification = 'Passed' | 'Passed with Observations' | 'Failed' | 'Blocked';

export interface RunClassificationPolicy {
  maxCollapsedTiers: number;
  minCompletedTiers: number;
  maxEvidenceIncompleteTiers: number;
  minDurableThroughputRatio: number;
  maxRejectedRatio: number;
  maxProcessingFailedRatio: number;
  maxFinalQueueSizeAfterRecovery?: number;
  maxFinalMempoolSizeAfterRecovery?: number;
  maxP95InclusionLatencyMs?: number;
  maxL1FeePerCommittedTxLovelace?: number;
}

export interface ClassificationCheckEvidence {
  id: string;
  name: string;
  severity: 'failure' | 'observation';
  outcome: 'passed' | 'violated' | 'not_evaluable';
  expected: string;
  observed: string;
  details: string;
}

export interface AnalyzeTiersOptions {
  policy?: Partial<RunClassificationPolicy>;
  harnessErrorOccurred?: boolean;
  harnessErrorMessage?: string;
}

export interface BenchmarkConclusion {
  highestCompletedTier: number | null;
  highestCompletedTargetTps: number | null;
  firstCollapsedTier: number | null;
  firstCollapsedTargetTps: number | null;
  primaryBottleneck: string;
  classification: FormalRunClassification;
  classificationReasons: string[];
  violatedChecks: ClassificationCheckEvidence[];
  criteriaChecks: ClassificationCheckEvidence[];
  policy: RunClassificationPolicy;
  notes: string[];
}

export const DEFAULT_RUN_CLASSIFICATION_POLICY: RunClassificationPolicy = {
  maxCollapsedTiers: 0,
  minCompletedTiers: 1,
  maxEvidenceIncompleteTiers: 0,
  minDurableThroughputRatio: 0.5,
  maxRejectedRatio: 0.02,
  maxProcessingFailedRatio: 0.01,
};

// Maps a collapse reason that directly identifies the bottleneck.
// Returns null for reasons that need metric gap analysis to narrow down.
function bottleneckFromCollapseReason(reason: string): string | null {
  switch (reason) {
    case 'node_unavailable':
      return 'API/runtime availability (node probe failed)';
    case 'prometheus_down':
      return 'runtime or observability availability (Prometheus target dropped)';
    case 'commitment_failures':
      return 'block commitment (commitment failures detected)';
    case 'merge_failures':
      return 'merge path (merge failures detected)';
    case 'queue_not_recovered':
      return 'queue processor or transaction parsing (tx queue did not recover after load)';
    case 'mempool_not_recovered':
      return 'block commitment (mempool did not recover after load)';
    case 'tx_generator_failed':
      return 'load generation (tx generator process crashed)';
    default:
      return null;
  }
}

// Checks pipeline stages in order and returns the first detected gap.
// All results from this function are heuristic.
function bottleneckFromMetricGaps(tier: TierSummary): {
  bottleneck: string;
  metricsIncomplete: boolean;
} {
  const {
    enqueuedDelta,
    mempoolAcceptedDelta,
    committedTxDelta,
    committedBlockDelta,
    submittedBlockDelta,
    mergeFailureDelta,
  } = tier;

  const metricsIncomplete =
    enqueuedDelta === null ||
    mempoolAcceptedDelta === null ||
    committedTxDelta === null ||
    committedBlockDelta === null ||
    submittedBlockDelta === null;

  // Stage 1: HTTP queue → durable mempool
  if (
    enqueuedDelta !== null &&
    enqueuedDelta > 0 &&
    (mempoolAcceptedDelta === null || mempoolAcceptedDelta === 0)
  ) {
    return { bottleneck: 'queue processor or transaction parsing', metricsIncomplete };
  }

  // Stage 2: mempool → block commitment
  if (
    mempoolAcceptedDelta !== null &&
    mempoolAcceptedDelta > 0 &&
    (committedTxDelta === null || committedTxDelta === 0)
  ) {
    return { bottleneck: 'block commitment', metricsIncomplete };
  }

  // Stage 3: committed blocks → L1 submission
  if (
    committedBlockDelta !== null &&
    committedBlockDelta > 0 &&
    (submittedBlockDelta === null || submittedBlockDelta === 0)
  ) {
    return { bottleneck: 'L1 submission', metricsIncomplete };
  }

  // Stage 4: submitted blocks → merge (merge failures are the available proxy
  // for "merge does not progress" since there is no merged-blocks counter)
  if (
    submittedBlockDelta !== null &&
    submittedBlockDelta > 0 &&
    mergeFailureDelta !== null &&
    mergeFailureDelta > 0
  ) {
    return { bottleneck: 'merge path', metricsIncomplete };
  }

  return { bottleneck: 'undetermined', metricsIncomplete: true };
}

function toPercent(value: number): string {
  return `${(value * 100).toFixed(2)}%`;
}

function formatNumber(value: number): string {
  return Number.isInteger(value) ? String(value) : value.toFixed(2);
}

function minDurableThroughputRatio(tiers: TierSummary[]): number | null {
  const ratios = tiers
    .filter((t) => t.observedMempoolAcceptedTps !== null && t.targetTps > 0)
    .map((t) => (t.observedMempoolAcceptedTps as number) / t.targetTps);

  if (ratios.length === 0) {
    return null;
  }

  return Math.min(...ratios);
}

function maxNumber(values: Array<number | null>): number | null {
  const nums = values.filter((v): v is number => v !== null);
  if (nums.length === 0) {
    return null;
  }
  return Math.max(...nums);
}

function sumNumbers(values: Array<number | null>): number {
  let total = 0;
  for (const value of values) {
    total += value ?? 0;
  }
  return total;
}

function buildCriteriaChecks(
  summaries: TierSummary[],
  policy: RunClassificationPolicy
): ClassificationCheckEvidence[] {
  const completed = summaries.filter((s) => s.result === 'completed');
  const collapsed = summaries.filter((s) => s.result === 'collapsed');
  const incomplete = summaries.filter((s) => s.result === 'evidence_incomplete');

  const checks: ClassificationCheckEvidence[] = [];

  checks.push({
    id: 'max_collapsed_tiers',
    name: 'Collapsed tiers are within threshold',
    severity: 'failure',
    outcome: collapsed.length <= policy.maxCollapsedTiers ? 'passed' : 'violated',
    expected: `<= ${policy.maxCollapsedTiers}`,
    observed: String(collapsed.length),
    details:
      collapsed.length <= policy.maxCollapsedTiers
        ? 'Collapsed tier count is within allowed threshold.'
        : `Collapsed tier count exceeds threshold by ${collapsed.length - policy.maxCollapsedTiers}.`,
  });

  checks.push({
    id: 'min_completed_tiers',
    name: 'Minimum completed tiers reached',
    severity: 'failure',
    outcome: completed.length >= policy.minCompletedTiers ? 'passed' : 'violated',
    expected: `>= ${policy.minCompletedTiers}`,
    observed: String(completed.length),
    details:
      completed.length >= policy.minCompletedTiers
        ? 'Completed tier count meets the required minimum.'
        : `Completed tier count is below required minimum by ${policy.minCompletedTiers - completed.length}.`,
  });

  checks.push({
    id: 'max_evidence_incomplete_tiers',
    name: 'Evidence-incomplete tiers are bounded',
    severity: 'observation',
    outcome: incomplete.length <= policy.maxEvidenceIncompleteTiers ? 'passed' : 'violated',
    expected: `<= ${policy.maxEvidenceIncompleteTiers}`,
    observed: String(incomplete.length),
    details:
      incomplete.length <= policy.maxEvidenceIncompleteTiers
        ? 'Evidence completeness is within configured tolerance.'
        : `Evidence-incomplete tier count exceeds tolerance by ${incomplete.length - policy.maxEvidenceIncompleteTiers}.`,
  });

  const durableRatio = minDurableThroughputRatio(completed);
  checks.push({
    id: 'min_durable_throughput_ratio',
    name: 'Durable throughput ratio floor',
    severity: 'failure',
    outcome:
      durableRatio === null
        ? 'not_evaluable'
        : durableRatio >= policy.minDurableThroughputRatio
          ? 'passed'
          : 'violated',
    expected: `>= ${toPercent(policy.minDurableThroughputRatio)}`,
    observed: durableRatio === null ? 'n/a' : toPercent(durableRatio),
    details:
      durableRatio === null
        ? 'No completed tiers with durable throughput metrics were available.'
        : 'Observed as the minimum (observed durable TPS / target TPS) across completed tiers.',
  });

  const totalEnqueued = sumNumbers(summaries.map((s) => s.enqueuedDelta));
  const totalRejected = sumNumbers(summaries.map((s) => s.rejectedDelta));
  const rejectedRatio = totalEnqueued > 0 ? totalRejected / totalEnqueued : null;
  checks.push({
    id: 'max_rejected_ratio',
    name: 'Rejected submission ratio',
    severity: 'observation',
    outcome:
      rejectedRatio === null
        ? 'not_evaluable'
        : rejectedRatio <= policy.maxRejectedRatio
          ? 'passed'
          : 'violated',
    expected: `<= ${toPercent(policy.maxRejectedRatio)}`,
    observed: rejectedRatio === null ? 'n/a' : toPercent(rejectedRatio),
    details:
      rejectedRatio === null
        ? 'No enqueued submissions were recorded, so ratio cannot be evaluated.'
        : `Computed from aggregate rejected (${totalRejected}) / enqueued (${totalEnqueued}).`,
  });

  const totalProcessingFailed = sumNumbers(summaries.map((s) => s.processingFailedDelta));
  const processingFailedRatio = totalEnqueued > 0 ? totalProcessingFailed / totalEnqueued : null;
  checks.push({
    id: 'max_processing_failed_ratio',
    name: 'Processing-failed ratio',
    severity: 'observation',
    outcome:
      processingFailedRatio === null
        ? 'not_evaluable'
        : processingFailedRatio <= policy.maxProcessingFailedRatio
          ? 'passed'
          : 'violated',
    expected: `<= ${toPercent(policy.maxProcessingFailedRatio)}`,
    observed: processingFailedRatio === null ? 'n/a' : toPercent(processingFailedRatio),
    details:
      processingFailedRatio === null
        ? 'No enqueued submissions were recorded, so ratio cannot be evaluated.'
        : `Computed from aggregate processing_failed (${totalProcessingFailed}) / enqueued (${totalEnqueued}).`,
  });

  const maxFinalQueue = maxNumber(summaries.map((s) => s.finalQueueSizeAfterRecovery));
  checks.push({
    id: 'max_final_queue_size_after_recovery',
    name: 'Final queue size after recovery',
    severity: 'failure',
    outcome:
      policy.maxFinalQueueSizeAfterRecovery === undefined
        ? 'not_evaluable'
        : maxFinalQueue === null
          ? 'not_evaluable'
          : maxFinalQueue <= policy.maxFinalQueueSizeAfterRecovery
            ? 'passed'
            : 'violated',
    expected:
      policy.maxFinalQueueSizeAfterRecovery === undefined
        ? 'disabled'
        : `<= ${formatNumber(policy.maxFinalQueueSizeAfterRecovery)}`,
    observed: maxFinalQueue === null ? 'n/a' : formatNumber(maxFinalQueue),
    details:
      policy.maxFinalQueueSizeAfterRecovery === undefined
        ? 'No threshold configured for final queue size.'
        : maxFinalQueue === null
          ? 'No final queue-size metrics were available.'
          : 'Observed as the maximum final queue size across all tiers.',
  });

  const maxFinalMempool = maxNumber(summaries.map((s) => s.finalMempoolSizeAfterRecovery));
  checks.push({
    id: 'max_final_mempool_size_after_recovery',
    name: 'Final mempool size after recovery',
    severity: 'failure',
    outcome:
      policy.maxFinalMempoolSizeAfterRecovery === undefined
        ? 'not_evaluable'
        : maxFinalMempool === null
          ? 'not_evaluable'
          : maxFinalMempool <= policy.maxFinalMempoolSizeAfterRecovery
            ? 'passed'
            : 'violated',
    expected:
      policy.maxFinalMempoolSizeAfterRecovery === undefined
        ? 'disabled'
        : `<= ${formatNumber(policy.maxFinalMempoolSizeAfterRecovery)}`,
    observed: maxFinalMempool === null ? 'n/a' : formatNumber(maxFinalMempool),
    details:
      policy.maxFinalMempoolSizeAfterRecovery === undefined
        ? 'No threshold configured for final mempool size.'
        : maxFinalMempool === null
          ? 'No final mempool-size metrics were available.'
          : 'Observed as the maximum final mempool size across all tiers.',
  });

  // p95 inclusion latency — observation-severity: the cohort-alignment estimator
  // has confidence caveats and may produce null or low-confidence results.
  const maxP95Latency = maxNumber(summaries.map((s) => s.acceptedToCommittedLatencyP95Ms));
  checks.push({
    id: 'max_p95_inclusion_latency',
    name: 'p95 mempool-accepted-to-committed inclusion latency',
    severity: 'observation',
    outcome:
      policy.maxP95InclusionLatencyMs === undefined
        ? 'not_evaluable'
        : maxP95Latency === null
          ? 'not_evaluable'
          : maxP95Latency <= policy.maxP95InclusionLatencyMs
            ? 'passed'
            : 'violated',
    expected:
      policy.maxP95InclusionLatencyMs === undefined
        ? 'disabled'
        : `<= ${formatNumber(policy.maxP95InclusionLatencyMs)} ms`,
    observed: maxP95Latency === null ? 'n/a' : `${formatNumber(maxP95Latency)} ms`,
    details:
      policy.maxP95InclusionLatencyMs === undefined
        ? 'No p95 inclusion latency threshold configured.'
        : maxP95Latency === null
          ? 'No p95 latency estimate available (insufficient Prometheus range data or no committed transactions).'
          : 'Observed as the maximum p95 accepted-to-committed latency across all tiers. ' +
            'Estimated via cohort counter alignment; treat as heuristic where confidence is low.',
  });

  // L1 fee per committed L2 transaction — observation-severity: zero on emulator
  // environments; meaningful only for runs with active L1 commitment.
  const maxL1Fee = maxNumber(summaries.map((s) => s.l1FeePerCommittedL2TxLovelace));
  checks.push({
    id: 'max_l1_fee_per_committed_tx',
    name: 'L1 fee per committed L2 transaction',
    severity: 'observation',
    outcome:
      policy.maxL1FeePerCommittedTxLovelace === undefined
        ? 'not_evaluable'
        : maxL1Fee === null
          ? 'not_evaluable'
          : maxL1Fee <= policy.maxL1FeePerCommittedTxLovelace
            ? 'passed'
            : 'violated',
    expected:
      policy.maxL1FeePerCommittedTxLovelace === undefined
        ? 'disabled'
        : `<= ${formatNumber(policy.maxL1FeePerCommittedTxLovelace)} lovelace`,
    observed: maxL1Fee === null ? 'n/a' : `${formatNumber(maxL1Fee)} lovelace`,
    details:
      policy.maxL1FeePerCommittedTxLovelace === undefined
        ? 'No L1 fee-per-committed-tx threshold configured. ' +
          'Set runClassificationPolicy.maxL1FeePerCommittedTxLovelace to enable this check.'
        : maxL1Fee === null
          ? 'No L1 fee data available (emulator environment or no committed blocks with fee metrics).'
          : 'Observed as the maximum l1_commitment_fees_lovelace_total Δ / commit_block_tx_count_total Δ across all tiers.',
  });

  return checks;
}

function classifyRun(
  checks: ClassificationCheckEvidence[],
  options: AnalyzeTiersOptions,
  summaries: TierSummary[]
): { classification: FormalRunClassification; reasons: string[] } {
  if (options.harnessErrorOccurred) {
    return {
      classification: 'Blocked',
      reasons: [options.harnessErrorMessage ?? 'Harness execution failed before clean completion.'],
    };
  }

  if (summaries.length === 0) {
    return {
      classification: 'Blocked',
      reasons: ['No tiers were executed; run has no measurable outcome.'],
    };
  }

  const violated = checks.filter((c) => c.outcome === 'violated');
  const failed = violated.filter((c) => c.severity === 'failure');
  const observations = violated.filter((c) => c.severity === 'observation');

  if (failed.length > 0) {
    return {
      classification: 'Failed',
      reasons: failed.map((c) => `${c.name}: ${c.observed} (expected ${c.expected})`),
    };
  }

  if (observations.length > 0) {
    return {
      classification: 'Passed with Observations',
      reasons: observations.map((c) => `${c.name}: ${c.observed} (expected ${c.expected})`),
    };
  }

  return {
    classification: 'Passed',
    reasons: ['All configured formal run criteria passed.'],
  };
}

export function analyzeTiers(
  summaries: TierSummary[],
  options: AnalyzeTiersOptions = {}
): BenchmarkConclusion {
  const notes: string[] = [];
  const policy: RunClassificationPolicy = {
    ...DEFAULT_RUN_CLASSIFICATION_POLICY,
    ...(options.policy ?? {}),
  };

  const criteriaChecks = buildCriteriaChecks(summaries, policy);
  const violatedChecks = criteriaChecks.filter((c) => c.outcome === 'violated');
  const classified = classifyRun(criteriaChecks, options, summaries);

  if (summaries.length === 0) {
    notes.push('No tiers executed.');
    return {
      highestCompletedTier: null,
      highestCompletedTargetTps: null,
      firstCollapsedTier: null,
      firstCollapsedTargetTps: null,
      primaryBottleneck: 'no data',
      classification: classified.classification,
      classificationReasons: classified.reasons,
      violatedChecks,
      criteriaChecks,
      policy,
      notes,
    };
  }

  // Highest completed tier (by tierIndex)
  const completedTiers = summaries.filter((s) => s.result === 'completed');
  const highestCompleted =
    completedTiers.length > 0
      ? completedTiers.reduce((a, b) => (a.tierIndex >= b.tierIndex ? a : b))
      : null;

  // First collapsed tier (lowest tierIndex)
  const collapsedTiers = summaries
    .filter((s) => s.result === 'collapsed')
    .sort((a, b) => a.tierIndex - b.tierIndex);
  const firstCollapsed = collapsedTiers[0] ?? null;

  // Note any tiers with incomplete metric evidence
  const incompleteCount = summaries.filter((s) => s.result === 'evidence_incomplete').length;
  if (incompleteCount > 0) {
    notes.push(
      `${incompleteCount} tier(s) reported incomplete metric evidence; conclusions drawn from those tiers are heuristic.`
    );
  }

  if (firstCollapsed === null) {
    notes.push('No collapse detected; all tiers completed or ran with incomplete evidence.');
    return {
      highestCompletedTier: highestCompleted?.tierIndex ?? null,
      highestCompletedTargetTps: highestCompleted?.targetTps ?? null,
      firstCollapsedTier: null,
      firstCollapsedTargetTps: null,
      primaryBottleneck: 'none detected',
      classification: classified.classification,
      classificationReasons: classified.reasons,
      violatedChecks,
      criteriaChecks,
      policy,
      notes,
    };
  }

  // Determine primary bottleneck from the first collapsed tier
  const collapseReason = firstCollapsed.collapseReason;
  let primaryBottleneck: string;
  let isHeuristic = false;

  const directBottleneck =
    collapseReason !== undefined ? bottleneckFromCollapseReason(collapseReason) : null;

  if (directBottleneck !== null) {
    primaryBottleneck = directBottleneck;
  } else {
    // useful_throughput_below_threshold or unknown collapse reason — infer from metric gaps
    const result = bottleneckFromMetricGaps(firstCollapsed);
    primaryBottleneck = result.bottleneck;
    isHeuristic = true;
    if (result.metricsIncomplete) {
      notes.push(
        'Metric data for the first collapsed tier is incomplete; bottleneck attribution is heuristic.'
      );
    }
  }

  if (isHeuristic) {
    notes.push(
      'Bottleneck identification is based on observed metric gaps; conclusions are heuristic, not definitive.'
    );
  }

  return {
    highestCompletedTier: highestCompleted?.tierIndex ?? null,
    highestCompletedTargetTps: highestCompleted?.targetTps ?? null,
    firstCollapsedTier: firstCollapsed.tierIndex,
    firstCollapsedTargetTps: firstCollapsed.targetTps,
    primaryBottleneck,
    classification: classified.classification,
    classificationReasons: classified.reasons,
    violatedChecks,
    criteriaChecks,
    policy,
    notes,
  };
}
