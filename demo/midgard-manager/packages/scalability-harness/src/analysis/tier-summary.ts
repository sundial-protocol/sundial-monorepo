import type { CounterDelta, GaugeSummary, TierWindowSummary } from '../metrics/window.js';
import type { TierMetricWindow } from '../metrics/window.js';
import type {
  LoadDriverResourceEvidence,
  LoadDriverSaturationFlags,
} from '../runner/host-resources.js';
import type { SubmissionAggregate } from '../runner/tx-generator.js';
import { estimateAcceptedToCommittedLatency } from './accepted-to-committed-latency.js';
import type { CollapseResult } from './collapse.js';

export interface TierSummary {
  tierIndex: number;
  targetTps: number;
  startedAt: string;
  stoppedAt: string;
  // Full tier window: load + recovery.
  durationSeconds: number;
  // Load phase only. This is the correct denominator for TPS metrics because
  // counter deltas (deltaLoad) cover only the load phase.
  loadDurationSeconds: number;
  result: 'completed' | 'collapsed' | 'evidence_incomplete';
  collapseReason?: string;
  // HTTP boundary acceptance into the in-memory tx queue.
  enqueuedDelta: number | null;
  rejectedDelta: number | null;
  // Durable acceptance into MempoolDB — distinct from enqueued.
  mempoolAcceptedDelta: number | null;
  processingFailedDelta: number | null;
  // Block commitment inclusion.
  committedTxDelta: number | null;
  committedBlockDelta: number | null;
  // L1 submission progress.
  submittedBlockDelta: number | null;
  mergeFailureDelta: number | null;
  commitmentFailureDelta: number | null;
  l1CommitmentFeesDeltaLovelace: number | null;
  l1CommitmentFeeLastLovelace: number | null;
  l1FeePerCommittedL2TxLovelace: number | null;
  // Observed TPS derived from counter deltas / durationSeconds.
  observedEnqueuedTps: number | null;
  observedMempoolAcceptedTps: number | null;
  observedCommittedTps: number | null;
  // Queue and mempool gauge summaries over the full tier window.
  peakQueueSize: number | null;
  finalQueueSizeAfterRecovery: number | null;
  peakMempoolSize: number | null;
  finalMempoolSizeAfterRecovery: number | null;
  clientSubmittedCount: number | null;
  clientRejectedCount: number | null;
  clientNodeUnavailableCount: number | null;
  clientErrorCount: number | null;
  clientTotalRetries: number | null;
  clientRetriedSubmissionCount: number | null;
  clientSubmittedLatencyP95Ms: number | null;
  acceptedToCommittedLatencyMethod: string | null;
  acceptedToCommittedLatencyConfidence: 'high' | 'medium' | 'low' | 'insufficient_data' | null;
  acceptedToCommittedLatencyConfidenceNotes: string[];
  acceptedToCommittedLatencyP50Ms: number | null;
  acceptedToCommittedLatencyP95Ms: number | null;
  acceptedToCommittedLatencyP99Ms: number | null;
  acceptedToCommittedResolvedRatio: number | null;
  acceptedToCommittedResolvedTxCount: number | null;
  acceptedToCommittedAcceptedTxCount: number | null;
  loadDriverResourceEvidence: LoadDriverResourceEvidence | null;
  loadDriverSaturationFlags: LoadDriverSaturationFlags | null;
}

export interface TierSummaryInput {
  tierIndex: number;
  targetTps: number;
  startedAt: string;
  // End of the load phase (when the tx generator stopped). Used as the
  // denominator for TPS metrics since counter deltas cover the load phase only.
  loadStoppedAt: string;
  // End of the full tier window (after recovery). Used for durationSeconds.
  stoppedAt: string;
  metricWindow: TierMetricWindow | null;
  windowSummary: TierWindowSummary | null;
  submissionAggregate: SubmissionAggregate | null;
  loadDriverResourceEvidence: LoadDriverResourceEvidence | null;
  collapse: CollapseResult | null;
  evidenceIncomplete: boolean;
}

function lookupDelta(deltas: CounterDelta[], query: string): number | null {
  return deltas.find((d) => d.query === query)?.deltaLoad ?? null;
}

function lookupGauge(summaries: GaugeSummary[], query: string): GaugeSummary | undefined {
  return summaries.find((s) => s.query === query);
}

function deriveTps(delta: number | null, durationSeconds: number): number | null {
  if (delta === null || durationSeconds <= 0) return null;
  return delta / durationSeconds;
}

function deriveL1FeePerCommittedL2Tx(
  l1CommitmentFeesDeltaLovelace: number | null,
  committedTxDelta: number | null
): number | null {
  if (l1CommitmentFeesDeltaLovelace === null || committedTxDelta === null) {
    return null;
  }
  if (committedTxDelta <= 0) {
    return null;
  }
  return l1CommitmentFeesDeltaLovelace / committedTxDelta;
}

export function buildTierSummary(input: TierSummaryInput): TierSummary {
  const {
    tierIndex,
    targetTps,
    startedAt,
    loadStoppedAt,
    stoppedAt,
    metricWindow,
    windowSummary,
    submissionAggregate,
    loadDriverResourceEvidence,
    collapse,
    evidenceIncomplete,
  } = input;

  const startMs = new Date(startedAt).getTime();
  const durationSeconds = (new Date(stoppedAt).getTime() - startMs) / 1000;
  const loadDurationSeconds = (new Date(loadStoppedAt).getTime() - startMs) / 1000;

  const result: TierSummary['result'] =
    collapse !== null ? 'collapsed' : evidenceIncomplete ? 'evidence_incomplete' : 'completed';

  const counterDeltas = windowSummary?.counterDeltas ?? [];
  const gaugeSummaries = windowSummary?.gaugeSummaries ?? [];

  const enqueuedDelta = lookupDelta(counterDeltas, 'tx_submissions_enqueued_total');
  const mempoolAcceptedDelta = lookupDelta(counterDeltas, 'tx_submissions_mempool_accepted_total');
  const committedTxDelta = lookupDelta(counterDeltas, 'commit_block_tx_count_total');
  const l1CommitmentFeesDeltaLovelace = lookupDelta(
    counterDeltas,
    'l1_commitment_fees_lovelace_total'
  );
  const l1CommitmentFeeLastLovelace =
    metricWindow?.afterLoad['l1_commitment_fee_lovelace_last'] ?? null;

  const queueGauge = lookupGauge(gaugeSummaries, 'tx_queue_size');
  const mempoolGauge = lookupGauge(gaugeSummaries, 'mempool_tx_count');
  const acceptedToCommitted = estimateAcceptedToCommittedLatency(metricWindow);

  return {
    tierIndex,
    targetTps,
    startedAt,
    stoppedAt,
    durationSeconds,
    loadDurationSeconds,
    result,
    ...(collapse !== null ? { collapseReason: collapse.reason } : {}),
    enqueuedDelta,
    rejectedDelta: lookupDelta(counterDeltas, 'tx_submissions_rejected_total'),
    mempoolAcceptedDelta,
    processingFailedDelta: lookupDelta(counterDeltas, 'tx_submissions_processing_failed_total'),
    committedTxDelta,
    committedBlockDelta: lookupDelta(counterDeltas, 'commit_block_count_total'),
    submittedBlockDelta: lookupDelta(counterDeltas, 'submit_block_count_total'),
    mergeFailureDelta: lookupDelta(counterDeltas, 'merge_block_failures_total'),
    commitmentFailureDelta: lookupDelta(counterDeltas, 'commit_block_commitment_failures_total'),
    l1CommitmentFeesDeltaLovelace,
    l1CommitmentFeeLastLovelace,
    l1FeePerCommittedL2TxLovelace: deriveL1FeePerCommittedL2Tx(
      l1CommitmentFeesDeltaLovelace,
      committedTxDelta
    ),
    observedEnqueuedTps: deriveTps(enqueuedDelta, loadDurationSeconds),
    observedMempoolAcceptedTps: deriveTps(mempoolAcceptedDelta, loadDurationSeconds),
    observedCommittedTps: deriveTps(committedTxDelta, loadDurationSeconds),
    peakQueueSize: queueGauge?.peak ?? null,
    finalQueueSizeAfterRecovery: queueGauge?.final ?? null,
    peakMempoolSize: mempoolGauge?.peak ?? null,
    finalMempoolSizeAfterRecovery: mempoolGauge?.final ?? null,
    clientSubmittedCount: submissionAggregate?.counters.submitted ?? null,
    clientRejectedCount: submissionAggregate?.counters.rejected ?? null,
    clientNodeUnavailableCount: submissionAggregate?.counters.node_unavailable ?? null,
    clientErrorCount: submissionAggregate?.counters.error ?? null,
    clientTotalRetries: submissionAggregate?.retries.totalRetries ?? null,
    clientRetriedSubmissionCount: submissionAggregate?.retries.submissionsRetried ?? null,
    clientSubmittedLatencyP95Ms: submissionAggregate?.percentilesMs.submitted.p95 ?? null,
    acceptedToCommittedLatencyMethod: acceptedToCommitted.method,
    acceptedToCommittedLatencyConfidence: acceptedToCommitted.confidence,
    acceptedToCommittedLatencyConfidenceNotes: acceptedToCommitted.confidenceNotes,
    acceptedToCommittedLatencyP50Ms: acceptedToCommitted.percentilesMs.p50,
    acceptedToCommittedLatencyP95Ms: acceptedToCommitted.percentilesMs.p95,
    acceptedToCommittedLatencyP99Ms: acceptedToCommitted.percentilesMs.p99,
    acceptedToCommittedResolvedRatio: acceptedToCommitted.resolvedRatio,
    acceptedToCommittedResolvedTxCount: acceptedToCommitted.resolvedTxCount,
    acceptedToCommittedAcceptedTxCount: acceptedToCommitted.acceptedTxCount,
    loadDriverResourceEvidence,
    loadDriverSaturationFlags: loadDriverResourceEvidence?.saturationFlags ?? null,
  };
}
