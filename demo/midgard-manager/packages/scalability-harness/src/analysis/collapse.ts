import type { StopConditions } from '../config/scenario.js';

const DEFAULT_MAX_UNSUBMITTED_BLOCK_BACKLOG_GROWTH = 0;

export type CollapseReason =
  | 'node_unavailable'
  | 'prometheus_down'
  | 'commitment_failures'
  | 'merge_failures'
  | 'queue_not_recovered'
  | 'mempool_not_recovered'
  | 'unsubmitted_backlog_growth'
  | 'tx_generator_failed'
  | 'commit_drain_below_threshold'
  | 'useful_throughput_below_threshold';

export interface CollapseResult {
  reason: CollapseReason;
  values: Record<string, number | boolean | null>;
}

export interface CollapseInputs {
  // Node availability from probe loop results.
  nodeUnavailable: boolean;
  consecutiveProbeFailures?: number;

  // Prometheus up{job="sundial_nodes"} instant scalar. null = metric unreachable.
  prometheusUp: number | null;

  // TX generator exit code. undefined = not tracked; null = killed by signal (clean shutdown).
  txGeneratorExitCode?: number | null;

  // Counter deltas from TierWindowSummary.counterDeltas (deltaLoad field).
  commitmentFailuresDelta: number | null;
  mergeFailuresDelta: number | null;

  // Recovery gauges from TierMetricWindow.afterRecovery.
  // Optional after-load gauge snapshot enables explicit "did not drain" checks
  // even when no absolute maxRecoveryMempoolSize threshold is configured.
  beforeQueueSize?: number | null;
  beforeMempoolSize?: number | null;
  afterLoadMempoolSize?: number | null;
  recoveryQueueSize: number | null;
  recoveryMempoolSize: number | null;
  beforeUnsubmittedBlockBacklog?: number | null;
  recoveryUnsubmittedBlockBacklog?: number | null;
  // Legacy fallback inputs: before explicit backlog gauge wiring, some callers
  // derived backlog growth from commit/submission counter deltas.
  committedBlocksDeltaRecovery?: number | null;
  submittedBlocksDeltaRecovery?: number | null;

  // Node drain health inputs.
  // mempoolAcceptedDelta is the tx_submissions_mempool_accepted_total counter delta
  // over the load phase — NOT tx_submissions_enqueued_total (which only reflects
  // acceptance into the in-memory queue, not into the mempool DB).
  // committedTxDelta is the commit_block_tx_count_total counter delta over the load phase.
  mempoolAcceptedDelta: number | null;
  committedTxDelta?: number | null;
  tierDurationSeconds: number;
  targetTps: number;

  stopConditions: StopConditions;
}

// Priority order (highest to lowest severity):
// 1. node_unavailable              — node completely unreachable
// 2. prometheus_down               — observability lost (when enabled)
// 3. commitment_failures           — L2 protocol failure (when enabled)
// 4. merge_failures                — L2 protocol failure (when enabled)
// 5. queue_not_recovered           — tx queue did not drain (when threshold set)
// 6. mempool_not_recovered         — mempool did not drain (when threshold set)
// 7. unsubmitted_backlog_growth    — DB-backed submission backlog grew during tier
// 8. tx_generator_failed           — load generation process crashed (unreliable data)
// 9. commit_drain_below_threshold  — node committed fewer txs than it accepted (node-health)
// 10. useful_throughput_below_threshold — load-driver delivered below ratio threshold (when set)
export function detectCollapse(inputs: CollapseInputs): CollapseResult | null {
  const { stopConditions } = inputs;

  if (inputs.nodeUnavailable) {
    return {
      reason: 'node_unavailable',
      values: {
        consecutiveProbeFailures: inputs.consecutiveProbeFailures ?? null,
      },
    };
  }

  if (stopConditions.stopOnPrometheusDown) {
    const up = inputs.prometheusUp;
    if (up === null || up === 0) {
      return {
        reason: 'prometheus_down',
        values: { prometheusUp: up },
      };
    }
  }

  if (stopConditions.stopOnCommitmentFailure) {
    const delta = inputs.commitmentFailuresDelta;
    if (delta !== null && delta > 0) {
      if (stopConditions.maxCommitmentFailureRatio !== undefined) {
        const accepted = inputs.mempoolAcceptedDelta;
        if (accepted !== null && accepted > 0) {
          const commitmentFailureRatio = delta / accepted;
          if (commitmentFailureRatio <= stopConditions.maxCommitmentFailureRatio) {
            // Keep running until the configured commitment-failure budget is exceeded.
            // Other stop conditions still apply and may collapse the tier earlier.
          } else {
            return {
              reason: 'commitment_failures',
              values: {
                commitmentFailuresDelta: delta,
                mempoolAcceptedDelta: accepted,
                commitmentFailureRatio,
                maxCommitmentFailureRatio: stopConditions.maxCommitmentFailureRatio,
              },
            };
          }
        } else {
          return {
            reason: 'commitment_failures',
            values: { commitmentFailuresDelta: delta },
          };
        }
      } else {
        return {
          reason: 'commitment_failures',
          values: { commitmentFailuresDelta: delta },
        };
      }
    }
  }

  if (stopConditions.stopOnMergeFailure) {
    const delta = inputs.mergeFailuresDelta;
    if (delta !== null && delta > 0) {
      return {
        reason: 'merge_failures',
        values: { mergeFailuresDelta: delta },
      };
    }
  }

  if (stopConditions.maxRecoveryQueueSize !== undefined) {
    const beforeQueueSize = inputs.beforeQueueSize ?? null;
    const recoveryQueueSize = inputs.recoveryQueueSize;
    if (beforeQueueSize !== null && recoveryQueueSize !== null) {
      const queueGrowth = recoveryQueueSize - beforeQueueSize;
      if (queueGrowth > stopConditions.maxRecoveryQueueSize) {
        return {
          reason: 'queue_not_recovered',
          values: {
            beforeQueueSize,
            recoveryQueueSize,
            queueGrowth,
            maxRecoveryQueueGrowth: stopConditions.maxRecoveryQueueSize,
          },
        };
      }
    }
  }

  if (stopConditions.maxRecoveryMempoolSize !== undefined) {
    const beforeMempoolSize = inputs.beforeMempoolSize ?? null;
    const recoveryMempoolSize = inputs.recoveryMempoolSize;
    if (beforeMempoolSize !== null && recoveryMempoolSize !== null) {
      const mempoolGrowth = recoveryMempoolSize - beforeMempoolSize;
      if (mempoolGrowth > stopConditions.maxRecoveryMempoolSize) {
        return {
          reason: 'mempool_not_recovered',
          values: {
            beforeMempoolSize,
            recoveryMempoolSize,
            mempoolGrowth,
            maxRecoveryMempoolGrowth: stopConditions.maxRecoveryMempoolSize,
          },
        };
      }
    }
  }

  // If mempool had backlog at the end of load, recovery must reduce it.
  // A flat or increasing mempool during recovery is treated as non-recovery.
  const afterLoadMempoolSize = inputs.afterLoadMempoolSize ?? null;
  const recoveryMempoolSize = inputs.recoveryMempoolSize;
  if (
    afterLoadMempoolSize !== null &&
    afterLoadMempoolSize > 0 &&
    recoveryMempoolSize !== null &&
    recoveryMempoolSize >= afterLoadMempoolSize
  ) {
    return {
      reason: 'mempool_not_recovered',
      values: {
        afterLoadMempoolSize,
        recoveryMempoolSize,
        drainedMempoolTxCount: afterLoadMempoolSize - recoveryMempoolSize,
      },
    };
  }

  {
    const maxUnsubmittedBlockBacklogGrowth =
      stopConditions.maxUnsubmittedBlockBacklogGrowth ??
      DEFAULT_MAX_UNSUBMITTED_BLOCK_BACKLOG_GROWTH;
    const beforeBacklog = inputs.beforeUnsubmittedBlockBacklog ?? null;
    const recoveryBacklog = inputs.recoveryUnsubmittedBlockBacklog ?? null;
    const legacyCommittedBlocksDeltaRecovery = inputs.committedBlocksDeltaRecovery ?? null;
    const legacySubmittedBlocksDeltaRecovery = inputs.submittedBlocksDeltaRecovery ?? null;
    if (
      beforeBacklog !== null &&
      recoveryBacklog !== null &&
      beforeBacklog >= 0 &&
      recoveryBacklog >= 0
    ) {
      const unsubmittedBacklogGrowth = recoveryBacklog - beforeBacklog;
      if (unsubmittedBacklogGrowth > maxUnsubmittedBlockBacklogGrowth) {
        return {
          reason: 'unsubmitted_backlog_growth',
          values: {
            beforeUnsubmittedBlockBacklog: beforeBacklog,
            recoveryUnsubmittedBlockBacklog: recoveryBacklog,
            unsubmittedBacklogGrowth,
            maxUnsubmittedBlockBacklogGrowth,
          },
        };
      }
    } else if (
      legacyCommittedBlocksDeltaRecovery !== null &&
      legacySubmittedBlocksDeltaRecovery !== null
    ) {
      const unsubmittedBacklogGrowth =
        legacyCommittedBlocksDeltaRecovery - legacySubmittedBlocksDeltaRecovery;
      if (unsubmittedBacklogGrowth > maxUnsubmittedBlockBacklogGrowth) {
        return {
          reason: 'unsubmitted_backlog_growth',
          values: {
            committedBlocksDeltaRecovery: legacyCommittedBlocksDeltaRecovery,
            submittedBlocksDeltaRecovery: legacySubmittedBlocksDeltaRecovery,
            unsubmittedBacklogGrowth,
            maxUnsubmittedBlockBacklogGrowth,
          },
        };
      }
    }
  }

  const exitCode = inputs.txGeneratorExitCode;
  if (exitCode !== undefined && exitCode !== null && exitCode !== 0) {
    return {
      reason: 'tx_generator_failed',
      values: { exitCode },
    };
  }

  if (stopConditions.minCommitToAcceptedRatio !== undefined) {
    const accepted = inputs.mempoolAcceptedDelta;
    const committed = inputs.committedTxDelta ?? null;
    if (accepted !== null && committed !== null && accepted > 0) {
      const commitToAcceptedRatio = committed / accepted;
      if (commitToAcceptedRatio < stopConditions.minCommitToAcceptedRatio) {
        return {
          reason: 'commit_drain_below_threshold',
          values: {
            committedTxDelta: committed,
            mempoolAcceptedDelta: accepted,
            commitToAcceptedRatio,
            minCommitToAcceptedRatio: stopConditions.minCommitToAcceptedRatio,
          },
        };
      }
    }
  }

  if (stopConditions.minUsefulThroughputRatio !== undefined) {
    const delta = inputs.mempoolAcceptedDelta;
    if (delta !== null && inputs.tierDurationSeconds > 0 && inputs.targetTps > 0) {
      const observedMempoolAcceptedTps = delta / inputs.tierDurationSeconds;
      const usefulThroughputRatio = observedMempoolAcceptedTps / inputs.targetTps;
      if (usefulThroughputRatio < stopConditions.minUsefulThroughputRatio) {
        return {
          reason: 'useful_throughput_below_threshold',
          values: {
            mempoolAcceptedDelta: delta,
            tierDurationSeconds: inputs.tierDurationSeconds,
            observedMempoolAcceptedTps,
            targetTps: inputs.targetTps,
            usefulThroughputRatio,
            minUsefulThroughputRatio: stopConditions.minUsefulThroughputRatio,
          },
        };
      }
    }
  }

  return null;
}
