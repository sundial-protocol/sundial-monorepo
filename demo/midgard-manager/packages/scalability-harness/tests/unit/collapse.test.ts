import { describe, expect, it } from 'vitest';

import type { CollapseInputs } from '../../src/analysis/collapse.js';
import { detectCollapse } from '../../src/analysis/collapse.js';
import type { StopConditions } from '../../src/config/scenario.js';

// ---------------------------------------------------------------------------
// Base fixtures
// ---------------------------------------------------------------------------

const BASE_STOP_CONDITIONS: StopConditions = {
  maxConsecutiveNodeProbeFailures: 3,
  stopOnPrometheusDown: false,
  stopOnCommitmentFailure: false,
  stopOnMergeFailure: false,
};

function makeInputs(overrides: Partial<CollapseInputs> = {}): CollapseInputs {
  return {
    nodeUnavailable: false,
    prometheusUp: 1,
    txGeneratorExitCode: 0,
    commitmentFailuresDelta: 0,
    mergeFailuresDelta: 0,
    afterLoadMempoolSize: null,
    recoveryQueueSize: 0,
    recoveryMempoolSize: 0,
    mempoolAcceptedDelta: 900,
    committedTxDelta: 900,
    tierDurationSeconds: 60,
    targetTps: 10,
    stopConditions: BASE_STOP_CONDITIONS,
    ...overrides,
  };
}

// ---------------------------------------------------------------------------
// No collapse
// ---------------------------------------------------------------------------

describe('detectCollapse — no collapse', () => {
  it('returns null when all conditions are healthy', () => {
    expect(detectCollapse(makeInputs())).toBeNull();
  });

  it('returns null when stop flags are all false and no thresholds set', () => {
    expect(
      detectCollapse(
        makeInputs({
          stopConditions: {
            ...BASE_STOP_CONDITIONS,
            stopOnPrometheusDown: false,
            stopOnCommitmentFailure: false,
            stopOnMergeFailure: false,
          },
        })
      )
    ).toBeNull();
  });

  it('returns null when mempoolAcceptedDelta is null and throughput threshold is set', () => {
    expect(
      detectCollapse(
        makeInputs({
          mempoolAcceptedDelta: null,
          stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.8 },
        })
      )
    ).toBeNull();
  });

  it('returns null when throughput ratio exactly meets threshold', () => {
    // observedTps = 480/60 = 8; ratio = 8/10 = 0.8; threshold = 0.8 → not below
    expect(
      detectCollapse(
        makeInputs({
          mempoolAcceptedDelta: 480,
          tierDurationSeconds: 60,
          targetTps: 10,
          stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.8 },
        })
      )
    ).toBeNull();
  });

  it('returns null when txGeneratorExitCode is null (killed by signal)', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: null }))).toBeNull();
  });

  it('returns null when txGeneratorExitCode is undefined (not tracked)', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: undefined }))).toBeNull();
  });

  it('returns null when txGeneratorExitCode is 0 (clean exit)', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: 0 }))).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// node_unavailable
// ---------------------------------------------------------------------------

describe('detectCollapse — node_unavailable', () => {
  it('returns node_unavailable when nodeUnavailable is true', () => {
    const result = detectCollapse(makeInputs({ nodeUnavailable: true }));
    expect(result?.reason).toBe('node_unavailable');
  });

  it('includes consecutiveProbeFailures in supporting values', () => {
    const result = detectCollapse(
      makeInputs({ nodeUnavailable: true, consecutiveProbeFailures: 5 })
    );
    expect(result?.values.consecutiveProbeFailures).toBe(5);
  });

  it('includes null for consecutiveProbeFailures when not provided', () => {
    const result = detectCollapse(makeInputs({ nodeUnavailable: true }));
    expect(result?.values.consecutiveProbeFailures).toBeNull();
  });

  it('takes priority over all other reasons', () => {
    const result = detectCollapse(
      makeInputs({
        nodeUnavailable: true,
        prometheusUp: null,
        commitmentFailuresDelta: 5,
        mergeFailuresDelta: 3,
        recoveryQueueSize: 9999,
        recoveryMempoolSize: 9999,
        txGeneratorExitCode: 1,
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 0,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          stopOnPrometheusDown: true,
          stopOnCommitmentFailure: true,
          stopOnMergeFailure: true,
          maxRecoveryQueueSize: 10,
          maxRecoveryMempoolSize: 10,
          minCommitToAcceptedRatio: 1.0,
          minUsefulThroughputRatio: 0.9,
        },
      })
    );
    expect(result?.reason).toBe('node_unavailable');
  });
});

// ---------------------------------------------------------------------------
// prometheus_down
// ---------------------------------------------------------------------------

describe('detectCollapse — prometheus_down', () => {
  it('returns prometheus_down when prometheusUp is null and flag is enabled', () => {
    const result = detectCollapse(
      makeInputs({
        prometheusUp: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnPrometheusDown: true },
      })
    );
    expect(result?.reason).toBe('prometheus_down');
    expect(result?.values.prometheusUp).toBeNull();
  });

  it('returns prometheus_down when prometheusUp is 0 (node not scraped)', () => {
    const result = detectCollapse(
      makeInputs({
        prometheusUp: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnPrometheusDown: true },
      })
    );
    expect(result?.reason).toBe('prometheus_down');
    expect(result?.values.prometheusUp).toBe(0);
  });

  it('does not trigger when stopOnPrometheusDown is false', () => {
    const result = detectCollapse(
      makeInputs({
        prometheusUp: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnPrometheusDown: false },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when prometheusUp is 1', () => {
    const result = detectCollapse(
      makeInputs({
        prometheusUp: 1,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnPrometheusDown: true },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over commitment_failures, merge_failures, drain, and throughput', () => {
    const result = detectCollapse(
      makeInputs({
        prometheusUp: null,
        commitmentFailuresDelta: 5,
        mergeFailuresDelta: 3,
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 0,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          stopOnPrometheusDown: true,
          stopOnCommitmentFailure: true,
          stopOnMergeFailure: true,
          minCommitToAcceptedRatio: 1.0,
          minUsefulThroughputRatio: 0.9,
        },
      })
    );
    expect(result?.reason).toBe('prometheus_down');
  });
});

// ---------------------------------------------------------------------------
// commitment_failures
// ---------------------------------------------------------------------------

describe('detectCollapse — commitment_failures', () => {
  it('returns commitment_failures when delta > 0 and flag enabled', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: 1,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnCommitmentFailure: true },
      })
    );
    expect(result?.reason).toBe('commitment_failures');
    expect(result?.values.commitmentFailuresDelta).toBe(1);
  });

  it('includes the delta value in supporting values', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: 7,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnCommitmentFailure: true },
      })
    );
    expect(result?.values.commitmentFailuresDelta).toBe(7);
  });

  it('does not trigger when flag is false', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: 5,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnCommitmentFailure: false },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when delta is 0', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnCommitmentFailure: true },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when delta is null (evidence incomplete)', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnCommitmentFailure: true },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over merge_failures', () => {
    const result = detectCollapse(
      makeInputs({
        commitmentFailuresDelta: 2,
        mergeFailuresDelta: 3,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          stopOnCommitmentFailure: true,
          stopOnMergeFailure: true,
        },
      })
    );
    expect(result?.reason).toBe('commitment_failures');
  });
});

// ---------------------------------------------------------------------------
// merge_failures
// ---------------------------------------------------------------------------

describe('detectCollapse — merge_failures', () => {
  it('returns merge_failures when delta > 0 and flag enabled', () => {
    const result = detectCollapse(
      makeInputs({
        mergeFailuresDelta: 2,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnMergeFailure: true },
      })
    );
    expect(result?.reason).toBe('merge_failures');
    expect(result?.values.mergeFailuresDelta).toBe(2);
  });

  it('does not trigger when flag is false', () => {
    const result = detectCollapse(
      makeInputs({
        mergeFailuresDelta: 5,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnMergeFailure: false },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when delta is 0', () => {
    const result = detectCollapse(
      makeInputs({
        mergeFailuresDelta: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnMergeFailure: true },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when delta is null (evidence incomplete)', () => {
    const result = detectCollapse(
      makeInputs({
        mergeFailuresDelta: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, stopOnMergeFailure: true },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over queue_not_recovered', () => {
    const result = detectCollapse(
      makeInputs({
        mergeFailuresDelta: 1,
        recoveryQueueSize: 9999,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          stopOnMergeFailure: true,
          maxRecoveryQueueSize: 100,
        },
      })
    );
    expect(result?.reason).toBe('merge_failures');
  });
});

// ---------------------------------------------------------------------------
// queue_not_recovered
// ---------------------------------------------------------------------------

describe('detectCollapse — queue_not_recovered', () => {
  it('returns queue_not_recovered when size exceeds threshold', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: 150,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryQueueSize: 100 },
      })
    );
    expect(result?.reason).toBe('queue_not_recovered');
    expect(result?.values.recoveryQueueSize).toBe(150);
    expect(result?.values.maxRecoveryQueueSize).toBe(100);
  });

  it('does not trigger when size equals threshold', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: 100,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryQueueSize: 100 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when size is below threshold', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: 50,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryQueueSize: 100 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when threshold is not set', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: 9999,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when size is null (evidence incomplete)', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryQueueSize: 100 },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over mempool_not_recovered', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryQueueSize: 200,
        recoveryMempoolSize: 500,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          maxRecoveryQueueSize: 100,
          maxRecoveryMempoolSize: 50,
        },
      })
    );
    expect(result?.reason).toBe('queue_not_recovered');
  });
});

// ---------------------------------------------------------------------------
// mempool_not_recovered
// ---------------------------------------------------------------------------

describe('detectCollapse — mempool_not_recovered', () => {
  it('returns mempool_not_recovered when size exceeds threshold', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryMempoolSize: 600,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryMempoolSize: 500 },
      })
    );
    expect(result?.reason).toBe('mempool_not_recovered');
    expect(result?.values.recoveryMempoolSize).toBe(600);
    expect(result?.values.maxRecoveryMempoolSize).toBe(500);
  });

  it('does not trigger when size equals threshold', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryMempoolSize: 500,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryMempoolSize: 500 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when threshold is not set', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryMempoolSize: 9999,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result).toBeNull();
  });

  it('returns mempool_not_recovered when recovery does not drain mempool backlog', () => {
    const result = detectCollapse(
      makeInputs({
        afterLoadMempoolSize: 500,
        recoveryMempoolSize: 500,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result?.reason).toBe('mempool_not_recovered');
    expect(result?.values.afterLoadMempoolSize).toBe(500);
    expect(result?.values.recoveryMempoolSize).toBe(500);
    expect(result?.values.drainedMempoolTxCount).toBe(0);
  });

  it('returns mempool_not_recovered when recovery mempool grows after load', () => {
    const result = detectCollapse(
      makeInputs({
        afterLoadMempoolSize: 400,
        recoveryMempoolSize: 700,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result?.reason).toBe('mempool_not_recovered');
    expect(result?.values.drainedMempoolTxCount).toBe(-300);
  });

  it('does not trigger non-draining check when no backlog existed after load', () => {
    const result = detectCollapse(
      makeInputs({
        afterLoadMempoolSize: 0,
        recoveryMempoolSize: 100,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when size is null (evidence incomplete)', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryMempoolSize: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryMempoolSize: 100 },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over tx_generator_failed', () => {
    const result = detectCollapse(
      makeInputs({
        recoveryMempoolSize: 600,
        txGeneratorExitCode: 1,
        stopConditions: { ...BASE_STOP_CONDITIONS, maxRecoveryMempoolSize: 100 },
      })
    );
    expect(result?.reason).toBe('mempool_not_recovered');
  });
});

// ---------------------------------------------------------------------------
// tx_generator_failed
// ---------------------------------------------------------------------------

describe('detectCollapse — tx_generator_failed', () => {
  it('returns tx_generator_failed when exit code is non-zero', () => {
    const result = detectCollapse(makeInputs({ txGeneratorExitCode: 1 }));
    expect(result?.reason).toBe('tx_generator_failed');
    expect(result?.values.exitCode).toBe(1);
  });

  it('includes the exit code in supporting values', () => {
    const result = detectCollapse(makeInputs({ txGeneratorExitCode: 127 }));
    expect(result?.values.exitCode).toBe(127);
  });

  it('does not trigger when exit code is 0', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: 0 }))).toBeNull();
  });

  it('does not trigger when exit code is null (killed by signal)', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: null }))).toBeNull();
  });

  it('does not trigger when txGeneratorExitCode is undefined (not tracked)', () => {
    expect(detectCollapse(makeInputs({ txGeneratorExitCode: undefined }))).toBeNull();
  });

  it('takes priority over commit_drain_below_threshold', () => {
    const result = detectCollapse(
      makeInputs({
        txGeneratorExitCode: 1,
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 100,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result?.reason).toBe('tx_generator_failed');
  });

  it('takes priority over useful_throughput_below_threshold', () => {
    const result = detectCollapse(
      makeInputs({
        txGeneratorExitCode: 1,
        mempoolAcceptedDelta: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result?.reason).toBe('tx_generator_failed');
  });
});

// ---------------------------------------------------------------------------
// commit_drain_below_threshold
// ---------------------------------------------------------------------------

describe('detectCollapse — commit_drain_below_threshold', () => {
  it('returns commit_drain_below_threshold when committed < accepted * ratio', () => {
    // accepted=1000, committed=800, ratio=800/1000=0.8, threshold=1.0 → fire
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 800,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result?.reason).toBe('commit_drain_below_threshold');
  });

  it('includes all supporting values', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 800,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result?.values.committedTxDelta).toBe(800);
    expect(result?.values.mempoolAcceptedDelta).toBe(1000);
    expect(result?.values.commitToAcceptedRatio).toBeCloseTo(0.8);
    expect(result?.values.minCommitToAcceptedRatio).toBe(1.0);
  });

  it('does not trigger when committed equals accepted (ratio meets threshold)', () => {
    // ratio=1.0, threshold=1.0 → not below
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 1000,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when committed exceeds accepted', () => {
    // node draining old backlog: committed > accepted → ratio > 1 → no collapse
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 1200,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when ratio meets a partial threshold', () => {
    // committed=800, accepted=1000, ratio=0.8, threshold=0.8 → not below
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 800,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 0.8 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when threshold is not set', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 100,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when mempoolAcceptedDelta is null', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: null,
        committedTxDelta: 100,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when committedTxDelta is null (evidence incomplete)', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when committedTxDelta is undefined (field absent)', () => {
    const inputs = makeInputs({
      mempoolAcceptedDelta: 1000,
      stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
    });
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    delete (inputs as any).committedTxDelta;
    expect(detectCollapse(inputs)).toBeNull();
  });

  it('does not trigger when mempoolAcceptedDelta is 0 (no load delivered)', () => {
    // Division by zero guard: skip check when no txs were accepted
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 0,
        committedTxDelta: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, minCommitToAcceptedRatio: 1.0 },
      })
    );
    expect(result).toBeNull();
  });

  it('takes priority over useful_throughput_below_threshold', () => {
    // Both thresholds set; drain check fires first
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 1000,
        committedTxDelta: 500,
        tierDurationSeconds: 60,
        targetTps: 100,
        stopConditions: {
          ...BASE_STOP_CONDITIONS,
          minCommitToAcceptedRatio: 1.0,
          minUsefulThroughputRatio: 0.9,
        },
      })
    );
    expect(result?.reason).toBe('commit_drain_below_threshold');
  });
});

// ---------------------------------------------------------------------------
// useful_throughput_below_threshold
// ---------------------------------------------------------------------------

describe('detectCollapse — useful_throughput_below_threshold', () => {
  it('returns useful_throughput_below_threshold when ratio is below minimum', () => {
    // mempoolAcceptedDelta=480, tierDurationSeconds=60 → observedTps=8, targetTps=10, ratio=0.8
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 480,
        tierDurationSeconds: 60,
        targetTps: 10,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result?.reason).toBe('useful_throughput_below_threshold');
  });

  it('includes all throughput supporting values', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 480,
        tierDurationSeconds: 60,
        targetTps: 10,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result?.values.mempoolAcceptedDelta).toBe(480);
    expect(result?.values.tierDurationSeconds).toBe(60);
    expect(result?.values.observedMempoolAcceptedTps).toBeCloseTo(8);
    expect(result?.values.targetTps).toBe(10);
    expect(result?.values.usefulThroughputRatio).toBeCloseTo(0.8);
    expect(result?.values.minUsefulThroughputRatio).toBe(0.9);
  });

  it('does not trigger when ratio meets the threshold exactly', () => {
    // delta=480, duration=60 → observedTps=8, targetTps=10, ratio=0.8; threshold=0.8
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 480,
        tierDurationSeconds: 60,
        targetTps: 10,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.8 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when threshold is not set', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 0,
        stopConditions: BASE_STOP_CONDITIONS,
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when mempoolAcceptedDelta is null', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: null,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when tierDurationSeconds is 0', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 0,
        tierDurationSeconds: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result).toBeNull();
  });

  it('does not trigger when targetTps is 0', () => {
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 0,
        targetTps: 0,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result).toBeNull();
  });

  it('uses mempoolAcceptedDelta/tierDurationSeconds for TPS, not enqueued total', () => {
    // At 100 TPS target with 6000 mempool-accepted over 60s → ratio=1.0 → no collapse
    const result = detectCollapse(
      makeInputs({
        mempoolAcceptedDelta: 6000,
        tierDurationSeconds: 60,
        targetTps: 100,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result).toBeNull();
  });

  it('is the lowest-priority reason', () => {
    // All other conditions are false/healthy — only throughput fires
    const result = detectCollapse(
      makeInputs({
        nodeUnavailable: false,
        prometheusUp: 1,
        txGeneratorExitCode: 0,
        commitmentFailuresDelta: 0,
        mergeFailuresDelta: 0,
        recoveryQueueSize: 0,
        recoveryMempoolSize: 0,
        mempoolAcceptedDelta: 0,
        tierDurationSeconds: 60,
        targetTps: 100,
        stopConditions: { ...BASE_STOP_CONDITIONS, minUsefulThroughputRatio: 0.9 },
      })
    );
    expect(result?.reason).toBe('useful_throughput_below_threshold');
  });
});
