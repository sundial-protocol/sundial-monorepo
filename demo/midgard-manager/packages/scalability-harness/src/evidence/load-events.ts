interface BaseEvent {
  ts: string;
  runId: string;
  event: string;
  tierIndex?: number;
  targetTps?: number;
}

export interface TierStartedEvent extends BaseEvent {
  event: 'tier_started';
  tierIndex: number;
  targetTps: number;
  durationSeconds: number;
  seed: string;
}

export interface TierStoppedEvent extends BaseEvent {
  event: 'tier_stopped';
  tierIndex: number;
  targetTps: number;
  elapsedMs: number;
  reason: 'completed' | 'stop_condition' | 'error';
}

export interface TxGeneratorStartedEvent extends BaseEvent {
  event: 'tx_generator_started';
  tierIndex: number;
  targetTps: number;
  pid?: number;
}

export interface TxGeneratorStoppedEvent extends BaseEvent {
  event: 'tx_generator_stopped';
  tierIndex: number;
  targetTps: number;
  exitCode: number | null;
  signal: string | null;
  errorSnippet?: string;
}

export interface TxGeneratorSubmissionAggregateEvent extends BaseEvent {
  event: 'tx_generator_submission_aggregate';
  tierIndex: number;
  targetTps: number;
  counters: {
    generated: number;
    attempted: number;
    submitted: number;
    rejected: number;
    node_unavailable: number;
    timed_out: number;
    error: number;
  };
  retries: {
    totalRetries: number;
    submissionsRetried: number;
    maxRetryCount: number;
  };
  submittedLatencyP95Ms: number | null;
}

export interface NodeProbeEvent extends BaseEvent {
  event: 'node_probe';
  tierIndex?: number;
  ok: boolean;
  latencyMs: number;
  statusCode?: number;
  errorMessage?: string;
}

export interface PrometheusSnapshotEvent extends BaseEvent {
  event: 'prometheus_snapshot';
  capture: 'before' | 'after_load' | 'after_recovery';
  tierIndex?: number;
  ok: boolean;
  metrics: Record<string, number>;
  errorMessage?: string;
}

export interface StopConditionEvent extends BaseEvent {
  event: 'stop_condition';
  tierIndex?: number;
  reason:
    | 'max_consecutive_probe_failures'
    | 'prometheus_down'
    | 'commitment_failure'
    | 'merge_failure'
    | 'recovery_queue_exceeded'
    | 'recovery_mempool_exceeded'
    | 'unsubmitted_backlog_growth'
    | 'commit_drain_below_threshold'
    | 'throughput_below_minimum';
  metricValues?: Record<string, number | boolean>;
}

export interface HarnessErrorEvent extends BaseEvent {
  event: 'harness_error';
  tierIndex?: number;
  errorMessage: string;
  stack?: string;
}

export type LoadEvent =
  | TierStartedEvent
  | TierStoppedEvent
  | TxGeneratorStartedEvent
  | TxGeneratorStoppedEvent
  | TxGeneratorSubmissionAggregateEvent
  | NodeProbeEvent
  | PrometheusSnapshotEvent
  | StopConditionEvent
  | HarnessErrorEvent;

export function makeEvent<T extends LoadEvent>(partial: Omit<T, 'ts'> & { ts?: string }): T {
  return {
    ts: new Date().toISOString(),
    ...partial,
  } as T;
}
