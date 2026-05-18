export type { CollapseInputs, CollapseReason, CollapseResult } from './analysis/collapse.js';
export { detectCollapse } from './analysis/collapse.js';
export type { ScalabilityScenario, StopConditions } from './config/scenario.js';
export { ScenarioValidationError, validateScenario } from './config/scenario.js';
export type { LoadTier } from './config/tiers.js';
export { generateTiers } from './config/tiers.js';
export type { RunManifest, TierSummary } from './evidence/artifacts.js';
export { ArtifactWriter } from './evidence/artifacts.js';
export type {
  HarnessErrorEvent,
  LoadEvent,
  NodeProbeEvent,
  PrometheusSnapshotEvent,
  StopConditionEvent,
  TierStartedEvent,
  TierStoppedEvent,
  TxGeneratorStartedEvent,
  TxGeneratorStoppedEvent,
  TxGeneratorSubmissionAggregateEvent,
} from './evidence/load-events.js';
export { makeEvent } from './evidence/load-events.js';
export type {
  CadvisorMetric,
  MatrixSample,
  MetricSnapshot,
  NodeMetric,
  Fetcher as PrometheusFetcher,
  PrometheusMatrixResult,
  PrometheusSamples,
  PrometheusVectorResult,
  VectorSample,
} from './metrics/prometheus.js';
export {
  CADVISOR_METRICS,
  flattenToScalars,
  NODE_METRICS,
  PrometheusClient,
  PrometheusQueryError,
  snapshotNodeMetrics,
} from './metrics/prometheus.js';
export type {
  CounterDelta,
  GaugeSummary,
  PrometheusSeries,
  TierMetricWindow,
  TierWindowSummary,
} from './metrics/window.js';
export {
  collectTierWindow,
  computeCounterDelta,
  computeGaugeFinal,
  computeGaugePeak,
  extractScalar,
  isCounter,
  RANGE_STEP_SECONDS,
  summarizeTierWindow,
} from './metrics/window.js';
export type {
  HostResourceCollector,
  HostResourcePhaseEvidence,
  HostResourceSnapshot,
  LoadDriverResourceEvidence,
  LoadDriverSaturationFlags,
} from './runner/host-resources.js';
export {
  buildLoadDriverResourceEvidence,
  createHostResourceCollector,
} from './runner/host-resources.js';
export type { MetricStopCondition, TierRunOptions, TierRunResult } from './runner/load-runner.js';
export { checkMetricStopConditions, runTier } from './runner/load-runner.js';
export type { Fetcher, ProbeConfig, ProbeLoopResult, ProbeResult } from './runner/node-probe.js';
export {
  PROBE_INTERVAL_MS,
  PROBE_TIMEOUT_MS,
  probeNode,
  runProbeLoop,
} from './runner/node-probe.js';
export type {
  ExecutionReadinessPreflightResult,
  PreflightCheckName,
  PreflightCheckResult,
  PreflightDependencies,
  RunExecutionReadinessPreflightOptions,
  TxGeneratorInvocabilityResult,
  TxGeneratorInvoker,
} from './runner/preflight.js';
export { PREFLIGHT_CHECK_NAMES, runExecutionReadinessPreflight } from './runner/preflight.js';
export type {
  GeneratorSettings,
  ProcessSpawner,
  RequestEventsMode,
  RunnerOptions,
  SubmissionAggregate,
  SubmissionLatencyHistogram,
  TxGeneratorHandle,
  TxGeneratorResult,
} from './runner/tx-generator.js';
export {
  computeSettings,
  defaultSpawner,
  REQUEST_EVENT_MODES,
  SIGINT_GRACE_MS,
  SIGTERM_GRACE_MS,
  startTxGenerator,
  stopProcess,
} from './runner/tx-generator.js';

// Exported as AnalysisTierSummary to avoid conflict with evidence/artifacts TierSummary.
export type { AcceptedToCommittedLatencyEstimate } from './analysis/accepted-to-committed-latency.js';
export {
  ACCEPTED_COUNTER_QUERY,
  ACCEPTED_TO_COMMITTED_METHOD,
  COMMITTED_COUNTER_QUERY,
  estimateAcceptedToCommittedLatency,
} from './analysis/accepted-to-committed-latency.js';
export type { BenchmarkConclusion } from './analysis/analyzer.js';
export { analyzeTiers } from './analysis/analyzer.js';
export type {
  TierSummary as AnalysisTierSummary,
  TierSummaryInput,
} from './analysis/tier-summary.js';
export { buildTierSummary } from './analysis/tier-summary.js';
export type { ReportInput } from './report/markdown.js';
export { renderReport } from './report/markdown.js';
