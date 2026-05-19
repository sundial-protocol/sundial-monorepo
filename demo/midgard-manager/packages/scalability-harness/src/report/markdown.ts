import type { BenchmarkConclusion, ClassificationCheckEvidence } from '../analysis/analyzer.js';
import type { TierSummary } from '../analysis/tier-summary.js';
import type { ScalabilityScenario } from '../config/scenario.js';
import type { RunManifest } from '../evidence/artifacts.js';
import type { LokiTierCapture } from '../evidence/loki.js';
import type { TempoTierCapture } from '../evidence/tempo.js';

export interface ReportInput {
  manifest: RunManifest;
  scenario: ScalabilityScenario;
  tierSummaries: TierSummary[];
  conclusion: BenchmarkConclusion;
  /** Filenames (not full paths) present in the artifact run directory. */
  artifactFiles: string[];
  /** Per-tier Loki log captures, present when lokiEndpoint was configured. */
  lokiCaptures?: LokiTierCapture[];
  /** Per-tier Tempo trace captures, present when tempoEndpoint was configured. */
  tempoCaptures?: TempoTierCapture[];
}

// --- Formatters ---

/** Formats a nullable integer count; null → 'n/a'. */
function n(value: number | null | undefined): string {
  return value === null || value === undefined ? 'n/a' : String(value);
}

/** Formats a nullable float to 2 decimal places; null → 'n/a'. */
function f2(value: number | null | undefined): string {
  return value === null || value === undefined ? 'n/a' : value.toFixed(2);
}

/** Formats a nullable string; undefined → 'n/a'. */
function s(value: string | undefined): string {
  return value ?? 'n/a';
}

function pct(value: number | null | undefined): string {
  return value === null || value === undefined ? 'n/a' : `${(value * 100).toFixed(1)}%`;
}

// --- Markdown table builder ---

function mdTable(headers: string[], rows: string[][]): string {
  const sep = headers.map(() => '---');
  return [
    `| ${headers.join(' | ')} |`,
    `| ${sep.join(' | ')} |`,
    ...rows.map((row) => `| ${row.join(' | ')} |`),
  ].join('\n');
}

// --- Section renderers ---

function renderHeader(): string {
  return [
    '# Scalability Baseline Report',
    '',
    '> **Scope:** This report measures the current behavior of the Midgard node under synthetic',
    '> load. Results reflect a single benchmark run executed without remediation and do not',
    '> claim or imply production scalability.',
  ].join('\n');
}

function renderRunMetadata(manifest: RunManifest): string {
  const gb = (manifest.host.totalMemoryBytes / 1_073_741_824).toFixed(1);
  const hostInfo = `${manifest.host.hostname} (${manifest.host.platform}/${manifest.host.arch}, ${manifest.host.cpus} CPUs, ${gb} GB)`;
  const rows: string[][] = [
    ['Run ID', manifest.runId],
    ['Started At', manifest.startedAt],
    ['Git SHA', manifest.gitSha],
    ['Node Endpoint', manifest.nodeEndpoint],
    ['Prometheus Endpoint', manifest.prometheusEndpoint],
    ['L1 Provider Mode', manifest.l1ProviderMode ?? 'not specified'],
    ['Wallet Mode', manifest.walletMode ?? 'not specified'],
    ...(manifest.walletProvisioningNote
      ? [['Wallet Provisioning Note', manifest.walletProvisioningNote]]
      : []),
    ['Harness Version', manifest.harnessVersion],
    ['Replay Corpus Path', manifest.replayCorpusPath ?? 'n/a'],
    ['Replay Corpus SHA256', manifest.replayCorpusSha256 ?? 'n/a'],
    ['Host', hostInfo],
  ];

  return ['## Run Metadata', '', mdTable(['Field', 'Value'], rows)].join('\n');
}

function renderScenario(scenario: ScalabilityScenario): string {
  const txLabel =
    scenario.transactionType === 'mixed' && scenario.oneToOneRatio !== undefined
      ? `mixed (${scenario.oneToOneRatio}% one-to-one)`
      : scenario.transactionType;

  const sc = scenario.stopConditions;

  const rows: string[][] = [
    ['Transaction Type', txLabel],
    ['L1 Provider Mode', scenario.l1ProviderMode ?? 'not specified'],
    ['Wallet Mode', scenario.walletMode ?? 'not specified'],
    ...(scenario.walletProvisioningNote
      ? [['Wallet Provisioning Note', scenario.walletProvisioningNote]]
      : []),
    ['Start TPS', String(scenario.startTps)],
    ['Max TPS', String(scenario.maxTps)],
    ['Step Multiplier', String(scenario.stepMultiplier)],
    ['Tier Duration', `${scenario.tierDurationSeconds} s`],
    ['Recovery Duration', `${scenario.recoverySeconds} s`],
    ['Batch Size', String(scenario.batchSize)],
    ['Concurrency', String(scenario.concurrency)],
    ['Retry Attempts', String(scenario.retryAttempts)],
    ['Retry Delay', `${scenario.retryDelayMs} ms`],
    ['Request Events Mode', scenario.requestEvents ?? 'off'],
    ['Seed', scenario.seed],
    ['Replay Corpus Path', scenario.replayCorpusPath ?? 'n/a'],
    ['Max Consecutive Probe Failures', String(sc.maxConsecutiveNodeProbeFailures)],
    ['Stop On Prometheus Down', String(sc.stopOnPrometheusDown)],
    ['Stop On Commitment Failure', String(sc.stopOnCommitmentFailure)],
    ['Stop On Merge Failure', String(sc.stopOnMergeFailure)],
    ...(sc.maxRecoveryQueueSize !== undefined
      ? [['Max Recovery Queue Size', String(sc.maxRecoveryQueueSize)]]
      : []),
    ...(sc.maxRecoveryMempoolSize !== undefined
      ? [['Max Recovery Mempool Size', String(sc.maxRecoveryMempoolSize)]]
      : []),
    ...(sc.minUsefulThroughputRatio !== undefined
      ? [['Min Useful Throughput Ratio', String(sc.minUsefulThroughputRatio)]]
      : []),
  ];

  return ['## Scenario', '', mdTable(['Parameter', 'Value'], rows)].join('\n');
}

function renderTierResultsTable(tiers: TierSummary[]): string {
  if (tiers.length === 0) {
    return '## Tier Results\n\n_No tiers were executed._';
  }

  const headers = [
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

  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    t.result,
    n(t.enqueuedDelta),
    n(t.mempoolAcceptedDelta),
    n(t.committedTxDelta),
    n(t.submittedBlockDelta),
    n(t.peakQueueSize),
    n(t.peakMempoolSize),
    n(t.commitmentFailureDelta),
    n(t.mergeFailureDelta),
    s(t.collapseReason),
  ]);

  return ['## Tier Results', '', mdTable(headers, rows)].join('\n');
}

function renderCollapsePoint(conclusion: BenchmarkConclusion): string {
  const lines: string[] = ['## Collapse Point', ''];

  if (conclusion.firstCollapsedTier === null) {
    lines.push('No collapse detected. All tiers completed or ran with incomplete evidence.');
  } else {
    lines.push(
      `**First collapsed tier:** ${conclusion.firstCollapsedTier} at ${conclusion.firstCollapsedTargetTps} TPS`
    );
  }

  if (conclusion.highestCompletedTier !== null) {
    lines.push(
      `**Highest completed tier:** ${conclusion.highestCompletedTier} at ${conclusion.highestCompletedTargetTps} TPS`
    );
  } else {
    lines.push('**Highest completed tier:** none');
  }

  return lines.join('\n');
}

function renderClassificationLabel(classification: BenchmarkConclusion['classification']): string {
  switch (classification) {
    case 'Passed':
      return 'Passed';
    case 'Passed with Observations':
      return 'Passed with Observations';
    case 'Failed':
      return 'Failed';
    case 'Blocked':
      return 'Blocked';
  }
}

function renderViolatedChecksTable(violatedChecks: ClassificationCheckEvidence[]): string {
  if (violatedChecks.length === 0) {
    return '_No violated checks._';
  }

  const headers = ['Check', 'Severity', 'Expected', 'Observed', 'Details'];
  const rows = violatedChecks.map((check) => [
    check.name,
    check.severity,
    check.expected,
    check.observed,
    check.details,
  ]);

  return mdTable(headers, rows);
}

function renderFormalRunClassification(conclusion: BenchmarkConclusion): string {
  const lines: string[] = ['## Formal Run Classification', ''];
  lines.push(`**Classification:** ${renderClassificationLabel(conclusion.classification)}`);
  lines.push('');
  lines.push(`- Policy checks evaluated: ${conclusion.criteriaChecks.length}`);
  lines.push(`- Violated checks: ${conclusion.violatedChecks.length}`);
  lines.push(
    `- Failure-severity violations: ${conclusion.violatedChecks.filter((c) => c.severity === 'failure').length}`
  );
  lines.push(
    `- Observation-severity violations: ${conclusion.violatedChecks.filter((c) => c.severity === 'observation').length}`
  );

  if (conclusion.classificationReasons.length > 0) {
    lines.push('', '**Reasons:**', '');
    for (const reason of conclusion.classificationReasons) {
      lines.push(`- ${reason}`);
    }
  }

  lines.push('', '**Violated checks:**', '', renderViolatedChecksTable(conclusion.violatedChecks));
  return lines.join('\n');
}

function renderThroughputStageDeltas(tiers: TierSummary[]): string {
  const prose = [
    'The columns below reflect three distinct pipeline stages:',
    '',
    '- **Enqueued TPS** (`tx_submissions_enqueued_total`) — rate of transactions',
    '  accepted at the HTTP boundary into the in-memory queue.',
    '  This is not a durability signal; it only reflects HTTP-level acceptance.',
    '- **Mempool Accepted TPS** (`tx_submissions_mempool_accepted_total`) — rate',
    '  of transactions durably written to MempoolDB.',
    '  This is the first durable acceptance boundary and is distinct from the enqueued count.',
    '- **Committed TPS** (`commit_block_tx_count_total`) — rate of transactions',
    '  included in a committed block and rooted in the on-chain state commitment.',
    '- Deprecated alias: `tx_submissions_accepted_total` is legacy and must not',
    '  be used for new reports.',
    '',
    'Gaps between adjacent columns identify where the pipeline loses throughput.',
  ].join('\n');

  if (tiers.length === 0) {
    return `## Throughput Stage Deltas\n\n${prose}\n\n_No tier data._`;
  }

  const headers = ['Tier', 'Target TPS', 'Enqueued TPS', 'Mempool Accepted TPS', 'Committed TPS'];
  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    f2(t.observedEnqueuedTps),
    f2(t.observedMempoolAcceptedTps),
    f2(t.observedCommittedTps),
  ]);

  return ['## Throughput Stage Deltas', '', prose, '', mdTable(headers, rows)].join('\n');
}

function renderClientSubmissionEvidence(tiers: TierSummary[]): string {
  const prose = [
    'Client-side submission evidence is aggregated per tier/window and does not',
    'require per-request JSONL.',
    '',
    '- Outcomes are tracked as submitted, rejected, node_unavailable, and error.',
    '- Retry evidence reports total retries and count of retried submissions.',
    '- Submitted-latency p95 is derived from bounded histogram buckets.',
  ].join('\n');

  if (tiers.length === 0) {
    return `## Client Submission Evidence\n\n${prose}\n\n_No tier data._`;
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Submitted',
    'Rejected',
    'Node Unavailable',
    'Error',
    'Total Retries',
    'Retried Submissions',
    'Submitted Latency p95 (ms)',
  ];
  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    n(t.clientSubmittedCount),
    n(t.clientRejectedCount),
    n(t.clientNodeUnavailableCount),
    n(t.clientErrorCount),
    n(t.clientTotalRetries),
    n(t.clientRetriedSubmissionCount),
    n(t.clientSubmittedLatencyP95Ms),
  ]);

  return ['## Client Submission Evidence', '', prose, '', mdTable(headers, rows)].join('\n');
}

function renderAcceptedToCommittedLatencyEvidence(tiers: TierSummary[]): string {
  const prose = [
    'Accepted-to-committed latency is estimated from Prometheus counters using',
    'cohort alignment (`cohort_counter_alignment_v1`):',
    '',
    '- Bucket accepted transactions by scrape interval from',
    '  `tx_submissions_mempool_accepted_total` deltas.',
    '- For each accepted cohort, find when cumulative committed count',
    '  (`commit_block_tx_count_total`) catches up.',
    '- Compute weighted p50/p95/p99 latency across resolved cohorts.',
    '',
    'Confidence notes indicate scrape-resolution limits and unresolved cohorts',
    '(right-censoring at tier window end).',
  ].join('\n');

  if (tiers.length === 0) {
    return `## Accepted-to-Committed Latency Evidence\n\n${prose}\n\n_No tier data._`;
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Method',
    'Confidence',
    'Resolved Tx Ratio',
    'Accepted→Committed p50 (ms)',
    'Accepted→Committed p95 (ms)',
    'Accepted→Committed p99 (ms)',
  ];
  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    s(t.acceptedToCommittedLatencyMethod ?? undefined),
    s(t.acceptedToCommittedLatencyConfidence ?? undefined),
    pct(t.acceptedToCommittedResolvedRatio),
    n(t.acceptedToCommittedLatencyP50Ms),
    n(t.acceptedToCommittedLatencyP95Ms),
    n(t.acceptedToCommittedLatencyP99Ms),
  ]);

  const notes: string[] = [];
  for (const tier of tiers) {
    if (tier.acceptedToCommittedLatencyConfidenceNotes.length === 0) {
      continue;
    }
    notes.push(
      `- Tier ${tier.tierIndex}: ${tier.acceptedToCommittedLatencyConfidenceNotes.join(' ')}`
    );
  }

  return [
    '## Accepted-to-Committed Latency Evidence',
    '',
    prose,
    '',
    mdTable(headers, rows),
    '',
    '**Confidence Notes:**',
    '',
    notes.length > 0 ? notes.join('\n') : '- No confidence notes were recorded.',
  ].join('\n');
}

function renderQueueAndMempoolBehavior(tiers: TierSummary[]): string {
  if (tiers.length === 0) {
    return '## Queue and Mempool Behavior\n\n_No tier data._';
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Peak Queue',
    'Final Queue (after recovery)',
    'Peak Mempool',
    'Final Mempool (after recovery)',
  ];

  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    n(t.peakQueueSize),
    n(t.finalQueueSizeAfterRecovery),
    n(t.peakMempoolSize),
    n(t.finalMempoolSizeAfterRecovery),
  ]);

  return ['## Queue and Mempool Behavior', '', mdTable(headers, rows)].join('\n');
}

function renderCommitSubmitMergeProgress(tiers: TierSummary[]): string {
  if (tiers.length === 0) {
    return '## Commit/Submit/Merge Progress\n\n_No tier data._';
  }

  const prose = [
    'L1 commitment fee fields are derived as follows:',
    '- `L1 Fees Δ` from `l1_commitment_fees_lovelace_total` counter delta over load phase.',
    '- `Last L1 Fee` from `l1_commitment_fee_lovelace_last` instant value at load stop.',
    '- `L1 Fee / Committed L2 Tx` = `L1 Fees Δ / Committed Tx Δ` when `Committed Tx Δ > 0`.',
  ].join('\n');

  const headers = [
    'Tier',
    'Target TPS',
    'Committed Blocks Δ',
    'Submitted Blocks Δ',
    'Merge Failures Δ',
    'Commit Failures Δ',
    'L1 Fees Δ (lovelace)',
    'Last L1 Fee (lovelace)',
    'L1 Fee / Committed L2 Tx (lovelace)',
  ];

  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    n(t.committedBlockDelta),
    n(t.submittedBlockDelta),
    n(t.mergeFailureDelta),
    n(t.commitmentFailureDelta),
    n(t.l1CommitmentFeesDeltaLovelace),
    n(t.l1CommitmentFeeLastLovelace),
    f2(t.l1FeePerCommittedL2TxLovelace),
  ]);

  return ['## Commit/Submit/Merge Progress', '', prose, '', mdTable(headers, rows)].join('\n');
}

function renderFailureSignals(tiers: TierSummary[]): string {
  if (tiers.length === 0) {
    return '## Failure Signals\n\n_No tier data._';
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Commit Failures Δ',
    'Merge Failures Δ',
    'Rejected Δ',
    'Processing Failed Δ',
  ];

  const rows = tiers.map((t) => [
    String(t.tierIndex),
    String(t.targetTps),
    n(t.commitmentFailureDelta),
    n(t.mergeFailureDelta),
    n(t.rejectedDelta),
    n(t.processingFailedDelta),
  ]);

  return ['## Failure Signals', '', mdTable(headers, rows)].join('\n');
}

function renderPrimaryBottleneckHypothesis(conclusion: BenchmarkConclusion): string {
  const lines: string[] = [
    '## Primary Bottleneck Hypothesis',
    '',
    `**Identified bottleneck:** ${conclusion.primaryBottleneck}`,
  ];

  if (conclusion.notes.length > 0) {
    lines.push('', '**Notes:**', '');
    for (const note of conclusion.notes) {
      lines.push(`- ${note}`);
    }
  }

  return lines.join('\n');
}

function renderLokiEvidence(captures: LokiTierCapture[] | undefined): string | null {
  if (captures === undefined || captures.length === 0) {
    return null;
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Window',
    'Query',
    'Streams',
    'Entries',
    'Truncated',
    'Error',
  ];
  const rows = captures.map((c) => [
    String(c.tierIndex),
    String(c.targetTps),
    `${c.startedAt.slice(0, 19)}Z → ${c.recoveryStoppedAt.slice(0, 19)}Z`,
    `\`${c.query}\``,
    c.result !== null ? String(c.result.streams.length) : 'n/a',
    c.result !== null ? String(c.result.totalEntries) : 'n/a',
    c.result !== null ? String(c.result.truncated) : 'n/a',
    c.error !== null ? c.error.slice(0, 80) : '',
  ]);

  return [
    '## Log Evidence (Loki)',
    '',
    'Per-tier log capture from Loki over the full tier window (load phase + recovery).',
    'Full log streams are in `loki-captures.json`.',
    '',
    mdTable(headers, rows),
  ].join('\n');
}

function renderTempoEvidence(captures: TempoTierCapture[] | undefined): string | null {
  if (captures === undefined || captures.length === 0) {
    return null;
  }

  const headers = [
    'Tier',
    'Target TPS',
    'Window',
    'Service',
    'Traces',
    'Inspected',
    'Truncated',
    'Error',
  ];
  const rows = captures.map((c) => [
    String(c.tierIndex),
    String(c.targetTps),
    `${c.startedAt.slice(0, 19)}Z → ${c.recoveryStoppedAt.slice(0, 19)}Z`,
    c.serviceName,
    c.result !== null ? String(c.result.traces.length) : 'n/a',
    c.result !== null
      ? c.result.inspectedTraces !== null
        ? String(c.result.inspectedTraces)
        : 'n/a'
      : 'n/a',
    c.result !== null ? String(c.result.truncated) : 'n/a',
    c.error !== null ? c.error.slice(0, 80) : '',
  ]);

  return [
    '## Trace Evidence (Tempo)',
    '',
    'Per-tier trace capture from Tempo over the full tier window (load phase + recovery).',
    'Full trace summaries are in `tempo-captures.json`.',
    '',
    mdTable(headers, rows),
  ].join('\n');
}

function renderArtifactIndex(files: string[]): string {
  if (files.length === 0) {
    return '## Artifact Index\n\n_No artifact files recorded._';
  }

  const list = files.map((f) => `- \`${f}\``).join('\n');
  return ['## Artifact Index', '', list].join('\n');
}

function renderLimitations(): string {
  return [
    '## Limitations',
    '',
    '- This run measures current behavior without remediation.',
    '  No performance tuning, protocol changes, or infrastructure adjustments were applied.',
    '- Results reflect a single run under the configured synthetic load profile.',
    '  They may not reproduce identically under different hardware, network, or node state.',
    '- This report does not claim or imply production scalability.',
    '  Observed TPS figures are specific to the test configuration and should not be extrapolated.',
    '- Enqueued TPS, mempool accepted TPS, and committed TPS are distinct pipeline boundaries.',
    '  Conflating them overstates actual throughput.',
    '- The legacy alias `tx_submissions_accepted_total` is intentionally excluded',
    '  from this report to keep enqueue and durable-acceptance boundaries explicit.',
    '- Bottleneck attribution is derived from counter deltas over the tier window.',
    '  Scrape jitter, slow counters, or incomplete Prometheus data may reduce accuracy.',
    '- Accepted-to-committed latency is a cohort estimate from counter alignment,',
    '  not per-transaction tracing. It is bounded by Prometheus scrape cadence and window coverage.',
  ].join('\n');
}

export function renderReport(input: ReportInput): string {
  const optionalSections: string[] = [];

  const lokiSection = renderLokiEvidence(input.lokiCaptures);
  if (lokiSection !== null) {
    optionalSections.push(lokiSection);
  }

  const tempoSection = renderTempoEvidence(input.tempoCaptures);
  if (tempoSection !== null) {
    optionalSections.push(tempoSection);
  }

  const sections = [
    renderHeader(),
    renderRunMetadata(input.manifest),
    renderScenario(input.scenario),
    renderTierResultsTable(input.tierSummaries),
    renderFormalRunClassification(input.conclusion),
    renderCollapsePoint(input.conclusion),
    renderThroughputStageDeltas(input.tierSummaries),
    renderClientSubmissionEvidence(input.tierSummaries),
    renderAcceptedToCommittedLatencyEvidence(input.tierSummaries),
    renderQueueAndMempoolBehavior(input.tierSummaries),
    renderCommitSubmitMergeProgress(input.tierSummaries),
    renderFailureSignals(input.tierSummaries),
    renderPrimaryBottleneckHypothesis(input.conclusion),
    ...optionalSections,
    renderArtifactIndex(input.artifactFiles),
    renderLimitations(),
  ];

  return sections.join('\n\n') + '\n';
}
