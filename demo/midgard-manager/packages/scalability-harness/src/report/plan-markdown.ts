import type { BenchmarkConclusion, FormalRunClassification } from '../analysis/analyzer.js';
import type { TierSummary } from '../analysis/tier-summary.js';
import type { ScalabilityScenario } from '../config/scenario.js';
import type { PlanManifest } from '../evidence/plan-artifacts.js';

export interface ScenarioRunRecord {
  scenarioIndex: number;
  scenarioPath: string;
  runId: string;
  runDir: string;
  // The scenario's maxTps, used as the declared target for this scenario.
  targetTps: number;
  scenario: ScalabilityScenario;
  tierSummaries: TierSummary[];
  conclusion: BenchmarkConclusion;
  skipped: boolean;
}

export type PlanClassification = 'Passed' | 'Passed with Observations' | 'Failed' | 'Blocked';

export interface PlanConclusion {
  classification: PlanClassification;
  completedScenarios: number;
  totalScenarios: number;
  highestPassedTargetTps: number | null;
  highestPassedRunId: string | null;
  stoppedEarlyAt: string | null;
  stoppedReason: string | null;
  harnessErrorOccurred: boolean;
}

export function buildPlanConclusion(
  records: ScenarioRunRecord[],
  harnessErrorOccurred: boolean
): PlanConclusion {
  const executed = records.filter((r) => !r.skipped);
  const skipped = records.filter((r) => r.skipped);

  let highestPassedTargetTps: number | null = null;
  let highestPassedRunId: string | null = null;
  let stoppedEarlyAt: string | null = null;
  let stoppedReason: string | null = null;

  for (const record of executed) {
    const c = record.conclusion.classification;
    if (c === 'Passed' || c === 'Passed with Observations') {
      if (record.targetTps > (highestPassedTargetTps ?? -1)) {
        highestPassedTargetTps = record.targetTps;
        highestPassedRunId = record.runId;
      }
    }
    if ((c === 'Failed' || c === 'Blocked') && stoppedEarlyAt === null) {
      stoppedEarlyAt = record.runId;
      stoppedReason =
        record.conclusion.classificationReasons[0] ?? record.conclusion.classification;
    }
  }

  let classification: PlanClassification;
  if (harnessErrorOccurred && executed.length === 0) {
    classification = 'Blocked';
  } else if (stoppedEarlyAt !== null || skipped.length > 0) {
    classification = 'Failed';
  } else {
    const allPassed = executed.every((r) => r.conclusion.classification === 'Passed');
    classification = allPassed ? 'Passed' : 'Passed with Observations';
  }

  return {
    classification,
    completedScenarios: executed.length,
    totalScenarios: records.length,
    highestPassedTargetTps,
    highestPassedRunId,
    stoppedEarlyAt,
    stoppedReason,
    harnessErrorOccurred,
  };
}

// --- Formatters ---

function n(value: number | null | undefined): string {
  return value === null || value === undefined ? 'n/a' : String(value);
}

function f2(value: number | null | undefined): string {
  return value === null || value === undefined ? 'n/a' : value.toFixed(2);
}

function s(value: string | null | undefined): string {
  return value ?? 'n/a';
}

function resultLabel(c: FormalRunClassification | 'skipped'): string {
  switch (c) {
    case 'Passed':
      return 'Passed ✅';
    case 'Passed with Observations':
      return 'Passed w/ Observations ✅';
    case 'Failed':
      return 'Failed 🚫';
    case 'Blocked':
      return 'Blocked 🚫';
    case 'skipped':
      return 'Skipped';
  }
}

function planLabel(c: PlanClassification): string {
  switch (c) {
    case 'Passed':
      return 'Passed ✅';
    case 'Passed with Observations':
      return 'Passed with Observations ✅';
    case 'Failed':
      return 'Failed 🚫';
    case 'Blocked':
      return 'Blocked 🚫';
  }
}

function mdTable(headers: string[], rows: string[][]): string {
  const sep = headers.map(() => '---');
  return [
    `| ${headers.join(' | ')} |`,
    `| ${sep.join(' | ')} |`,
    ...rows.map((row) => `| ${row.join(' | ')} |`),
  ].join('\n');
}

function sumNullable(values: Array<number | null>): number {
  return values.reduce((acc: number, b) => acc + (b ?? 0), 0);
}

function maxNullable(values: Array<number | null>): number | null {
  const nums = values.filter((v): v is number => v !== null);
  return nums.length === 0 ? null : Math.max(...nums);
}

// --- Report input ---

export interface PlanReportInput {
  manifest: PlanManifest;
  records: ScenarioRunRecord[];
  conclusion: PlanConclusion;
}

// --- Renderer ---

export function renderPlanReport(input: PlanReportInput): string {
  const { manifest, records, conclusion } = input;
  const executed = records.filter((r) => !r.skipped);
  const allTiers = executed.flatMap((r) => r.tierSummaries);

  const sections: string[] = [];

  // --- Header ---
  sections.push('# 📊 Scalability Plan Run Report');
  sections.push('');
  sections.push(
    '> **Scope:** This report covers a multi-scenario scalability plan executed by the\n' +
      '> Midgard scalability harness. Results reflect a single plan run and do not constitute\n' +
      '> a production capacity certification.'
  );
  sections.push('');

  // --- Plan metadata ---
  sections.push('## 📌 Plan Metadata');
  sections.push('');
  sections.push(
    mdTable(
      ['Field', 'Value'],
      [
        ['Plan ID', manifest.planId],
        ['Description', s(manifest.description)],
        ['Started at', manifest.startedAt],
        ['Git SHA', manifest.gitSha],
        ['Harness version', manifest.harnessVersion],
        ['Scenarios', String(manifest.scenarioPaths.length)],
        ['Host', `${manifest.host.hostname} (${manifest.host.platform}/${manifest.host.arch})`],
        ['CPUs', String(manifest.host.cpus)],
        ['Total memory', `${(manifest.host.totalMemoryBytes / 1024 / 1024 / 1024).toFixed(1)} GiB`],
      ]
    )
  );
  sections.push('');

  // --- Execution overview ---
  sections.push('## 🏃 Execution Overview');
  sections.push('');
  sections.push(
    mdTable(
      [
        '#',
        'Run ID',
        'Target TPS',
        'Result',
        'Tiers Passed',
        'Highest Passed TPS',
        'Collapse TPS',
        'Bottleneck',
      ],
      records.map((r) => {
        const c: FormalRunClassification | 'skipped' = r.skipped
          ? 'skipped'
          : r.conclusion.classification;
        const tiersPassed = r.skipped
          ? '—'
          : String(r.tierSummaries.filter((t) => t.result === 'completed').length);
        return [
          String(r.scenarioIndex + 1),
          r.runId,
          n(r.targetTps),
          resultLabel(c),
          tiersPassed,
          r.skipped ? '—' : n(r.conclusion.highestCompletedTargetTps),
          r.skipped ? '—' : n(r.conclusion.firstCollapsedTargetTps),
          r.skipped ? '—' : r.conclusion.primaryBottleneck,
        ];
      })
    )
  );
  sections.push('');

  // --- Throughput progression ---
  sections.push('## ⚡ Throughput Progression');
  sections.push('');
  sections.push(
    'Average observed transaction rates across completed tiers per scenario.\n' +
      'Values are derived from Prometheus counter deltas over the full tier window (load + recovery).'
  );
  sections.push('');

  if (executed.length === 0) {
    sections.push('_No scenarios executed._');
  } else {
    sections.push(
      mdTable(
        ['Scenario', 'Target TPS', 'Enqueued tx/s', 'Durable Accepted tx/s', 'Committed tx/s'],
        executed.map((r) => {
          const completed = r.tierSummaries.filter((t) => t.result === 'completed');
          const avg = (field: keyof TierSummary): number | null => {
            if (completed.length === 0) return null;
            const vals = completed
              .map((t) => t[field] as number | null)
              .filter((v): v is number => v !== null);
            return vals.length === 0 ? null : vals.reduce((a, b) => a + b, 0) / vals.length;
          };
          return [
            r.runId,
            n(r.targetTps),
            f2(avg('observedEnqueuedTps')),
            f2(avg('observedMempoolAcceptedTps')),
            f2(avg('observedCommittedTps')),
          ];
        })
      )
    );
  }
  sections.push('');

  // --- Transaction totals ---
  sections.push('## 📈 Transaction Totals');
  sections.push('');
  sections.push(
    'Aggregate counts across all executed scenarios.\n' +
      'Node-side Prometheus metrics are the authoritative source.'
  );
  sections.push('');
  const totalEnqueued = sumNullable(allTiers.map((t) => t.enqueuedDelta));
  const totalAccepted = sumNullable(allTiers.map((t) => t.mempoolAcceptedDelta));
  const totalCommitted = sumNullable(allTiers.map((t) => t.committedTxDelta));
  const totalBlocks = sumNullable(allTiers.map((t) => t.committedBlockDelta));
  const totalRejected = sumNullable(allTiers.map((t) => t.rejectedDelta));
  sections.push(
    mdTable(
      ['Metric', 'Total'],
      [
        ['Enqueued transactions', n(totalEnqueued)],
        ['Durable mempool accepted transactions', n(totalAccepted)],
        ['Committed transactions', n(totalCommitted)],
        ['Committed blocks', n(totalBlocks)],
        ['Rejected submissions', n(totalRejected)],
      ]
    )
  );
  sections.push('');

  // --- Peak metrics per scenario ---
  sections.push('## 📦 Peak Metrics per Scenario');
  sections.push('');
  if (executed.length === 0) {
    sections.push('_No scenarios executed._');
  } else {
    sections.push(
      mdTable(
        [
          'Scenario',
          'Peak Queue Size',
          'Peak Mempool Size',
          'Commitment Failures',
          'Merge Failures',
        ],
        executed.map((r) => {
          const peakQueue = maxNullable(r.tierSummaries.map((t) => t.peakQueueSize));
          const peakMempool = maxNullable(r.tierSummaries.map((t) => t.peakMempoolSize));
          const commitFailures = sumNullable(r.tierSummaries.map((t) => t.commitmentFailureDelta));
          const mergeFailures = sumNullable(r.tierSummaries.map((t) => t.mergeFailureDelta));
          return [r.runId, n(peakQueue), n(peakMempool), n(commitFailures), n(mergeFailures)];
        })
      )
    );
  }
  sections.push('');

  // --- Inclusion latency ---
  sections.push('## ⏱️ Inclusion Latency (p95) per Scenario');
  sections.push('');
  sections.push(
    'Estimated p95 mempool-accepted-to-committed inclusion latency per §7 of the test plan.\n' +
      'Target: ≤ 20,000 ms. Values are heuristic estimates via Prometheus counter cohort alignment.'
  );
  sections.push('');
  if (executed.length === 0) {
    sections.push('_No scenarios executed._');
  } else {
    sections.push(
      mdTable(
        ['Scenario', 'p50 (ms)', 'p95 (ms)', 'p99 (ms)', 'Confidence'],
        executed.map((r) => {
          const p50 = maxNullable(r.tierSummaries.map((t) => t.acceptedToCommittedLatencyP50Ms));
          const p95 = maxNullable(r.tierSummaries.map((t) => t.acceptedToCommittedLatencyP95Ms));
          const p99 = maxNullable(r.tierSummaries.map((t) => t.acceptedToCommittedLatencyP99Ms));
          const confidence =
            r.tierSummaries.find((t) => t.acceptedToCommittedLatencyConfidence !== null)
              ?.acceptedToCommittedLatencyConfidence ?? null;
          return [r.runId, f2(p50), f2(p95), f2(p99), s(confidence)];
        })
      )
    );
  }
  sections.push('');

  // --- Collapse analysis ---
  sections.push('## 💥 Collapse Analysis');
  sections.push('');
  const collapsedRecords = executed.filter((r) => r.conclusion.firstCollapsedTier !== null);
  if (collapsedRecords.length === 0) {
    sections.push('No collapse detected across all executed scenarios.');
  } else {
    for (const r of collapsedRecords) {
      sections.push(
        `**${r.runId}** collapsed at tier ${r.conclusion.firstCollapsedTier} (${r.conclusion.firstCollapsedTargetTps} TPS).`
      );
      sections.push('');
      sections.push(`- **Primary bottleneck:** ${r.conclusion.primaryBottleneck}`);
      for (const note of r.conclusion.notes) {
        sections.push(`- ${note}`);
      }
      sections.push('');
    }
  }

  // --- Final disposition ---
  sections.push('## 🏁 Final Disposition');
  sections.push('');
  sections.push(`**Overall plan result: ${planLabel(conclusion.classification)}**`);
  sections.push('');
  sections.push(
    mdTable(
      ['Field', 'Value'],
      [
        ['Plan classification', planLabel(conclusion.classification)],
        ['Scenarios executed', `${conclusion.completedScenarios} / ${conclusion.totalScenarios}`],
        ['Highest passed target TPS', n(conclusion.highestPassedTargetTps)],
        ['Highest passed scenario', s(conclusion.highestPassedRunId)],
        ['Stopped early at', s(conclusion.stoppedEarlyAt)],
        ['Stop reason', s(conclusion.stoppedReason)],
      ]
    )
  );
  sections.push('');

  if (conclusion.classification === 'Passed') {
    sections.push('All scenarios completed within configured acceptance criteria.');
  } else if (conclusion.classification === 'Passed with Observations') {
    sections.push(
      'All scenarios completed. One or more had non-blocking observations — review per-scenario reports for details.'
    );
  } else if (conclusion.classification === 'Failed') {
    sections.push(
      `Plan stopped after ${conclusion.completedScenarios} of ${conclusion.totalScenarios} scenarios. ` +
        `Highest passed: ${conclusion.highestPassedRunId ?? 'none'} ` +
        `(${conclusion.highestPassedTargetTps ?? 0} TPS).`
    );
    if (conclusion.stoppedReason !== null) {
      sections.push('');
      sections.push(`Stop reason: ${conclusion.stoppedReason}`);
    }
  } else {
    sections.push('Plan was blocked before any scenario could complete.');
  }
  sections.push('');

  // --- Artifact index ---
  sections.push('## 📂 Artifact Index');
  sections.push('');
  sections.push('Per-scenario evidence:');
  sections.push('');
  for (const r of records) {
    const status: FormalRunClassification | 'skipped' = r.skipped
      ? 'skipped'
      : r.conclusion.classification;
    sections.push(
      `- \`${r.runDir}\` — **${r.runId}** (${r.targetTps} TPS) — ${resultLabel(status)}`
    );
  }
  sections.push('');
  sections.push('Plan-level artifacts (this directory):');
  sections.push('- `plan-manifest.json` — plan metadata and host environment');
  sections.push('- `plan-summary.json` — machine-readable plan conclusion');
  sections.push('- `plan-report.md` — this document');

  return sections.join('\n');
}
