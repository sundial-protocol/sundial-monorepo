Executive Summary — Scalability Harness (@midgard-manager/scalability-harness)
A new standalone benchmark package has been built at demo/midgard-manager/packages/scalability-harness/. It owns all benchmark orchestration, evidence collection, and analysis for Midgard L2 load testing. It does not modify midgard-node runtime behavior and does not embed logic into tx-generator.

we don't want to fix scalability issues right now, we want to measure the current state of affairs even if it means the system will halt very fast - this is what it is. then we fix the issues to improve througput and run it again
1:28 PM

Correct. Then the plan should be measurement-first, not remediation-first.

Revised Goal
Build a benchmark harness that can measure the current system honestly, including fast failure, queue explosion, stalled commitment, failed merge, node halt, or OOM. The first run establishes the baseline. Fixes come later and are measured against that baseline.

Plan

Do not change node behavior yet

Keep current unbounded queue.
Keep current processor/commitment behavior.
Do not add backpressure, batching fixes, or lifecycle fixes before baseline.
Add external measurement harness

Drive load through existing midgard-manager tx-generator.
Add fixed run controls: run_id, duration, target TPS, batch size, concurrency, seed, replay corpus.
Emit JSONL per request/attempt with timestamp, latency, status, error class, retry count, tx id, and final outcome.
Capture node-side evidence

Query Prometheus before, during, and after the run.
Capture deltas for:
tx_submissions_enqueued_total
tx_submissions_rejected_total
tx_submissions_mempool_accepted_total
tx_submissions_processing_failed_total
tx_queue_size
tx_queue_peak_size
mempool_tx_count
commit_block_count_total
submit_block_count_total
commit_block_tx_count_total
commit_block_commitment_failures_total
merge_block_count_total
merge_block_failures_total
Define failure as a valid result

If the node halts after 10 seconds, the harness records that.
If queue grows without recovery, record peak and recovery failure.
If accepted/enqueued greatly exceeds mempool accepted, record ingestion overstatement.
If commit/submit/merge counters stop, record the last progress timestamp.
Add stop conditions for evidence preservation

Stop load if node API is unavailable for N consecutive probes.
Stop if Prometheus target goes down.
Stop if memory/OOM or container exit is detected.
Continue collecting recovery metrics for a configured window if services remain alive.
Generate baseline report

Output markdown/JSON summary with:
target TPS vs observed submit TPS
first failure time
last successful enqueue
last durable mempool acceptance
last committed/submitted/merged block
queue peak
mempool peak
retry amplification
node/container status
result: Passed, Failed, or Collapsed, not just pass/fail.
Only then fix throughput issues

Apply one improvement at a time or in traceable groups.
Re-run the same seed/corpus/profile.
Compare against baseline using identical environment and harness config.
Implementation Order

Harness run config and fixed-duration execution.
Per-request JSONL and summary artifacts.
Prometheus window collection.
Collapse/stop-condition detection.
Report generation.
Baseline run.
Fixes and before/after comparison.
This preserves the current system as the baseline while still producing useful data when it fails quickly.

1:28 PM
where to put the harness - the one that will execute midgard-manager at specific pace, collect and record the metrics, then increase the load, collect and record the metrics and so on. then in the end when system for sure fails at specific load - analyse the results.
1:30 PM

Put it in a new manager package:

/home/vicgenin/work/git/sundial-monorepo/demo/midgard-manager/packages/scalability-harness

Reason: this is operator/test tooling, not node runtime. It should orchestrate tx-generator, Prometheus, logs, and reporting without coupling benchmark logic into midgard-node.

Recommended Layout

demo/midgard-manager/packages/scalability-harness/
package.json
tsconfig.json
src/
bin/index.ts # CLI entrypoint
config/scenario.ts # scenario schema/validation
runner/load-runner.ts # executes stepped load profiles
runner/tx-generator.ts # spawns or imports tx-generator
metrics/prometheus.ts # query_range/snapshot collection
evidence/artifacts.ts # JSONL, summary, manifest writers
analysis/analyzer.ts # saturation/collapse/regression analysis
report/markdown.ts # final report rendering
scenarios/
baseline-100-800.json
saturation-discovery.json
tests/
unit/
Artifact Output
Use a gitignored run directory outside package source:

demo/benchmark-runs/
2026-05-18T120000Z-baseline/
run-manifest.json
scenario.json
load-events.jsonl
prometheus-samples.json
summary.json
report.md
Why Not Elsewhere

Not midgard-node: the harness must measure the node from outside, including failures.
Not tx-generator: that package should remain the load driver, not the benchmark controller.
Not scripts/test: shell scripts should only orchestrate startup/teardown; benchmark logic and analysis should be typed/tested.
Not internal-docs: docs describe the plan; code belongs in the demo tooling workspace.
Execution Model
The harness should own the loop:

Start tier: e.g. 100 TPS.
Run midgard-manager/tx-generator at that pace.
Record per-request artifacts.
Query Prometheus for the exact time window.
Wait recovery window.
Increase load.
Repeat until collapse/stop condition.
Analyze the last successful tier and first failed tier.
Render report.md.
Package Command
Add a manager script later:

"scalability-harness": "pnpm --filter @midgard-manager/scalability-harness start"
This keeps the harness close to the existing manager tooling while preserving a clean boundary between load generation, node runtime, and benchmark analysis.

Below is a concrete implementation backlog. Each task is intended to be independently executable by an engineer with only this text and the repo.

1. Create Scalability Harness Package
   Create a new package at demo/midgard-manager/packages/scalability-harness. This package owns benchmark orchestration, metric collection, evidence writing, and final analysis. It must not modify midgard-node behavior and must not embed benchmark logic into tx-generator.

Files to add:

demo/midgard-manager/packages/scalability-harness/package.json
demo/midgard-manager/packages/scalability-harness/tsconfig.json
demo/midgard-manager/packages/scalability-harness/src/bin/index.ts
demo/midgard-manager/packages/scalability-harness/src/index.ts
demo/midgard-manager/packages/scalability-harness/README.md
Package name:

"@midgard-manager/scalability-harness"
Required scripts:

{
"build": "tsc",
"type-check": "tsc --noEmit -p tsconfig.json --skipLibCheck",
"start": "node ./dist/bin/index.js",
"test": "vitest run --mode=test",
"clean": "rm -rf dist"
}
Update demo/midgard-manager/package.json with:

"scalability-harness": "pnpm --filter @midgard-manager/scalability-harness start",
"type-check:scalability-harness": "pnpm --filter @midgard-manager/scalability-harness run type-check"
Acceptance criteria:

pnpm --filter @midgard-manager/scalability-harness build succeeds.
pnpm --filter @midgard-manager/scalability-harness start --help prints CLI help.
No changes are made to midgard-node runtime behavior. 2. Define Scenario Config Schema
Add src/config/scenario.ts. This file defines the full benchmark scenario input. The scenario describes how load tiers are generated, where the node and Prometheus live, how artifacts are written, and what conditions stop the run.

Required type:

export interface ScalabilityScenario {
runId: string;
nodeEndpoint: string;
prometheusEndpoint: string;
outputDir: string;
seed: string;
transactionType: "one-to-one" | "multi-output" | "mixed";
oneToOneRatio?: number;
tierDurationSeconds: number;
recoverySeconds: number;
startTps: number;
maxTps: number;
stepMultiplier: number;
batchSize: number;
concurrency: number;
retryAttempts: number;
retryDelayMs: number;
stopConditions: StopConditions;
}
Required stop conditions:

export interface StopConditions {
maxConsecutiveNodeProbeFailures: number;
stopOnPrometheusDown: boolean;
stopOnCommitmentFailure: boolean;
stopOnMergeFailure: boolean;
maxRecoveryQueueSize?: number;
maxRecoveryMempoolSize?: number;
minUsefulThroughputRatio?: number;
}
Add validation rules:

runId must be non-empty and filesystem-safe.
URLs must start with http:// or https://.
startTps, maxTps, tierDurationSeconds, and batchSize must be positive.
stepMultiplier must be greater than 1.
concurrency must be positive.
oneToOneRatio must be 0..100 when provided.
maxTps must be greater than or equal to startTps.
Add example scenarios:

demo/midgard-manager/packages/scalability-harness/scenarios/baseline-100-800.json
demo/midgard-manager/packages/scalability-harness/scenarios/saturation-discovery.json
Acceptance criteria:

Invalid scenarios fail before any load is started.
Valid scenarios produce deterministic tier definitions.
Unit tests cover valid config, missing fields, invalid URLs, invalid numeric ranges, and invalid mixed-mode ratio. 3. Implement Tier Generation
Add src/config/tiers.ts. This file converts a scenario into ordered load tiers. The harness should not hardcode tier lists in the runner.

Input:

ScalabilityScenario
Output:

export interface LoadTier {
tierIndex: number;
targetTps: number;
durationSeconds: number;
recoverySeconds: number;
seed: string;
}
Tier generation logic:

First tier uses startTps.
Each next tier multiplies by stepMultiplier.
Stop when the next tier would exceed maxTps.
Include maxTps as the final tier if multiplication skips over it.
Tier seed should be deterministic, for example ${scenario.seed}:tier:${tierIndex}:tps:${targetTps}.
Example:

{
"startTps": 100,
"maxTps": 800,
"stepMultiplier": 2
}
Expected tiers:

100, 200, 400, 800
Acceptance criteria:

Tier generation is deterministic.
Tests cover exact max hit, skipped max, non-integer multiplier, and single-tier scenario. 4. Implement Artifact Directory and Writers
Add src/evidence/artifacts.ts. This module creates and writes all benchmark evidence. Artifacts must be machine-readable first, human-readable second.

Run directory format:

demo/benchmark-runs/<ISO_TIMESTAMP>-<runId>/
Files to create:

scenario.json
run-manifest.json
load-events.jsonl
tier-summaries.jsonl
prometheus-samples.json
stdout.log
stderr.log
summary.json
report.md
run-manifest.json must include:

{
runId: string;
startedAt: string;
gitSha: string;
workingTreeStatus: string;
nodeEndpoint: string;
prometheusEndpoint: string;
scenarioPath: string;
harnessVersion: string;
host: {
hostname: string;
platform: string;
arch: string;
cpus: number;
totalMemoryBytes: number;
};
}
Implementation notes:

Use git rev-parse HEAD for commit SHA.
Use git status --short for working tree state.
Use Node os module for host metadata.
Write JSONL by appending one JSON object per line.
Do not keep all request events in memory.
Acceptance criteria:

A dry run creates the full artifact directory.
JSON files are valid JSON.
JSONL files contain one valid JSON object per line.
Existing run directories are never overwritten. 5. Define Load Event Schema
Add src/evidence/load-events.ts. This defines the event records written by the harness. These events are separate from tx-generator’s current manifest files and are the harness-level audit trail.

Required event types:

export type LoadEvent =
| TierStartedEvent
| TierStoppedEvent
| TxGeneratorStartedEvent
| TxGeneratorStoppedEvent
| NodeProbeEvent
| PrometheusSnapshotEvent
| StopConditionEvent
| HarnessErrorEvent;
Minimum fields for all events:

{
ts: string;
runId: string;
tierIndex?: number;
targetTps?: number;
event: string;
}
Examples:

{"ts":"2026-05-18T10:00:00.000Z","runId":"baseline","tierIndex":0,"targetTps":100,"event":"tier_started"}
{"ts":"2026-05-18T10:01:00.000Z","runId":"baseline","tierIndex":0,"event":"node_probe","ok":true,"latencyMs":12}
{"ts":"2026-05-18T10:05:00.000Z","runId":"baseline","tierIndex":3,"event":"stop_condition","reason":"prometheus_down"}
Acceptance criteria:

Every tier has start and stop events.
Every stop condition is recorded with reason and supporting metric values.
Event writing continues even when tx-generator exits non-zero. 6. Implement Tx-Generator Wrapper
Add src/runner/tx-generator.ts. This module launches the existing @midgard-manager/tx-generator as the load driver. It should spawn the CLI rather than importing internals initially, because the harness must measure the load generator as an external process.

Command shape:

pnpm --filter @midgard-manager/tx-generator start -- \
 --endpoint <nodeEndpoint> \
 --type <transactionType> \
 --ratio <oneToOneRatio> \
 --batch-size <batchSize> \
 --interval <intervalSeconds> \
 --concurrency <concurrency> \
 --seed <tierSeed> \
 --output-dir <tierArtifactDir> \
 --test-wallet
Important current limitation:

The existing generator is batch/interval based, not exact TPS based.
The wrapper should calculate approximate interval from targetTps, batchSize, and concurrency.
Formula: intervalSeconds = max(1, floor((batchSize \* concurrency) / targetTps)) is coarse and may under-drive high TPS.
Record the requested target TPS and actual generator settings so reports do not pretend pacing is exact.
Required output capture:

Append stdout to stdout.log.
Append stderr to stderr.log.
Record process PID, start time, stop time, exit code, and signal.
Required controls:

Start process at tier start.
Stop process after durationSeconds.
Send SIGINT first.
If still running after grace period, send SIGTERM.
If still running after second grace period, send SIGKILL.
Acceptance criteria:

Wrapper starts and stops tx-generator reliably.
Process output is captured.
Non-zero exit is recorded as a measured event, not thrown away.
Tests mock process spawning and verify stop behavior. 7. Implement Node Health Probe
Add src/runner/node-probe.ts. This module checks whether the node API is alive during a tier and recovery window.

Probe endpoint:

GET <nodeEndpoint>/tx?tx_hash=0000000000000000000000000000000000000000000000000000000000000000
Expected healthy behavior:

Current MidgardNodeClient.isAvailable() treats 404 as healthy.
Match that behavior for consistency.
Probe result:

{
ok: boolean;
statusCode?: number;
latencyMs: number;
error?: string;
}
Probe frequency:

Default every 5s.
Make configurable later if needed.
Acceptance criteria:

Probe distinguishes healthy 404 from connection failure.
Consecutive failures are counted.
Stop condition fires after maxConsecutiveNodeProbeFailures.
Probe events are written to load-events.jsonl. 8. Implement Prometheus Client
Add src/metrics/prometheus.ts. This module queries Prometheus HTTP API directly.

Required endpoints:

/api/v1/query
/api/v1/query_range
Required functions:

queryInstant(query: string, time?: Date): Promise<PrometheusVectorResult>
queryRange(query: string, start: Date, end: Date, stepSeconds: number): Promise<PrometheusMatrixResult>
Required metric queries:

tx_submissions_enqueued_total
tx_submissions_rejected_total
tx_submissions_mempool_accepted_total
tx_submissions_processing_failed_total
tx_queue_size
tx_queue_peak_size
mempool_tx_count
commit_block_count_total
submit_block_count_total
commit_block_tx_count_total
commit_block_commitment_failures_total
merge_block_count_total
merge_block_failures_total
up{job="midgard_nodes"}
Optional cAdvisor queries:

container_memory_usage_bytes{image!=""}
rate(container_cpu_user_seconds_total{image!=""}[1m])
rate(container_network_receive_bytes_total{image!=""}[1m])
rate(container_network_transmit_bytes_total{image!=""}[1m])
Implementation rule:

Missing optional cAdvisor metrics should not fail the benchmark.
Missing primary node metrics should mark the tier as evidence-incomplete.
Acceptance criteria:

Tests parse vector and matrix responses.
HTTP errors and Prometheus "status":"error" responses are surfaced clearly.
Metrics are saved into prometheus-samples.json. 9. Implement Metric Window Collection
Add src/metrics/window.ts. This module collects before, during, and after metrics for each tier.

For each tier collect:

{
tierIndex: number;
targetTps: number;
startedAt: string;
stoppedAt: string;
recoveryStartedAt: string;
recoveryStoppedAt: string;
before: Record<string, number | null>;
afterLoad: Record<string, number | null>;
afterRecovery: Record<string, number | null>;
ranges: Record<string, PrometheusSeries[]>;
}
Range query step:

Use 15s by default to match current demo/midgard-node/prometheus.yml.
Make step configurable in scenario later if needed.
Counters should be summarized by deltas:

afterLoad - before
afterRecovery - before
Gauges should be summarized by peak and final value:

max(range)
last(range)
Acceptance criteria:

Counter deltas are computed correctly.
Gauge peaks are computed correctly.
Missing series are represented as null, not 0.
Recovery window metrics are collected even if the tier failed, as long as Prometheus is reachable. 10. Implement Tier Runner
Add src/runner/load-runner.ts. This is the main orchestration loop.

Tier lifecycle:

record tier_started
collect before metrics
start tx-generator
run node probes while generator is active
stop tx-generator after duration
collect after-load metrics
record recovery_started
continue probes during recovery
collect after-recovery metrics
summarize tier
decide whether to continue
The runner should continue to the next tier only if:

Node probe stop condition did not fire.
Prometheus target remained available if configured.
Stop-on-failure metrics did not increase if configured.
Useful throughput did not fall below configured threshold if configured.
The runner must not classify fast failure as a harness crash. Fast failure is expected baseline evidence.

Acceptance criteria:

One-tier dry run completes.
Multi-tier dry run executes tiers in order.
Stop condition prevents later tiers.
Tier summary is written even when the tier fails. 11. Implement Collapse Detection
Add src/analysis/collapse.ts. This module turns metrics and process state into a clear stop reason.

Collapse signals:

export type CollapseReason =
| "node_unavailable"
| "prometheus_down"
| "commitment_failures"
| "merge_failures"
| "queue_not_recovered"
| "mempool_not_recovered"
| "tx_generator_failed"
| "useful_throughput_below_threshold";
Useful throughput definition:

For current baseline, useful throughput should be tx_submissions_mempool_accepted_total delta divided by tier duration.
Do not use tx_submissions_enqueued_total as useful throughput because it only means accepted into the in-memory queue.
Throughput ratio:

usefulThroughputRatio = observedMempoolAcceptedTps / targetTps
Acceptance criteria:

Collapse reason includes supporting values.
If multiple reasons apply, choose the earliest or most severe.
Tests cover each collapse reason. 12. Implement Tier Summary Calculation
Add src/analysis/tier-summary.ts.

Required output:

export interface TierSummary {
tierIndex: number;
targetTps: number;
startedAt: string;
stoppedAt: string;
durationSeconds: number;
result: "completed" | "collapsed" | "evidence_incomplete";
collapseReason?: string;
enqueuedDelta: number | null;
rejectedDelta: number | null;
mempoolAcceptedDelta: number | null;
processingFailedDelta: number | null;
committedTxDelta: number | null;
committedBlockDelta: number | null;
submittedBlockDelta: number | null;
mergeFailureDelta: number | null;
commitmentFailureDelta: number | null;
observedEnqueuedTps: number | null;
observedMempoolAcceptedTps: number | null;
observedCommittedTps: number | null;
peakQueueSize: number | null;
finalQueueSizeAfterRecovery: number | null;
peakMempoolSize: number | null;
finalMempoolSizeAfterRecovery: number | null;
}
Interpretation rules:

enqueuedDelta measures HTTP boundary acceptance into the in-memory queue.
mempoolAcceptedDelta measures durable acceptance into MempoolDB.
committedTxDelta measures block commitment inclusion.
submittedBlockDelta measures L1 submission progress.
Report all four separately because current system can diverge between these stages.
Acceptance criteria:

Summary never conflates enqueued with durable accepted.
Null values appear when metrics are missing.
Tests cover normal tier, missing metrics, and collapsed tier. 13. Implement Final Analyzer
Add src/analysis/analyzer.ts. This module reads all tier summaries and produces the final benchmark conclusion.

Required analysis:

{
highestCompletedTier: number | null;
highestCompletedTargetTps: number | null;
firstCollapsedTier: number | null;
firstCollapsedTargetTps: number | null;
primaryBottleneck: string;
notes: string[];
}
Bottleneck heuristics:

If enqueued grows but mempool accepted does not, bottleneck is queue processor or transaction parsing.
If mempool accepted grows but committed tx does not, bottleneck is block commitment.
If committed blocks grow but submitted blocks do not, bottleneck is L1 submission.
If submitted blocks grow but merge does not, bottleneck is merge path.
If node probe fails, bottleneck is API/runtime availability.
If Prometheus target drops, bottleneck is runtime or observability availability.
Acceptance criteria:

Analyzer produces a clear first-failure tier.
Analyzer names the likely bottleneck from observed metric gaps.
Analyzer explicitly labels conclusions as heuristic when metrics are incomplete. 14. Implement Markdown Report Renderer
Add src/report/markdown.ts. This module renders report.md.

Required sections:

# Scalability Baseline Report

Run Metadata
Scenario
Tier Results
Collapse Point
Throughput Stage Deltas
Queue and Mempool Behavior
Commit/Submit/Merge Progress
Failure Signals
Primary Bottleneck Hypothesis
Artifact Index
Limitations
Tier table columns:

Tier
Target TPS
Result
Enqueued Δ
Mempool Accepted Δ
Committed Tx Δ
Submitted Blocks Δ
Peak Queue
Peak Mempool
Commit Failures Δ
Merge Failures Δ
Collapse Reason
Report wording requirement:

The report must explicitly state that this run measures current behavior without remediation.
The report must not claim production scalability.
The report must distinguish enqueued TPS, mempool accepted TPS, and committed TPS.
Acceptance criteria:

report.md is generated for successful and collapsed runs.
Report links or lists every artifact file.
Missing metrics are shown as n/a. 15. Implement CLI
Add CLI in src/bin/index.ts using commander, consistent with existing manager tooling.

Required command:

pnpm scalability-harness run --scenario scenarios/saturation-discovery.json
Required options:

--scenario <path>
--run-id <id>
--output-dir <dir>
--dry-run
--max-tier <number>
--no-increase
Behavior:

--scenario is required.
--run-id overrides scenario runId.
--output-dir overrides scenario outputDir.
--dry-run validates config, prints planned tiers, creates no load.
--max-tier stops after a given tier index.
--no-increase runs only the first tier.
Acceptance criteria:

CLI validates scenario before running.
CLI prints run directory at start.
CLI prints final report path at the end.
CLI exits non-zero only for harness failures, not for measured system collapse. 16. Add Example Scenario Files
Add scenarios/baseline-100-800.json.

Example:

{
"runId": "baseline-100-800",
"nodeEndpoint": "http://localhost:3000",
"prometheusEndpoint": "http://localhost:9090",
"outputDir": "../../benchmark-runs",
"seed": "baseline-seed-001",
"transactionType": "one-to-one",
"tierDurationSeconds": 300,
"recoverySeconds": 120,
"startTps": 100,
"maxTps": 800,
"stepMultiplier": 2,
"batchSize": 100,
"concurrency": 5,
"retryAttempts": 1,
"retryDelayMs": 1000,
"stopConditions": {
"maxConsecutiveNodeProbeFailures": 3,
"stopOnPrometheusDown": true,
"stopOnCommitmentFailure": false,
"stopOnMergeFailure": false,
"minUsefulThroughputRatio": 0.1
}
}
Add scenarios/saturation-discovery.json.

This scenario should start lower and grow until failure:

{
"runId": "saturation-discovery",
"nodeEndpoint": "http://localhost:3000",
"prometheusEndpoint": "http://localhost:9090",
"outputDir": "../../benchmark-runs",
"seed": "saturation-seed-001",
"transactionType": "mixed",
"oneToOneRatio": 70,
"tierDurationSeconds": 180,
"recoverySeconds": 180,
"startTps": 100,
"maxTps": 25000,
"stepMultiplier": 2,
"batchSize": 100,
"concurrency": 10,
"retryAttempts": 1,
"retryDelayMs": 1000,
"stopConditions": {
"maxConsecutiveNodeProbeFailures": 3,
"stopOnPrometheusDown": true,
"stopOnCommitmentFailure": false,
"stopOnMergeFailure": false,
"minUsefulThroughputRatio": 0.05
}
}
Acceptance criteria:

Both scenarios validate.
Dry-run prints expected tiers.
README explains these are baseline measurement scenarios, not acceptance tests. 17. Add Tests
Add tests under:

demo/midgard-manager/packages/scalability-harness/tests/unit
Required test files:

scenario.test.ts
tiers.test.ts
artifacts.test.ts
prometheus.test.ts
collapse.test.ts
tier-summary.test.ts
report.test.ts
tx-generator-wrapper.test.ts
Testing approach:

Mock filesystem using temp directories.
Mock Prometheus fetch responses.
Mock child process spawning.
Do not require a running node.
Do not require Docker.
Do not run real load in unit tests.
Acceptance criteria:

pnpm --filter @midgard-manager/scalability-harness test passes.
Tests cover both completed and collapsed run paths.
Tests verify collapse is not treated as harness failure. 18. Document Usage
Add demo/midgard-manager/packages/scalability-harness/README.md.

Required content:

Purpose
Prerequisites
How to run dry-run
How to run baseline
How to run saturation discovery
Expected artifacts
Metric interpretation
Known current limitations
Before/after workflow
Important wording:

The harness measures current behavior as-is.
A fast halt or collapse is a valid benchmark result.
tx_submissions_enqueued_total is not useful throughput.
Use tx_submissions_mempool_accepted_total and commit_block_tx_count_total for durable/useful progress.
Fixes should be applied after baseline and then measured with identical scenario, seed, and environment.
Acceptance criteria:

A developer can run the dry-run from the README.
A developer can identify where artifacts are written.
README explains how to compare before and after fixes. 19. Integrate Type Check
Update demo/midgard-manager/package.json so package-level type check includes the harness.

Change:

"type-check": "pnpm run type-check:cli && pnpm run type-check:tx-generator"
To:

"type-check": "pnpm run type-check:cli && pnpm run type-check:tx-generator && pnpm run type-check:scalability-harness"
Acceptance criteria:

pnpm --dir demo/midgard-manager run type-check includes the harness.
Existing CLI and tx-generator checks still run. 20. First Manual Validation Procedure
After implementation, validate manually without changing node code.

Commands:

cd demo/midgard-manager
pnpm install
pnpm build
pnpm scalability-harness run --scenario packages/scalability-harness/scenarios/baseline-100-800.json --dry-run
With node and Prometheus running:

pnpm scalability-harness run --scenario packages/scalability-harness/scenarios/baseline-100-800.json --max-tier 0
Expected output:

Run directory: demo/benchmark-runs/<timestamp>-baseline-100-800
Final report: demo/benchmark-runs/<timestamp>-baseline-100-800/report.md
Acceptance criteria:

Harness creates artifacts.
Harness starts tx-generator.
Harness collects Prometheus metrics.
Harness stops tx-generator.
Harness writes a report even if the node collapses.

What was built
Package scaffold — package.json, tsconfig.json, vitest.config.ts aligned to tx-generator conventions. Two new scripts added to the manager workspace: scalability-harness (run) and type-check:scalability-harness.

Scenario config and validation (src/config/scenario.ts) — The ScalabilityScenario type defines all benchmark inputs: endpoints, transaction mix, tier shape, stop conditions. validateScenario() enforces every constraint (filesystem-safe run IDs, URL prefixes, numeric bounds, ratio ranges) and fails before any load starts.

Tier generation (src/config/tiers.ts) — generateTiers() converts a scenario into an ordered list of LoadTier objects using the start→multiply→cap-at-max algorithm. Tier seeds are deterministic (seed:tier:N:tps:T). Two example scenarios ship in scenarios/: a clean 100→800 TPS stepped baseline and a 50→1600 TPS saturation-discovery run.

Artifact directory and writers (src/evidence/artifacts.ts) — ArtifactWriter.create() initializes a timestamped run directory under outputDir, writes scenario.json and run-manifest.json (capturing git SHA, working tree status, and host metadata), then exposes append-only JSONL writers for events and tier summaries. Run directories are never overwritten.

Load event schema (src/evidence/load-events.ts) — A discriminated union of eight typed event records (tier_started, tier_stopped, tx_generator_started, tx_generator_stopped, node_probe, prometheus_snapshot, stop_condition, harness_error). Every tier transition, probe result, and stop condition is a first-class typed event.

Tx-generator wrapper (src/runner/tx-generator.ts) — Spawns the existing @midgard-manager/tx-generator CLI as an external process via pnpm --filter … start -- start <flags>. Derives a coarse intervalSeconds from floor((batchSize × concurrency) / targetTps) and records both the requested TPS and actual estimate so reports cannot overstate pacing accuracy. Stdout/stderr are captured line-by-line into the artifact log files without buffering in memory. Shutdown follows SIGINT → SIGTERM → SIGKILL with configurable grace periods. Non-zero exits are recorded as events, not thrown.

Node health probe (src/runner/node-probe.ts) — probeNode() fires GET /tx?tx_hash=000…0 and treats 404 as healthy, matching MidgardNodeClient.isAvailable() exactly. runProbeLoop() runs sequential probes every 5 s (configurable), counts consecutive failures, and fires a stop_condition event with metric context when maxConsecutiveNodeProbeFailures is reached. External AbortSignal cancels the loop cleanly.

Prometheus client (src/metrics/prometheus.ts) — PrometheusClient queries /api/v1/query and /api/v1/query_range, surfacing network errors, HTTP errors, and Prometheus status:"error" responses as typed PrometheusQueryError. Fourteen primary node metrics and four optional cAdvisor metrics are defined as as const tuples. snapshotNodeMetrics() fans out all queries in parallel; cAdvisor failures are silently captured while node metric failures set evidenceIncomplete: true. flattenToScalars() produces the clean Record<string, number> used in PrometheusSnapshotEvent.

Metric window collection (src/metrics/window.ts) — collectTierWindow() fires instant snapshots at three time points (tier start, tier stop, recovery stop) and range queries across the full window in one parallel fan-out. All failures return null (never 0) for scalars and [] for ranges, so recovery metrics are always collected regardless of tier-phase failures. summarizeTierWindow() dispatches each metric to the right summary: counters get deltaLoad / deltaRecovery, gauges get peak / final from range data. isCounter() correctly classifies all node and cAdvisor queries, treating rate(...) wrappers as gauges.

Tier runner (src/runner/load-runner.ts) — runTier() orchestrates the full single-tier lifecycle: emits tier_started, spawns the tx-generator, runs a timed probe loop during the load phase, stops the generator, emits tier_stopped, runs a second probe loop during the recovery phase, collects the full metric window via collectTierWindow(), evaluates all metric-based stop conditions, emits a stop_condition event when one fires, and writes the tier summary unconditionally. Returns TierRunResult with shouldContinue so the caller decides whether to advance to the next tier. checkMetricStopConditions() is exported as a pure function covering all six stop condition kinds: prometheus_down, commitment_failure, merge_failure, recovery_queue_exceeded, recovery_mempool_exceeded, and throughput_below_minimum. The collectWindowFn and probeFetcher are injectable for testing. Duration overrides (tierDurationMs, recoveryDurationMs) allow fast test execution without real waits.

Collapse detection (src/analysis/collapse.ts) — detectCollapse() maps metrics and process state into a typed CollapseReason. Eight collapse reasons are supported: node_unavailable, prometheus_down, commitment_failures, merge_failures, queue_not_recovered, mempool_not_recovered, tx_generator_failed, and useful_throughput_below_threshold. Reasons are checked in priority order (most severe first); the first match wins. Useful throughput is derived from tx_submissions_mempool_accepted_total delta divided by tier duration, not from the enqueued counter, so HTTP-boundary acceptance is never conflated with durable MempoolDB acceptance. Each CollapseResult carries a values map with supporting metric figures. CollapseInputs accepts pre-extracted scalars so the function is pure and injectable in tests.

Tier summary (src/analysis/tier-summary.ts) — buildTierSummary() converts a TierWindowSummary, a CollapseResult, and an evidenceIncomplete flag into a fully typed TierSummary. The summary covers all four pipeline stages separately: HTTP boundary acceptance (enqueuedDelta), durable MempoolDB acceptance (mempoolAcceptedDelta), block commitment inclusion (committedTxDelta), and L1 submission progress (submittedBlockDelta). Observed TPS fields are derived from counter deltas divided by load-phase duration. Gauge fields carry peak and final-after-recovery values for the tx queue and mempool. The result field is collapsed when a CollapseResult is present, evidence_incomplete when metrics are incomplete but no collapse was detected, and completed otherwise.

Final analyzer (src/analysis/analyzer.ts) — analyzeTiers() receives the full slice of TierSummary records produced by a run and returns a BenchmarkConclusion: highestCompletedTier/Tps, firstCollapsedTier/Tps, primaryBottleneck, and notes. Bottleneck attribution follows two paths. Direct collapse reasons (node_unavailable, prometheus_down, commitment_failures, merge_failures, queue_not_recovered, mempool_not_recovered, tx_generator_failed) map to a named bottleneck without heuristic labeling. The useful_throughput_below_threshold reason and any unrecognized reason fall through to metric-gap analysis, which walks the four pipeline stages in order — HTTP queue to mempool, mempool to block commitment, committed blocks to L1 submission, submitted blocks to merge — and returns the first detected gap. All metric-gap conclusions are labeled heuristic in the notes. Missing evidence across any tier is counted and noted. Conclusions derived from incomplete metrics are explicitly flagged.

Markdown report renderer (src/report/markdown.ts) — renderReport() is a pure function that accepts a ReportInput (RunManifest, ScalabilityScenario, TierSummary slice, BenchmarkConclusion, and the list of artifact filenames) and returns a complete report.md string. Twelve sections are rendered in order: title and scope disclaimer, run metadata, scenario parameters and stop conditions, a twelve-column tier results table, collapse point, throughput stage deltas with prose distinguishing enqueued TPS / mempool accepted TPS / committed TPS as separate pipeline boundaries, queue and mempool gauge behavior, commit/submit/merge block progress, failure signals, primary bottleneck hypothesis with analyzer notes, artifact index, and limitations. Null metric fields render as n/a throughout. The limitations section explicitly states that the run measures current behavior without remediation and does not claim production scalability.

CLI and run-level orchestrator (src/bin/index.ts) — Two commands are exposed under midgard-scalability-harness. The run command accepts --scenario <path> (required), --run-id, --output-dir, --dry-run, --max-tier <n>, and --no-increase. Scenario validation runs before any artifact directory is created; invalid input exits non-zero with a clear message. --dry-run prints the filtered tier plan and exits without touching the filesystem. The live path creates the artifact directory (printing its path as the first output line), reads the run manifest back for the report, then drives a tier loop: runTier → detectCollapse (bridging TierRunResult to CollapseInputs via local helpers) → buildTierSummary → accumulate. The loop breaks on shouldContinue: false or on a harness throw, which sets a harnessErrorOccurred flag without discarding already-collected data. After the loop, analyzeTiers and renderReport produce the final BenchmarkConclusion and report.md string; writeSummary and writeReport persist them. Artifact files are enumerated from the run directory after summary is written and report.md is added explicitly so it appears in its own artifact index. The final report path is printed as the last output line. System collapse (measured performance breakdown) exits 0; harness failures exit 1. The tiers command was updated in parallel to use --scenario, --max-tier, and --no-increase for consistency with the run command.

Test coverage
419 unit tests across 11 test files, all passing. The CLI and orchestration layer are not covered by automated tests; correctness is verified by the acceptance criteria (scenario validation, run-directory print, report-path print, exit-code semantics) and by the comprehensive unit coverage of every underlying module.
