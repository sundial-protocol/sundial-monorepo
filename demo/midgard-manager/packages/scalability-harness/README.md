# @midgard-manager/scalability-harness

Benchmark orchestration, metric collection, evidence writing, and analysis for Midgard L2 scalability testing.

This package is standalone: it does not modify `midgard-node` runtime behavior and does not embed benchmark logic into `tx-generator`.

## Commands

```bash
# Build
pnpm build

# Type-check without emitting
pnpm type-check

# Print CLI help
pnpm start --help

# Run preflight checks (required before formal benchmark runs)
pnpm run start -- preflight --scenario scenarios/baseline-100-800.json

# Run a scenario
pnpm run start -- run --scenario scenarios/baseline-100-800.json

# Preview load tiers for a scenario without running
pnpm run start -- tiers --scenario scenarios/baseline-100-800.json

# Run unit tests
pnpm test
```

`--scenario <path>` is required for `preflight`, `run`, and `tiers`.

## Formal Run Gate

Formal benchmark execution is gated by a preflight checklist. The harness
`run` command executes this gate automatically before any load starts.

The preflight checks are:

- `scenario_validity`
- `node_probe`
- `prometheus_scrape_health`
- `required_metrics_presence`
- `artifact_directory_writable`
- `tx_generator_invocable`

If any check fails, classification is `Blocked`, the harness prints actionable
reasons, and no formal run starts.

## Scenarios

Scenario files live in `scenarios/`. Each file is a JSON object conforming to `ScalabilityScenario`.

| Scenario                    | Description                                                                    |
| --------------------------- | ------------------------------------------------------------------------------ |
| `baseline-100-800.json`     | Stepped load from 100 to 800 TPS (one-to-one, ×2 steps)                        |
| `saturation-discovery.json` | Aggressive ramp from 50 to 1600 TPS (mixed, ×2 steps) to find saturation point |

## Metric Contract

Scalability reports and analysis in this package use three distinct node
pipeline boundaries:

- `tx_submissions_enqueued_total` for HTTP enqueue throughput (in-memory queue).
- `tx_submissions_mempool_accepted_total` for durable intake throughput
  (`MempoolDB` persistence).
- `commit_block_tx_count_total` for committed throughput (included in committed
  blocks).
- `l1_commitment_fees_lovelace_total` for total L1 commitment fees (lovelace)
  spent during the measured window.
- `l1_commitment_fee_lovelace_last` for the most recent L1 commitment fee
  (lovelace) observed at the end of the load phase.

The harness report derives `L1 Fee / Committed L2 Tx` as:

- `l1_commitment_fees_lovelace_total Δ / commit_block_tx_count_total Δ`

Do not use `tx_submissions_accepted_total` in new reports. It is a deprecated
legacy alias that collapses enqueue and durable-acceptance semantics.

## Artifact Output

Each run creates a timestamped directory under `outputDir`:

```
<outputDir>/<ISO_TIMESTAMP>-<runId>/
  scenario.json           — scenario input verbatim
  run-manifest.json       — git SHA, host, harness version
  load-events.jsonl       — one event per line (tier lifecycle, probes, stop conditions)
  tier-summaries.jsonl    — one summary per tier
  prometheus-samples.json — metric snapshots
  stdout.log              — generator stdout
  stderr.log              — generator stderr
  summary.json            — final run summary
  report.md               — human-readable report
```

Run directories are never overwritten.

## Scenario Fields

| Field                     | Type                                      | Description                                                 |
| ------------------------- | ----------------------------------------- | ----------------------------------------------------------- |
| `runId`                   | string                                    | Filesystem-safe identifier (alphanumeric, `-`, `_`)         |
| `nodeEndpoint`            | string                                    | Midgard node HTTP endpoint                                  |
| `prometheusEndpoint`      | string                                    | Prometheus HTTP endpoint                                    |
| `outputDir`               | string                                    | Parent directory for run artifacts                          |
| `seed`                    | string                                    | Deterministic generation seed                               |
| `transactionType`         | `one-to-one` \| `multi-output` \| `mixed` | Transaction mix                                             |
| `oneToOneRatio`           | number (0–100)                            | Percentage of one-to-one txs in mixed mode                  |
| `startTps`                | number                                    | First tier target TPS                                       |
| `maxTps`                  | number                                    | Maximum tier target TPS                                     |
| `stepMultiplier`          | number (>1)                               | TPS multiplier between tiers                                |
| `tierDurationSeconds`     | number                                    | How long each tier runs                                     |
| `recoverySeconds`         | number                                    | Cool-down between tiers                                     |
| `batchSize`               | number                                    | Transactions per generator batch                            |
| `concurrency`             | number                                    | Concurrent generator batches                                |
| `retryAttempts`           | number                                    | Submission retry count                                      |
| `retryDelayMs`            | number                                    | Delay between retries (ms)                                  |
| `runClassificationPolicy` | object?                                   | Optional overrides for formal run-classification thresholds |
| `stopConditions`          | object                                    | Conditions that abort the run early                         |

## Stop Conditions

| Field                             | Type          | Description                                                                                                                     |
| --------------------------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `maxConsecutiveNodeProbeFailures` | number        | Abort after N consecutive probe failures                                                                                        |
| `stopOnPrometheusDown`            | boolean       | Abort if Prometheus becomes unreachable                                                                                         |
| `stopOnCommitmentFailure`         | boolean       | Abort if block commitment stops advancing                                                                                       |
| `stopOnMergeFailure`              | boolean       | Abort if merge stops advancing                                                                                                  |
| `maxRecoveryQueueSize`            | number?       | Abort if queue depth exceeds this during recovery                                                                               |
| `maxRecoveryMempoolSize`          | number?       | Abort if mempool depth exceeds this during recovery                                                                             |
| `minUsefulThroughputRatio`        | number? (0–1) | Abort if observed durable mempool accepted TPS (`tx_submissions_mempool_accepted_total`) divided by target TPS falls below this |
