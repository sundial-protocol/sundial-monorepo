# @midgard-manager/scalability-harness

Benchmark orchestration, metric collection, evidence writing, and analysis for Midgard L2 scalability testing.

This package is standalone: it does not modify `midgard-node` runtime behavior and does not embed benchmark logic into `tx-generator`.

## Commands

```bash
# Build
pnpm build

# Type-check without emitting
pnpm type-check

# Run unit tests
pnpm test

# Print CLI help
pnpm start --help
```

### Run a plan (recommended)

A plan runs an ordered sequence of scenarios and produces an aggregated report.

```bash
# Full §8.1 benchmark sequence
pnpm run start -- run --plan plans/standard-progression.json

# §8.3 saturation discovery sequence
pnpm run start -- run --plan plans/saturation.json

# Dry run — validate all scenarios and print tier breakdowns without load
pnpm run start -- run --plan plans/standard-progression.json --dry-run

# Override output directory
pnpm run start -- run --plan plans/standard-progression.json --output-dir /tmp/bench
```

### Run a single scenario

```bash
pnpm run start -- run --scenario scenarios/warmup.json
pnpm run start -- run --scenario scenarios/warmup.json --dry-run
pnpm run start -- run --scenario scenarios/stress-10000.json --max-tier 2
pnpm run start -- run --scenario scenarios/initial-800.json --run-id initial-800-v2
```

### Other commands

```bash
# Run preflight checks against a scenario's endpoints
pnpm run start -- preflight --scenario scenarios/warmup.json

# Print load tiers for a scenario without running
pnpm run start -- tiers --scenario scenarios/practical-24985.json
pnpm run start -- tiers --scenario scenarios/saturation-ramp-25pct.json --max-tier 4
```

### Collateral top-up (block commitment wallet)

If block commitments fail with collateral errors (for example "required 5000000 Lovelace collateral"),
top up the block-commitment operator wallet before running formal scalability scenarios.

```bash
# From demo/ (workspace shortcut)
npm run wallet:topup:block-commitment

# Directly in demo/midgard-node
npm --prefix midgard-node run wallet:topup-block-commitment
```

### Flags reference

| Flag                      | Commands                    | Description                                          |
| ------------------------- | --------------------------- | ---------------------------------------------------- |
| `--plan <path>`           | `run`                       | Path to a plan JSON file                             |
| `--scenario <path>`       | `run`, `preflight`, `tiers` | Path to a scenario JSON file                         |
| `--dry-run`               | `run`                       | Validate config and print tier breakdown; no load    |
| `--output-dir <dir>`      | `run`                       | Override the scenario/plan `outputDir`               |
| `--request-events <mode>` | `run`                       | `off` \| `sampled` \| `all` per-request JSONL events |
| `--grafana-screenshots`   | `run`, `preflight`          | Force-enable `scenario.grafanaScreenshots.enabled`   |
| `--run-id <id>`           | `run --scenario`            | Override the scenario's `runId`                      |
| `--max-tier <n>`          | `run --scenario`, `tiers`   | Stop after tier index n (inclusive)                  |
| `--no-increase`           | `run --scenario`, `tiers`   | Run only the first tier                              |

`--plan` and `--scenario` are mutually exclusive. `--run-id`, `--max-tier`, and `--no-increase` only apply in `--scenario` mode.

## Formal Run Gate

Formal benchmark execution is gated by a preflight checklist. The `run` command executes this gate automatically before any load starts. In plan mode the gate runs once against the first scenario's endpoints before scenario 1 starts.

Preflight checks:

- `scenario_validity`
- `node_probe`
- `prometheus_scrape_health`
- `required_metrics_presence`
- `artifact_directory_writable`
- `tx_generator_invocable`

If any check fails, classification is `Blocked`, the harness prints actionable reasons, and no load starts.

## Plans

A plan file encodes an ordered sequence of scenarios to run. Plan files live in `plans/`.

```jsonc
{
  "planId": "standard-progression",
  "description": "...",
  "outputDir": "benchmark-plans",
  "stopOnFailure": true, // abort on Failed/Blocked; continue on Passed with Observations
  "scenarios": [
    "../scenarios/warmup.json", // paths relative to the plan file
    "../scenarios/initial-800.json",
  ],
}
```

**Stop condition:** when `stopOnFailure` is `true` (the default), the harness stops after the first scenario that returns `Failed` or `Blocked`. Scenarios that return `Passed with Observations` do not stop the plan. Remaining scenarios are recorded as `Skipped` in the plan report.

### `plans/standard-progression.json`

Full §8.1 benchmark sequence; stops on first failure:

```
warmup → initial-800 → institutional-1000 → institutional-5000 → stress-10000 → practical-24985
```

### `plans/saturation.json`

Post-formal-tier saturation sequence (§8.3); continues regardless of per-scenario result:

```
saturation-discovery → saturation-ramp-25pct
```

## Scenarios

Scenario files live in `scenarios/`. Each file is a JSON object conforming to `ScalabilityScenario`.

| File                               | runId                         | Target TPS | Test plan section              |
| ---------------------------------- | ----------------------------- | ---------- | ------------------------------ |
| `warmup.json`                      | `warmup`                      | 100        | §8.1 warm-up                   |
| `initial-800.json`                 | `initial-800`                 | 800        | §8.1 initial TPS validation    |
| `institutional-1000.json`          | `institutional-1000`          | 1,000      | §8.1 institutional baseline    |
| `institutional-5000.json`          | `institutional-5000`          | 5,000      | §8.1 institutional load        |
| `stress-10000.json`                | `stress-10000`                | 10,000     | §8.1 institutional stress      |
| `practical-24985.json`             | `practical-24985`             | 24,985     | §8.1 practical estimate target |
| `spike.json`                       | `spike`                       | 49,970     | §8.1 peak spike                |
| `baseline-100-800.json`            | `baseline-100-800`            | 800        | §13 step-ramp baseline         |
| `baseline-100-800-replay.json`     | `baseline-100-800-replay`     | 800        | §13 replay variant             |
| `saturation-discovery.json`        | `saturation-discovery`        | 1,600      | §8.3 saturation discovery      |
| `saturation-discovery-replay.json` | `saturation-discovery-replay` | 1,600      | §8.3 replay variant            |
| `saturation-ramp-25pct.json`       | `saturation-ramp-25pct`       | varies     | §8.3 25% increment ramp        |

Tiers are generated from `startTps` to `maxTps` using `stepMultiplier` (multiply) or `ramp.percentIncrement` (percent increment). When `startTps === maxTps` the scenario has a single tier.

## Artifact Output

### Single scenario run

Each run creates a timestamped directory under `outputDir`:

```
<outputDir>/<ISO_TIMESTAMP>-<runId>/
  scenario.json           — scenario config as loaded
  run-manifest.json       — runId, git SHA, host, endpoints, harness version
  load-events.jsonl       — per-request events (if --request-events != off)
  tier-summaries.jsonl    — one JSON line per tier
  prometheus-samples.json — metric snapshots
  loki-captures.json      — Loki log captures (if lokiEndpoint configured)
  tempo-captures.json     — Tempo trace captures (if tempoEndpoint configured)
  grafana-screenshots/    — dashboard screenshots (if grafanaScreenshots.enabled)
  grafana-screenshots.json — screenshot capture manifest (if enabled)
  summary.json            — machine-readable BenchmarkConclusion
  report.md               — human-readable scenario report
  stdout.log / stderr.log
```

### Plan run

```
<outputDir>/<ISO_TIMESTAMP>-<planId>/
  plan-manifest.json      — plan metadata, git SHA, host
  plan-summary.json       — machine-readable PlanConclusion + scenario index
  plan-report.md          — aggregated human-readable plan report
  01-warmup/              — per-scenario subdirectory (1-based, zero-padded)
    scenario.json
    run-manifest.json
    tier-summaries.jsonl
    prometheus-samples.json
    grafana-screenshots/
    grafana-screenshots.json
    summary.json
    report.md
    ...
  02-initial-800/
  03-institutional-1000/
  ...
```

Scenario subdirectories are named `<NN>-<runId>` so they sort in execution order. Run directories are never overwritten.

## Scenario Fields

| Field                     | Type    | Description                                                                                         |
| ------------------------- | ------- | --------------------------------------------------------------------------------------------------- |
| `runId`                   | string  | Filesystem-safe identifier (alphanumeric, `-`, `_`)                                                 |
| `description`             | string? | Free-text description; recorded in manifest and report                                              |
| `nodeEndpoint`            | string  | Midgard node HTTP endpoint                                                                          |
| `prometheusEndpoint`      | string  | Prometheus HTTP endpoint                                                                            |
| `lokiEndpoint`            | string? | Loki endpoint for log evidence capture                                                              |
| `tempoEndpoint`           | string? | Tempo endpoint for trace evidence capture                                                           |
| `outputDir`               | string  | Parent directory for run artifacts                                                                  |
| `seed`                    | string  | Deterministic generation seed                                                                       |
| `replayCorpusPath`        | string? | Path to a pre-built transaction corpus (relative to scenario file)                                  |
| `l1ProviderMode`          | string? | `kupmios` \| `blockfrost` \| `emulator` \| `unknown`                                                |
| `walletMode`              | string? | `test-wallet` \| `external-key`                                                                     |
| `walletProvisioningNote`  | string? | Free-text provisioning note; recorded in manifest                                                   |
| `transactionType`         | string  | `one-to-one` \| `multi-output` \| `mixed`                                                           |
| `oneToOneRatio`           | number? | Percentage of one-to-one txs when `mixed` (0–100)                                                   |
| `startTps`                | number  | First tier target TPS                                                                               |
| `maxTps`                  | number  | Maximum tier target TPS                                                                             |
| `stepMultiplier`          | number? | TPS multiplier between tiers (legacy; prefer `ramp`)                                                |
| `ramp`                    | object? | `{ strategy: "multiply", stepMultiplier }` or `{ strategy: "percent_increment", percentIncrement }` |
| `tierOverrides`           | array?  | Per-tier duration/recovery overrides by `tierIndex`                                                 |
| `tierDurationSeconds`     | number  | How long each tier runs                                                                             |
| `recoverySeconds`         | number  | Cool-down between tiers                                                                             |
| `batchSize`               | number  | Transactions per generator batch                                                                    |
| `concurrency`             | number  | Concurrent generator batches                                                                        |
| `retryAttempts`           | number  | Submission retry count                                                                              |
| `retryDelayMs`            | number  | Delay between retries (ms)                                                                          |
| `grafanaScreenshots`      | object? | Opt-in Grafana screenshot capture configuration                                                     |
| `runClassificationPolicy` | object? | Optional overrides for formal run-classification thresholds                                         |
| `stopConditions`          | object  | Conditions that abort the run early                                                                 |

### Grafana Screenshots

When `grafanaScreenshots.enabled` is true, the harness captures event-based screenshots:

- tier baseline (`tier_started` timestamp)
- first metric stop-condition threshold crossing (one-time per scenario)
- tier end (`after_load`)
- recovery end (`after_recovery`)
- optional new peaks (queue, mempool, commitment duration) with cooldown
- plan-summary layout plus optional per-panel set (`d-solo`) for final evidence

Runtime prerequisite: install `playwright` in this package and ensure Chromium is available.

`grafanaScreenshots` fields:

| Field                        | Type     | Description                                                                  |
| ---------------------------- | -------- | ---------------------------------------------------------------------------- |
| `enabled`                    | boolean  | Enable screenshot capture for this scenario                                  |
| `grafanaBaseUrl`             | string   | Grafana base URL (for example `http://localhost:3001`)                       |
| `dashboardJsonPath`          | string   | Dashboard JSON path used for deterministic panel/layout mapping              |
| `dashboardUid`               | string?  | Optional UID override (otherwise read from dashboard JSON)                   |
| `timezone`                   | string?  | Grafana URL timezone parameter (default `utc`)                               |
| `theme`                      | string?  | `light` or `dark` (default `light`)                                          |
| `lookbackMinutes`            | number?  | Lookback window for point-in-time captures (default `10`)                    |
| `viewportWidth`              | number?  | Browser viewport width in px (default `1920`)                                |
| `viewportHeight`             | number?  | Browser viewport height in px (default `1080`)                               |
| `waitForPanelsMs`            | number?  | Extra wait after navigation before capture (default `4000`)                  |
| `peakCaptureCooldownSeconds` | number?  | Cooldown between peak-triggered captures (default `60`)                      |
| `capturePeakEvents`          | boolean? | Enable optional peak captures (default `true`)                               |
| `captureFinalPanelSet`       | boolean? | Capture final plan-summary per-panel image set via `d-solo` (default `true`) |

## Stop Conditions

| Field                             | Type          | Description                                                                           |
| --------------------------------- | ------------- | ------------------------------------------------------------------------------------- |
| `maxConsecutiveNodeProbeFailures` | number        | Abort after N consecutive probe failures                                              |
| `stopOnPrometheusDown`            | boolean       | Abort if Prometheus becomes unreachable                                               |
| `stopOnCommitmentFailure`         | boolean       | Abort if block commitment stops advancing                                             |
| `stopOnMergeFailure`              | boolean       | Abort if merge stops advancing                                                        |
| `maxRecoveryQueueSize`            | number?       | Abort if queue depth exceeds this during recovery                                     |
| `maxRecoveryMempoolSize`          | number?       | Abort if mempool depth exceeds this during recovery                                   |
| `minUsefulThroughputRatio`        | number? (0–1) | Abort if observed durable mempool accepted TPS divided by target TPS falls below this |

## Metric Contract

Scalability reports use three distinct node pipeline boundaries:

- `tx_submissions_enqueued_total` — HTTP enqueue throughput (in-memory queue)
- `tx_submissions_mempool_accepted_total` — durable intake throughput (`MempoolDB` persistence)
- `commit_block_tx_count_total` — committed throughput (included in committed blocks)
- `l1_commitment_fees_lovelace_total` — total L1 commitment fees spent during the window
- `l1_commitment_fee_lovelace_last` — most recent L1 commitment fee at end of load phase

The harness derives `L1 Fee / Committed L2 Tx` as `l1_commitment_fees_lovelace_total Δ / commit_block_tx_count_total Δ`.

Do not use `tx_submissions_accepted_total` in new reports. It is a deprecated legacy alias.
