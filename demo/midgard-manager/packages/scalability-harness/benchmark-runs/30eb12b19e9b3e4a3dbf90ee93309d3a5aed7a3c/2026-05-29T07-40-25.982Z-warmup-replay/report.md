# 📊 Scalability Baseline Report

> **Scope:** This report measures the current behavior of the Sundial node under synthetic
> load. Results reflect a single benchmark run executed without remediation and do not
> claim or imply production scalability.

## 📌 Run Metadata

| Field                    | Value                                                                                                                                  |
| ------------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| Run ID                   | warmup-replay                                                                                                                          |
| Started At               | 2026-05-29T07:40:25.982Z                                                                                                               |
| Git SHA                  | 30eb12b19e9b3e4a3dbf90ee93309d3a5aed7a3c                                                                                               |
| Node Endpoint            | http://localhost:3000                                                                                                                  |
| Prometheus Endpoint      | http://localhost:9090                                                                                                                  |
| L1 Provider Mode         | emulator                                                                                                                               |
| Wallet Mode              | test-wallet                                                                                                                            |
| Wallet Provisioning Note | external-key mode — set WALLET_PRIVATE_KEY to a pre-funded key whose UTxOs are initialized on the node before running replay scenarios |
| Harness Version          | 0.1.0                                                                                                                                  |
| Replay Corpus Path       | midgard-manager/packages/scalability-harness/corpora/corpus-v1-net-preview-t-one-to-one-r-na-n-5000000.jsonl                           |
| Replay Corpus SHA256     | 4c1485e9f69df29001b9c14e8017d0bf0301b377aca02f95cfedd9cb75a3ca80                                                                       |
| Host                     | dev3 (linux/x64, 6 CPUs, 31.0 GB)                                                                                                      |

## 🔬 Scenario

| Parameter                      | Value                                                                                                                                  |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| Transaction Type               | one-to-one                                                                                                                             |
| L1 Provider Mode               | emulator                                                                                                                               |
| Wallet Mode                    | test-wallet                                                                                                                            |
| Wallet Provisioning Note       | external-key mode — set WALLET_PRIVATE_KEY to a pre-funded key whose UTxOs are initialized on the node before running replay scenarios |
| Start TPS                      | 100                                                                                                                                    |
| Max TPS                        | 100                                                                                                                                    |
| Step Multiplier                | 2                                                                                                                                      |
| Tier Duration                  | 600 s                                                                                                                                  |
| Recovery Duration              | 120 s                                                                                                                                  |
| Tx Cost (s)                    | 0.2                                                                                                                                    |
| Retry Attempts                 | 1                                                                                                                                      |
| Retry Delay                    | 0 ms                                                                                                                                   |
| Request Events Mode            | off                                                                                                                                    |
| Seed                           | warmup-2026-05-18                                                                                                                      |
| Replay Corpus Path             | midgard-manager/packages/scalability-harness/corpora/corpus-v1-net-preview-t-one-to-one-r-na-n-5000000.jsonl                           |
| Max Consecutive Probe Failures | 8                                                                                                                                      |
| Stop On Prometheus Down        | true                                                                                                                                   |
| Stop On Commitment Failure     | true                                                                                                                                   |
| Stop On Merge Failure          | true                                                                                                                                   |
| Max Recovery Queue Size        | 5000                                                                                                                                   |
| Max Recovery Mempool Size      | 0                                                                                                                                      |

## 🏃 Tier Results

| Tier | Target TPS | Result       | Enqueued Δ        | Mempool Accepted Δ | Committed Tx Δ | Submitted Blocks Δ | Peak Queue         | Peak Mempool | Commit Failures Δ | Merge Failures Δ | Collapse Reason |
| ---- | ---------- | ------------ | ----------------- | ------------------ | -------------- | ------------------ | ------------------ | ------------ | ----------------- | ---------------- | --------------- |
| 0    | 100        | completed ✅ | 59803.06790132307 | 59504.397179137624 | 58196          | 244                | 34.948498965414736 | 1308         | 0                 | 0                | n/a             |

## 🚦 Formal Run Classification

**Classification:** Passed ✅

- Policy checks evaluated: 10
- Violated checks: 0
- Failure-severity violations: 0
- Observation-severity violations: 0

**Reasons:**

- All configured formal run criteria passed.

**Violated checks:**

_No violated checks._

## 💥 Collapse Point

No collapse detected. All tiers completed or ran with incomplete evidence.
**Highest completed tier:** 0 at 100 TPS

## ⚡ Throughput Stage Deltas

The columns below reflect three distinct pipeline stages:

- **Enqueued TPS** (`tx_submissions_enqueued_total`) — rate of transactions
  accepted at the HTTP boundary into the in-memory queue.
  This is not a durability signal; it only reflects HTTP-level acceptance.
- **Mempool Accepted TPS** (`tx_submissions_mempool_accepted_total`) — rate
  of transactions durably written to MempoolDB.
  This is the first durable acceptance boundary and is distinct from the enqueued count.
- **Committed TPS** (`commit_block_tx_count_total`) — rate of transactions
  included in a committed block and rooted in the on-chain state commitment.
- Deprecated alias: `tx_submissions_accepted_total` is legacy and must not
  be used for new reports.

Gaps between adjacent columns identify where the pipeline loses throughput.

| Tier | Target TPS | Enqueued TPS | Mempool Accepted TPS | Committed TPS |
| ---- | ---------- | ------------ | -------------------- | ------------- |
| 0    | 100        | 99.66        | 99.17                | 96.99         |

![Received Transactions Per Second](charts/received-tps.svg)

![Mempool Drain Rate (committed tx/s)](charts/mempool-drain-rate.svg)

![Net Mempool Drain Rate (committed - accepted tx/s)](charts/net-mempool-drain-rate.svg)

![Received Transactions](charts/received-cumulative.svg)

![Committed Transactions](charts/committed-cumulative.svg)

## 📨 Client Submission Evidence

Client-side submission evidence is aggregated per tier/window and does not
require per-request JSONL.

- Outcomes are tracked as submitted, rejected, node_unavailable, and error.
- Retry evidence reports total retries and count of retried submissions.
- Submitted-latency p95 is derived from bounded histogram buckets.

| Tier | Target TPS | Submitted | Rejected | Node Unavailable | Error | Total Retries | Retried Submissions | Submitted Latency p95 (ms) |
| ---- | ---------- | --------- | -------- | ---------------- | ----- | ------------- | ------------------- | -------------------------- |
| 0    | 100        | 62800     | 0        | 0                | 0     | 0             | 0                   | 50                         |

## ⏱️ Accepted-to-Committed Latency Evidence

Accepted-to-committed latency is estimated from Prometheus counters using
cohort alignment (`cohort_counter_alignment_v1`):

- Bucket accepted transactions by scrape interval from
  `tx_submissions_mempool_accepted_total` deltas.
- For each accepted cohort, find when cumulative committed count
  (`commit_block_tx_count_total`) catches up.
- Compute weighted p50/p95/p99 latency across resolved cohorts.

Confidence notes indicate scrape-resolution limits and unresolved cohorts
(right-censoring at tier window end).

| Tier | Target TPS | Method                      | Confidence | Resolved Tx Ratio | Accepted→Committed p50 (ms) | Accepted→Committed p95 (ms) | Accepted→Committed p99 (ms) |
| ---- | ---------- | --------------------------- | ---------- | ----------------- | --------------------------- | --------------------------- | --------------------------- |
| 0    | 100        | cohort_counter_alignment_v1 | high       | 99.5%             | 0                           | 15000                       | 15000                       |

**Confidence Notes:**

- Tier 0: Scrape step is approximately 15s; latency resolution is bounded by this interval. Only 99.5% of accepted transactions were matched to committed progress before window end.

## 📦 Queue and Mempool Behavior

| Tier | Target TPS | Peak Queue         | Final Queue (after recovery) | Peak Mempool | Final Mempool (after recovery) |
| ---- | ---------- | ------------------ | ---------------------------- | ------------ | ------------------------------ |
| 0    | 100        | 34.948498965414736 | 0                            | 1308         | 0                              |

![Sundial Transactions in Queue](charts/tx-queue.svg)

![Mempool Transactions](charts/mempool-count.svg)

![Mempool Growth Rate (d/dt mempool tx count)](charts/mempool-growth-rate.svg)

![Commitment Deferred Tx Requests](charts/commitment-window-deferred.svg)

## 🔗 Commit/Submit/Merge Progress

L1 commitment fee fields are derived as follows:

- `L1 Fees Δ` from `l1_commitment_fees_lovelace_total` counter delta over load phase.
- `Last L1 Fee` from `l1_commitment_fee_lovelace_last` instant value at load stop.
- `L1 Fee / Committed L2 Tx` = `L1 Fees Δ / Committed Tx Δ` when `Committed Tx Δ > 0`.

| Tier | Target TPS | Committed Blocks Δ | Submitted Blocks Δ | Merged Blocks Δ | Merge Failures Δ | Commit Failures Δ | L1 Fees Δ (lovelace) | Last L1 Fee (lovelace) | L1 Fee / Committed L2 Tx (lovelace) |
| ---- | ---------- | ------------------ | ------------------ | --------------- | ---------------- | ----------------- | -------------------- | ---------------------- | ----------------------------------- |
| 0    | 100        | 245                | 244                | 241             | 0                | 0                 | 54929643             | n/a                    | 943.87                              |

![Built Blocks](charts/built-blocks.svg)

![Built Blocks Per Second](charts/built-blocks-rate.svg)

![Submitted Blocks](charts/submitted-blocks.svg)

![Submitted Blocks Per Second](charts/submitted-blocks-rate.svg)

![Merged Blocks](charts/merged-blocks.svg)

![Merged Blocks Per Second](charts/merged-blocks-rate.svg)

## 🚨 Failure Signals

| Tier | Target TPS | Commit Failures Δ | Merge Failures Δ | Rejected Δ | Queue Backpressure Rejected Δ | Stream Backpressure Rejected Δ | Offer Timeout Rejected Δ | Deferred Tx Requests Peak | Processing Failed Δ |
| ---- | ---------- | ----------------- | ---------------- | ---------- | ----------------------------- | ------------------------------ | ------------------------ | ------------------------- | ------------------- |
| 0    | 100        | 0                 | 0                | 23         | 0                             | 0                              | 0                        | 0                         | 78                  |

![Block Commitment Failures](charts/commit-failures.svg)

![Merge Failures](charts/merge-failures.svg)

![Rejected Submissions](charts/rejected-submissions.svg)

![Stream Backpressure Rejections](charts/rejected-stream-backpressure.svg)

![Offer Timeout Rejections](charts/rejected-offer-timeout.svg)

## 🔧 Primary Bottleneck Hypothesis

**Identified bottleneck:** none detected

**Notes:**

- No collapse detected; all tiers completed or ran with incomplete evidence.

## 📋 Log Evidence (Loki)

Per-tier log capture from Loki over the tier window plus a short post-window tail.
Full log streams are in `loki-captures.json`.

| Tier | Target TPS | Tier Window                                 | Capture Range                               | Query                   | Streams | Entries | Truncated | Error |
| ---- | ---------- | ------------------------------------------- | ------------------------------------------- | ----------------------- | ------- | ------- | --------- | ----- |
| 0    | 100        | 2026-05-29T07:40:38Z → 2026-05-29T07:52:38Z | 2026-05-29T07:40:38Z → 2026-05-29T07:53:38Z | `{job="containerlogs"}` | 2       | 2927    | false     |       |

## 🔍 Trace Evidence (Tempo)

Per-tier trace capture from Tempo over the full tier window (load phase + recovery).
Full trace summaries are in `tempo-captures.json`.

| Tier | Target TPS | Window                                      | Service      | Traces | Inspected | Truncated | Error |
| ---- | ---------- | ------------------------------------------- | ------------ | ------ | --------- | --------- | ----- |
| 0    | 100        | 2026-05-29T07:40:38Z → 2026-05-29T07:52:38Z | midgard-node | 250    | 250       | false     |       |

## 📂 Artifact Index

- `load-events.jsonl`
- `loki-captures.json`
- `prometheus-samples.json`
- `report.md`
- `run-manifest.json`
- `scenario.json`
- `stdout.log`
- `summary.json`
- `tempo-captures.json`
- `tier-summaries.jsonl`

## ⚠️ Limitations

- This run measures current behavior without remediation.
  No performance tuning, protocol changes, or infrastructure adjustments were applied.
- Results reflect a single run under the configured synthetic load profile.
  They may not reproduce identically under different hardware, network, or node state.
- This report does not claim or imply production scalability.
  Observed TPS figures are specific to the test configuration and should not be extrapolated.
- Enqueued TPS, mempool accepted TPS, and committed TPS are distinct pipeline boundaries.
  Conflating them overstates actual throughput.
- The legacy alias `tx_submissions_accepted_total` is intentionally excluded
  from this report to keep enqueue and durable-acceptance boundaries explicit.
- Bottleneck attribution is derived from counter deltas over the tier window.
  Scrape jitter, slow counters, or incomplete Prometheus data may reduce accuracy.
- Accepted-to-committed latency is a cohort estimate from counter alignment,
  not per-transaction tracing. It is bounded by Prometheus scrape cadence and window coverage.
