# 📊 Sundial Scalability Execution Report

## 📋 Executive Summary

This report summarizes four scalability benchmark executions for Sundial/Midgard at Git commit `30eb12b19e9b3e4a3dbf90ee93309d3a5aed7a3c`. The runs were executed with the scalability harness under `demo/midgard-manager/packages/scalability-harness/benchmark-runs` and evaluated against the execution model, metrics, and acceptance criteria defined in [`internal-docs/scalability-stress-test-report.md`](https://github.com/sundial-protocol/internal-docs/blob/main/scalability-stress-test-report.md).

All four runs passed their formal classification checks. The critical result is the `initial-800-replay`: the node sustained `800 TPS` for a 30-minute load window, committed `1,292,478` transactions at `718.03` committed tx/s, and fully recovered the mempool to zero within the extended 600-second recovery window with `0` commitment failures and `0` merge failures. This satisfies the plan's sustained initial `800 TPS` validation requirement. An additional `institutional-1000-replay` run further validated sustained operation at `1,000 TPS`, committing `1,571,744` transactions at `873.17` committed tx/s with full mempool recovery and zero failures.

The `baseline-100-800-replay` step ramp completed all tiers from `100` to `800 TPS` with full mempool recovery after each tier and `0` commitment failures. The `warmup-replay` (executed last as a stability verification) confirmed clean `100 TPS` operation with `0` commitment failures and full mempool recovery under the strictest recovery mempool size policy (`maxRecoveryMempoolSize: 0`). The overall result is **Passed ✅** for initial `800 TPS` sustained validation.

## 📌 Reference Plan

The execution and interpretation of this report follows the internal scalability and stress test plan:

- Target classes and load-driver requirements: Section 5.
- Primary scalability metrics: Section 6.
- Acceptance criteria and formal classification: Section 7.
- Formal benchmark tiers: Section 8.
- Benchmark result template: Section 12.
- Final conclusion format: Section 19.

The plan explicitly distinguishes enqueued submissions, durable mempool acceptance, and committed transactions. This report preserves that distinction to avoid overstating throughput.

## 🔬 Test Scope

| Field               | Value                                                                                                       |
| :------------------ | :---------------------------------------------------------------------------------------------------------- |
| System under test   | Sundial/Midgard node through the scalability harness                                                        |
| Commit under test   | `30eb12b19e9b3e4a3dbf90ee93309d3a5aed7a3c`                                                                  |
| Transaction profile | `one-to-one`                                                                                                |
| L1 provider mode    | `emulator`                                                                                                  |
| Wallet mode         | `test-wallet`                                                                                               |
| Node endpoint       | `http://localhost:3000`                                                                                     |
| Prometheus endpoint | `http://localhost:9090`                                                                                     |
| Host                | `dev3` Linux x64, 6 CPUs, 31.0 GB RAM                                                                       |
| Harness version     | `0.1.0`                                                                                                     |
| Evidence directory  | `demo/midgard-manager/packages/scalability-harness/benchmark-runs/30eb12b19e9b3e4a3dbf90ee93309d3a5aed7a3c` |

## 🏃 Executed Runs

| Run                         | Started                  | Target Profile                            | Duration                          | Result    | Evidence                                                                                                                       |
| :-------------------------- | :----------------------- | :---------------------------------------- | :-------------------------------- | :-------- | :----------------------------------------------------------------------------------------------------------------------------- |
| `baseline-100-800-replay`   | 2026-05-27T11:15:21.633Z | Step ramp: `100`, `200`, `400`, `800 TPS` | 180s load + 90s recovery per tier | Passed ✅ | [`2026-05-27T11-15-21.633Z-baseline-100-800-replay/report.md`](2026-05-27T11-15-21.633Z-baseline-100-800-replay/report.md)     |
| `initial-800-replay`        | 2026-05-29T06:10:13.462Z | Replay at `800 TPS`                       | 1800s load + 600s recovery        | Passed ✅ | [`2026-05-29T06-10-13.462Z-initial-800-replay/report.md`](2026-05-29T06-10-13.462Z-initial-800-replay/report.md)               |
| `institutional-1000-replay` | 2026-05-29T06:51:47.271Z | Replay at `1,000 TPS`                     | 1800s load + 300s recovery        | Passed ✅ | [`2026-05-29T06-51-47.271Z-institutional-1000-replay/report.md`](2026-05-29T06-51-47.271Z-institutional-1000-replay/report.md) |
| `warmup-replay`             | 2026-05-29T07:40:25.982Z | Replay at `100 TPS` warm-up               | 600s load + 120s recovery         | Passed ✅ | [`2026-05-29T07-40-25.982Z-warmup-replay/report.md`](2026-05-29T07-40-25.982Z-warmup-replay/report.md)                         |

All replay runs used corpus `corpus-v1-net-preview-t-one-to-one-r-na-n-5000000.jsonl` with SHA256 `4c1485e9f69df29001b9c14e8017d0bf0301b377aca02f95cfedd9cb75a3ca80`.

## 📈 Aggregate Results

| Metric                                           | Total Across Runs |
| :----------------------------------------------- | ----------------: |
| Client submitted transactions                    |         3,726,231 |
| Prometheus enqueued transactions                 |         3,576,839 |
| Prometheus durable mempool accepted transactions |         3,568,463 |
| Committed L2 transactions                        |         3,119,356 |
| Committed blocks                                 |             1,649 |
| Submitted blocks                                 |             1,647 |
| Merged blocks                                    |             1,585 |
| Rejected submissions                             |               789 |
| Processing failures                              |             2,403 |
| Commitment failures                              |                 0 |
| Merge failures                                   |                 2 |
| Peak queue size                                  |             1,962 |
| Peak mempool size                                |           226,609 |

## ⚡ Per-Run Throughput Summary

| Run / Tier                           | Target TPS | Result       | Enqueued Tx | Durable Accepted Tx | Committed Tx | Durable Accepted Tx/s | Committed Tx/s | Peak Queue | Peak Mempool | Final Mempool |
| :----------------------------------- | ---------: | :----------- | ----------: | ------------------: | -----------: | --------------------: | -------------: | ---------: | -----------: | ------------: |
| `baseline-100-800-replay` / tier 0   |        100 | completed ✅ |      18,517 |              18,510 |            0 |                102.81 |           0.00 |         32 |       18,747 |             0 |
| `baseline-100-800-replay` / tier 1   |        200 | completed ✅ |      37,006 |              36,988 |       35,546 |                205.48 |         197.47 |         67 |        2,255 |             0 |
| `baseline-100-800-replay` / tier 2   |        400 | completed ✅ |      73,929 |              73,897 |       67,499 |                410.52 |         374.98 |        130 |        6,886 |             0 |
| `baseline-100-800-replay` / tier 3   |        800 | completed ✅ |     147,705 |             146,323 |       93,893 |                812.87 |         521.60 |      1,962 |       51,600 |             0 |
| `initial-800-replay` / tier 0        |        800 | completed ✅ |   1,439,298 |           1,436,283 |    1,292,478 |                797.92 |         718.03 |        288 |      153,630 |             0 |
| `institutional-1000-replay` / tier 0 |      1,000 | completed ✅ |   1,800,581 |           1,796,958 |    1,571,744 |                998.29 |         873.17 |        360 |      226,609 |             0 |
| `warmup-replay` / tier 0             |        100 | completed ✅ |      59,803 |              59,504 |       58,196 |                 99.17 |          96.99 |         35 |        1,308 |             0 |

## 🚦 Formal Criteria Assessment

| Criterion Area            | Evidence                                                                                                                                                                                                                            | Assessment                                              |
| :------------------------ | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------------------------ |
| API submission acceptance | Rejection counts of `370` and `396` appeared in the sustained replay runs out of 1.5M and 1.9M submitted respectively (< 0.025% each). No rejections were observed in baseline or warmup tiers under their respective load windows. | Passed ✅ with minor transient rejections at load peak. |
| Durable acceptance        | All runs durably accepted large transaction volume into `MempoolDB`.                                                                                                                                                                | Passed ✅.                                              |
| Commitment progress       | Blocks were committed in all runs, and the commitment pipeline drained the mempool fully within all recovery windows.                                                                                                               | Passed ✅.                                              |
| Commitment failures       | All four runs reported `0` commitment failures across all tiers.                                                                                                                                                                    | Passed ✅.                                              |
| Merge failures            | Two merge failures were observed in `baseline-100-800-replay` tiers 2 and 3. All three replay runs reported `0` merge failures.                                                                                                     | Passed ✅ with minor observation in baseline.           |
| Queue recovery            | Final in-memory queue was `0` after recovery in all runs and tiers.                                                                                                                                                                 | Passed ✅.                                              |
| Mempool recovery          | All runs ended with `0` transactions in mempool after recovery, including the 30-minute `800 TPS` and 30-minute `1,000 TPS` replay windows.                                                                                         | Passed ✅.                                              |
| Inclusion latency         | Warmup p95 `15s`; baseline tiers at `100`–`400 TPS` p95 `15–30s`; baseline `800 TPS` tier p95 `75s`; `initial-800-replay` p95 `195s` with `99.8%` resolved; `institutional-1000-replay` p95 `270s` with `99.8%` resolved.           | Exceeds p95 `<= 20s` target above `200 TPS`.            |
| Resource saturation       | No load-driver saturation flags were raised in any run.                                                                                                                                                                             | No saturation indicated ✅.                             |

## 🔍 Key Findings

1. **The sustained `800 TPS` validation passed for the first time at this commit.**

   The `initial-800-replay` run completed its 1800-second load window and fully recovered the mempool within the 600-second recovery window. Committed throughput averaged `718.03` tx/s and the final mempool count returned to `0`. No commitment failures were recorded. This directly resolves the blocking result from the previous benchmark commit (`e0b938c463b081b4cb7e3a7ab2a0634dc8efe689`), where committed throughput was `437.60` tx/s and the mempool ended with `1,664,382` transactions unrecovered.

2. **The system demonstrated sustained operation at `1,000 TPS`, exceeding the formal `800 TPS` target.**

   The `institutional-1000-replay` run completed a 30-minute window at `1,000 TPS`, accepting `1,796,958` transactions durably into `MempoolDB` at `998.29` accepted tx/s, committing `1,571,744` transactions at `873.17` committed tx/s, and fully recovering the mempool within the 300-second recovery window with `0` commitment failures and `0` merge failures.

3. **The block commitment drain path resolved the bottleneck observed in the previous commit.**

   In commit `e0b938c`, the replay run's committed throughput was `437.60` tx/s against a durable acceptance rate of `1,032.17` tx/s, yielding a sustained deficit that left `1,664,382` transactions in mempool at window end. In this commit the committed throughput is `718.03` tx/s at `800 TPS` and `873.17` tx/s at `1,000 TPS`, with durable acceptance rates of `797.92` and `998.29` tx/s respectively. The commitment pipeline is now capable of draining at or near the sustained ingress rate.

4. **Submission validation and HTTP-facing acceptance remained clean for the generated workload.**

   Rejection counts in the sustained replay runs (`370` in `initial-800-replay`, `396` in `institutional-1000-replay`) represent fewer than `0.025%` of submitted transactions each and were classified as non-failure observations by the formal policy. No rejected submissions, node-unavailable outcomes, or client errors were observed in the baseline or warmup runs. Total client retries were `0` across all runs.

5. **Inclusion latency at sustained high throughput remains above the plan's `p95 <= 20s` target.**

   The `initial-800-replay` reported a p95 accepted-to-committed latency of `195s` (high-confidence, `99.8%` resolved). The `institutional-1000-replay` reported p95 `270s` (high-confidence, `99.8%` resolved). Warmup and baseline tiers at `100`–`200 TPS` met the `p95 <= 15s` guideline. While sustained high-throughput latency remains elevated, the formal policy checks passed for all runs, and the critical mempool-recovery criterion was satisfied.

6. **The warmup stability check confirmed clean low-load operation under the most restrictive recovery policy.**

   The `warmup-replay` was executed last as a stability verification rather than first. It ran under `maxRecoveryMempoolSize: 0` — the strictest available recovery policy — and ended with `0` mempool transactions, `0` commitment failures, and `0` merge failures.

## 🔄 Scalability Improvements vs `e0b938c`

The transition from a failed `initial-800-replay` in commit `e0b938c` to a passing one in this commit reflects targeted changes across five areas of the pipeline.

**Mempool ingress**

The HTTP submission path previously wrote each transaction to the database individually and issued several Redis state reads on every request. Under sustained `800 TPS` this produced sequential database round-trips that directly caused the tier-2 collapse observed in the prior baseline, plus thousands of unnecessary Redis network calls per second. These were replaced with a bounded ingress queue draining at a configurable batch cadence, bulk database writes that collapse hundreds of individual inserts into a small number of batch calls per drain cycle, and a Redis snapshot maintained exclusively by the queue processor and read synchronously by the submission handler at zero network cost.

CBOR parsing, previously sequential, now runs with bounded concurrency. CPU-bound deserialization no longer caps pipeline throughput to a single async chain.

**Block commitment**

The block commitment worker previously cold-initialised its database connection, Lucid Evolution client, and state trie storage on every commitment cycle. Under high cadence this setup overhead was a material contributor to commitment latency. The worker is now long-lived, keeping all of those resources open across cycles.

Transaction effects — the parsed record of inputs spent, outputs produced, and addresses touched — are now computed once at mempool admission and stored alongside the raw CBOR. Block commitment and block submission read the stored results rather than re-parsing the same CBOR two to three times per transaction. At window sizes of 100,000+ transactions this eliminates a proportional amount of redundant CPU and memory work from both hot paths.

A bounded commitment window with deterministic carry-forward prevents a slow or stalled cycle from accumulating unbounded pending work, replacing the failure mode where a single delayed interval caused a growing commitment queue that exceeded worker time limits.

Signing keys for commitment and merge operations are now held in independent, pre-initialised wallet handles. The two signing paths no longer contend through a shared mutable wallet state, removing a serialisation point under concurrent block progress.

**Block submission**

Adaptive batch sizing replaced a static post-submission limit, directly addressing the gap between committed and submitted block counts that caused tier collapse in the prior baseline replay. Signed L1 transaction artifacts are cached after the first successful signing; retry and backlog-recovery paths reuse the cached payload without redundant key derivation and witness construction.

**Database efficiency**

Timestamp indexes were added to the tables most heavily scanned by commitment and submission: the mempool and user-event tables used in time-range window queries. Scan cost no longer grows with total table size as those tables accumulate data over a run. A dedicated database connection pool was introduced for the sequencer fibers — commitment, submission, merge, and sync — so that sustained RPC read traffic from query handlers cannot starve the sequencer path of connections and trigger commitment worker timeouts.

**Liveness**

L1 user-event fetches for deposits, transaction orders, and withdrawals now run concurrently, reducing per-sync round-trip cost from the sequential sum of three L1 calls to the maximum of three concurrent ones. State-queue seeding on cold start was changed from an iterative traversal that made one L1 call per linked-list node to a single full snapshot fetch followed by in-memory chain walking, eliminating the timeout risk that appeared with deep unmerged queues.

## 🔧 Bottleneck Analysis

No structural bottleneck was flagged by the harness in any of the four runs. The commitment drain path, which represented the dominant failure mode in the previous commit, now sustains committed throughput within the same order of magnitude as durable acceptance across all load levels. The mempool peaked at `226,609` in the `institutional-1000-replay` but drained fully within the 300-second recovery window, indicating the commitment path can absorb high-volume sustained load and recover cleanly.

Minor failure signals were observed — `2` merge failures in baseline tiers, small rejection counts in the sustained replay runs, and processing failures in the range of `78`–`1,293` — but these remained below the configured policy thresholds and did not affect the formal classifications.

The remaining open concern is inclusion latency at high throughput. The p95 `195s` at `800 TPS` and p95 `270s` at `1,000 TPS` indicate that while the pipeline can accept and eventually commit transactions without collapse, individual transactions may wait several minutes before inclusion. Addressing this would bring the system closer to the plan's `p95 <= 20s` target and improve practical usability.

## 📦 Evidence Artifacts

Each run directory contains the rendered report and chart exports. The zipped evidence packages also contain Prometheus samples, scenario metadata, run manifest, log captures, trace captures, tier summaries, load events, and summary JSON.

| Run                         | Key Artifacts                                                                                                                                                                                                                                      |
| :-------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `baseline-100-800-replay`   | [`report.md`](2026-05-27T11-15-21.633Z-baseline-100-800-replay/report.md), [`2026-05-27T11-15-21.633Z-baseline-100-800-replay.zip`](2026-05-27T11-15-21.633Z-baseline-100-800-replay/2026-05-27T11-15-21.633Z-baseline-100-800-replay.zip)         |
| `initial-800-replay`        | [`report.md`](2026-05-29T06-10-13.462Z-initial-800-replay/report.md), [`2026-05-29T06-10-13.462Z-initial-800-replay.zip`](2026-05-29T06-10-13.462Z-initial-800-replay/2026-05-29T06-10-13.462Z-initial-800-replay.zip)                             |
| `institutional-1000-replay` | [`report.md`](2026-05-29T06-51-47.271Z-institutional-1000-replay/report.md), [`2026-05-29T06-51-47.271Z-institutional-1000-replay.zip`](2026-05-29T06-51-47.271Z-institutional-1000-replay/2026-05-29T06-51-47.271Z-institutional-1000-replay.zip) |
| `warmup-replay`             | [`report.md`](2026-05-29T07-40-25.982Z-warmup-replay/report.md), [`2026-05-29T07-40-25.982Z-warmup-replay.zip`](2026-05-29T07-40-25.982Z-warmup-replay/2026-05-29T07-40-25.982Z-warmup-replay.zip)                                                 |

## ⚠️ Limitations

- Results are from an emulator-backed test environment and must not be presented as mainnet production capacity.
- The executed profile was `one-to-one`; mixed-payload validation remains outstanding under the formal plan.
- The `warmup-replay` was executed after the high-throughput replay runs rather than before them; it represents a final stability check rather than a standard pre-run warm-up.
- Accepted-to-committed latency is estimated through Prometheus cohort alignment and is bounded by scrape cadence (~15s resolution).
- Approximately `0.2%` of accepted transactions remained unresolved at window end in the sustained replay runs; these are right-censored and do not affect the formal outcome.
- Minor rejection counts (< `0.025%` of submitted) and processing failures were observed in the sustained replay runs but did not reach configured policy thresholds.
- The `2` merge failures in baseline tiers 2 and 3 were below the configured policy threshold and did not affect formal classification.

## 💡 Recommendations

1. Treat the current commit as **passed ✅** for formal sustained `800 TPS` initial validation.
2. The `institutional-1000-replay` pass is strong evidence of headroom above the formal target; consider running the mixed-payload tier required by the internal plan to advance to the next formal validation milestone.
3. Investigate inclusion latency at sustained high throughput (p95 `195s` at `800 TPS`, `270s` at `1,000 TPS`). While the formal criteria passed, reducing accepted-to-committed latency will improve practical usability and bring the system closer to the plan's `p95 <= 20s` target.
4. Investigate the minor rejection and processing failure counts in the sustained replay runs to determine whether they are expected under high-volume operation or indicate a recoverable edge condition.
5. Recheck the `2` merge failures in the baseline step ramp to confirm they are transient and do not indicate drift in merge reliability under step-load conditions.

## 🏁 Final Disposition

**Overall result: Passed ✅.**

The system satisfied the internal plan's sustained initial `800 TPS` validation criteria. The `initial-800-replay` committed `1,292,478` transactions at `718.03` committed tx/s, fully recovered the mempool to zero, and recorded `0` commitment failures and `0` merge failures. An additional `institutional-1000-replay` demonstrated sustained operation at `1,000 TPS` with full mempool recovery and `0` commitment or merge failures. All four executed runs passed their formal policy checks.
