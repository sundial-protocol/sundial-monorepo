# 📊 Sundial Scalability Execution Report

## 📋 Executive Summary

This report summarizes three scalability benchmark executions for Sundial/Sundial at Git commit `e0b938c463b081b4cb7e3a7ab2a0634dc8efe689`. The runs were executed with the scalability harness under `midgard-manager/packages/scalability-harness/benchmark-runs` and evaluated against the execution model, metrics, and acceptance criteria defined in [`internal-docs/scalability-stress-test-report.md`](https://github.com/sundial-protocol/internal-docs/blob/main/scalability-stress-test-report.md).

The evidence shows that the node accepted high L2 submission volume and completed short-window stepped tiers up to a nominal `800 TPS` target. However, the formal 30-minute `800 TPS` replay failed. During that run, the node durably accepted `1,858,055` transactions but committed only `787,733` before the recovery window ended, leaving `1,664,382` transactions in the mempool and recording `2` commitment failures. Under the plan's classification rules, the overall result is **Failed 🚫** for initial `800 TPS` sustained validation.

The strongest completed short-window tier was the baseline `800 TPS` tier, which observed `990.78` durable-accepted tx/s and `703.70` committed tx/s, with final queue and mempool recovery to zero. That result is useful engineering evidence, but it does not satisfy the plan's sustained initial target, which calls for an `800 TPS` validation window of approximately 30 minutes.

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
| System under test   | Sundial/Sundial node through the scalability harness                                                        |
| Commit under test   | `e0b938c463b081b4cb7e3a7ab2a0634dc8efe689`                                                                  |
| Transaction profile | `one-to-one`                                                                                                |
| L1 provider mode    | `emulator`                                                                                                  |
| Wallet mode         | `test-wallet`                                                                                               |
| Node endpoint       | `http://localhost:3000`                                                                                     |
| Prometheus endpoint | `http://localhost:9090`                                                                                     |
| Host                | `dev3` Linux x64, 6 CPUs, 31.0 GB RAM                                                                       |
| Harness version     | `0.1.0`                                                                                                     |
| Evidence directory  | `midgard-manager/packages/scalability-harness/benchmark-runs/e0b938c463b081b4cb7e3a7ab2a0634dc8efe689` |

## 🏃 Executed Runs

| Run                  | Started                  | Target Profile                            | Duration                          | Result    | Evidence                                                                                                         |
| :------------------- | :----------------------- | :---------------------------------------- | :-------------------------------- | :-------- | :--------------------------------------------------------------------------------------------------------------- |
| `warmup`             | 2026-05-19T16:29:27.803Z | `100 TPS` warm-up                         | 600s load + 120s recovery         | Passed ✅ | [`2026-05-19T16-29-27.803Z-warmup/report.md`](2026-05-19T16-29-27.803Z-warmup/report.md)                         |
| `baseline-100-800`   | 2026-05-20T15:07:15.936Z | Step ramp: `100`, `200`, `400`, `800 TPS` | 180s load + 90s recovery per tier | Passed ✅ | [`2026-05-20T15-07-15.936Z-baseline-100-800/report.md`](2026-05-20T15-07-15.936Z-baseline-100-800/report.md)     |
| `initial-800-replay` | 2026-05-21T13:16:18.076Z | Replay at `800 TPS`                       | 1800s load + 300s recovery        | Failed 🚫 | [`2026-05-21T13-16-18.076Z-initial-800-replay/report.md`](2026-05-21T13-16-18.076Z-initial-800-replay/report.md) |

The replay run used corpus `corpus-v1-net-preview-t-one-to-one-r-na-n-5000000.jsonl` with SHA256 `4c1485e9f69df29001b9c14e8017d0bf0301b377aca02f95cfedd9cb75a3ca80`.

## 📈 Aggregate Results

| Metric                                           |                                                      Total Across Runs |
| :----------------------------------------------- | ---------------------------------------------------------------------: |
| Client submitted transactions                    |                                                              2,268,770 |
| Prometheus enqueued transactions                 |                                                              2,261,696 |
| Prometheus durable mempool accepted transactions |                                                              2,251,978 |
| Committed L2 transactions                        |                                                              1,125,020 |
| Committed blocks                                 |                                                                    168 |
| Submitted blocks                                 |                                                                    163 |
| Merged blocks                                    | At least 9; warm-up merge count was not reported in the rendered table |
| Rejected submissions                             |                                                                      0 |
| Processing failures                              |                                                                      0 |
| Commitment failures                              |                                                                      2 |
| Merge failures                                   |                                                                      0 |
| Peak queue size                                  |                                                                  2,443 |
| Peak mempool size                                |                                                              1,664,382 |

## ⚡ Per-Run Throughput Summary

| Run / Tier                    | Target TPS | Result       | Enqueued Tx | Durable Accepted Tx | Committed Tx | Durable Accepted Tx/s | Committed Tx/s | Peak Queue | Peak Mempool | Final Mempool |
| :---------------------------- | ---------: | :----------- | ----------: | ------------------: | -----------: | --------------------: | -------------: | ---------: | -----------: | ------------: |
| `warmup` / tier 0             |        100 | completed ✅ |      59,520 |              59,497 |       57,400 |                 82.63 |          79.72 |         80 |        4,447 |             0 |
| `baseline-100-800` / tier 0   |        100 | completed ✅ |      22,381 |              22,327 |       21,989 |                124.03 |         122.15 |         69 |          344 |             0 |
| `baseline-100-800` / tier 1   |        200 | completed ✅ |      44,722 |              44,581 |       44,032 |                247.66 |         244.61 |        140 |        1,374 |             0 |
| `baseline-100-800` / tier 2   |        400 | completed ✅ |      89,399 |              89,167 |       87,193 |                495.34 |         484.38 |        291 |        4,810 |             0 |
| `baseline-100-800` / tier 3   |        800 | completed ✅ |     178,634 |             178,351 |      126,673 |                990.78 |         703.70 |      2,094 |       62,230 |             0 |
| `initial-800-replay` / tier 0 |        800 | collapsed 🚫 |   1,867,040 |           1,858,055 |      787,733 |              1,032.17 |         437.60 |      2,443 |    1,664,382 |     1,664,382 |

## 🚦 Formal Criteria Assessment

| Criterion Area            | Evidence                                                                                                                                                                           | Assessment                                                             |
| :------------------------ | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :--------------------------------------------------------------------- |
| API submission acceptance | No rejected, unavailable, or errored client submissions were reported in the rendered run reports.                                                                                 | Passed ✅ for valid generated load.                                    |
| Durable acceptance        | All runs durably accepted large transaction volume into `MempoolDB`.                                                                                                               | Passed ✅ as an ingress signal.                                        |
| Commitment progress       | Blocks continued to be committed in all runs.                                                                                                                                      | Passed ✅ as progress, failed 🚫 as sustained drain at long `800 TPS`. |
| Commitment failures       | The long `800 TPS` replay recorded `2` commitment failures.                                                                                                                        | Failed 🚫.                                                             |
| Merge failures            | All runs reported `0` merge failures.                                                                                                                                              | Passed ✅.                                                             |
| Queue recovery            | Final in-memory queue was `0` after recovery in all rendered reports.                                                                                                              | Passed ✅.                                                             |
| Mempool recovery          | The long `800 TPS` replay ended with `1,664,382` transactions still in mempool.                                                                                                    | Failed 🚫.                                                             |
| Inclusion latency         | Baseline tiers up to `400 TPS` reported p95 `15s`; baseline `800 TPS` reported p95 `75s`; replay `800 TPS` reported p95 `360s` with low confidence and only `41.3%` resolved.      | Failed 🚫 for sustained `800 TPS` target.                              |
| Resource saturation       | Harness-side resource flags did not mark CPU, memory, event-loop, or network saturation. Replay load phase host CPU was `70.49%`, below the configured `90%` saturation threshold. | No load-driver saturation indicated ✅.                                |

## 🔍 Key Findings

1. Short-window ramp testing reached the nominal `800 TPS` tier, but sustained validation did not pass.

   The `baseline-100-800` run completed all tiers and recovered the mempool after each tier. Its nominal `800 TPS` tier observed `990.78` durable-accepted tx/s and `703.70` committed tx/s. This confirms the node can ingest and partially drain high short-window load, but the result should not be presented as a successful 30-minute `800 TPS` benchmark.

2. The sustained replay exposed a block commitment drain bottleneck.

   In the `initial-800-replay` run, durable acceptance averaged `1,032.17` tx/s while committed throughput averaged `437.60` tx/s. The gap accumulated into `1,664,382` unrecovered mempool transactions after the 300-second recovery window. The run-level report identified the primary bottleneck as `block commitment (mempool did not recover after load)`.

3. Submission validation and HTTP-facing acceptance remained clean for the generated workload.

   Across the rendered reports, rejected submissions, node-unavailable outcomes, client errors, and processing failures remained at `0`. Client retries appeared only under the heavier tiers: `47` retries in the short `800 TPS` baseline tier and `8,873` total retries in the long replay.

4. Inclusion latency diverged from the plan's target at the `800 TPS` level.

   The internal plan identifies p95 mempool-accepted-to-committed latency `<= 20s` as the target where instrumentation supports measurement. The `100`, `200`, and `400 TPS` baseline tiers reported p95 `15s`, while the short `800 TPS` tier reported p95 `75s` and the long replay reported p95 `360s` with only `41.3%` of accepted transactions resolved before window end.

5. L1 fee per committed L2 transaction improved as block sizes increased, but the replay fee result is not a pass signal.

   The baseline ramp showed fee-per-committed-L2-tx decreasing from `489.85` lovelace at the `100 TPS` tier to `19.49` lovelace at the short `800 TPS` tier. The replay reported `3.42` lovelace per committed transaction, but that figure is attached to a failed run with an unrecovered mempool and should be treated as cost evidence only, not scalability validation.

## 🔧 Bottleneck Analysis

The dominant bottleneck is the block commitment and submission drain path rather than HTTP ingress or the immediate tx queue. The replay run accepted transactions into the durable mempool at more than `1,000` tx/s, kept the final in-memory queue at `0`, and did not report load-driver saturation. The failure occurred downstream: committed throughput did not keep pace with durable acceptance, commitment failures increased by `2`, and mempool depth did not recover.

This indicates that the next optimization pass should focus on:

- block construction and commitment worker duration under large mempool depth;
- transaction selection and batch sizing behavior;
- persistence cost in the commitment path;
- interaction between submitted, committed, and merged block progress;
- recovery behavior when durable acceptance outpaces committed throughput for extended windows.

## 📦 Evidence Artifacts

Each run directory contains the rendered report and chart exports. The zipped evidence packages also contain Prometheus samples, scenario metadata, run manifest, log captures, trace captures, tier summaries, load events, and summary JSON.

| Run                  | Key Artifacts                                                                                                                                                                                                          |
| :------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `warmup`             | [`report.md`](2026-05-19T16-29-27.803Z-warmup/report.md), [`2026-05-19T16-29-27.803Z-warmup.zip`](2026-05-19T16-29-27.803Z-warmup/2026-05-19T16-29-27.803Z-warmup.zip)                                                 |
| `baseline-100-800`   | [`report.md`](2026-05-20T15-07-15.936Z-baseline-100-800/report.md), [`2026-05-20T15-07-15.936Z-baseline-100-800.zip`](2026-05-20T15-07-15.936Z-baseline-100-800/2026-05-20T15-07-15.936Z-baseline-100-800.zip)         |
| `initial-800-replay` | [`report.md`](2026-05-21T13-16-18.076Z-initial-800-replay/report.md), [`2026-05-21T13-16-18.076Z-initial-800-replay.zip`](2026-05-21T13-16-18.076Z-initial-800-replay/2026-05-21T13-16-18.076Z-initial-800-replay.zip) |

## ⚠️ Limitations

- Results are from an emulator-backed test environment and must not be presented as mainnet production capacity.
- The executed profile was `one-to-one`; mixed payload validation remains outstanding under the formal plan.
- The successful `800 TPS` result was a short stepped tier, not the plan's sustained 30-minute initial TPS validation.
- Accepted-to-committed latency is estimated through Prometheus cohort alignment and is bounded by scrape cadence.
- The long replay had low latency-resolution confidence because only `41.3%` of accepted transactions were matched to committed progress before the window ended.
- Warm-up merged-block count was not rendered in the run report's commit/submit/merge table, so aggregate merged-block totals are reported as "at least" the sum visible in baseline and replay reports.

## 💡 Recommendations

1. Treat the current commit as **not passed 🚫** for formal sustained `800 TPS` validation.
2. Open a high-severity performance defect for unrecovered mempool growth and commitment failures in `initial-800-replay`.
3. Profile the block commitment worker and persistence calls under replay-sized mempool depth.
4. Add or review metrics for block commitment duration distribution, selected tx count per block, mempool drain latency, and commitment worker failure classes.
5. Repeat the `800 TPS` replay only after the commitment drain bottleneck is addressed, using the same corpus SHA256 for comparability.
6. After sustained one-to-one `800 TPS` passes, run the mixed-payload tier required by the internal plan before making broader scalability claims.

## 🏁 Final Disposition

**Overall result: Failed 🚫.**

The system demonstrated clean HTTP submission handling and short-window high-volume operation, including a completed nominal `800 TPS` ramp tier. It did not satisfy the internal plan's sustained initial `800 TPS` validation criteria. The decisive failure was the long replay's unrecovered mempool growth, with `1,664,382` transactions remaining after recovery and `2` commitment failures recorded.
