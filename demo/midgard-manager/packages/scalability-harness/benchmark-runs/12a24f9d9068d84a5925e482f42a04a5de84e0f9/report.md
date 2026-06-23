# 📊 Sundial Fee Optimization Execution Report

## 📋 Executive Summary

This report summarizes the `fee-baseline-100-replay` benchmark for Sundial at Git commit `12a24f9d9068d84a5925e482f42a04a5de84e0f9` and compares it with the equivalent run at commit `ef887af1415f4a34fc85dd1e128bcdd1039b790d`.

The optimized run passed all formal checks while sustaining essentially unchanged throughput. It committed `92,742` L2 transactions using `91` submitted L1 commitment blocks, compared with `92,015` transactions using `508` submitted blocks in the baseline. Total L1 commitment fees fell from `116,410,740` to `20,853,105` lovelace, and the fee per committed L2 transaction fell from `1,265.13` to `224.85` lovelace: an **82.2% reduction**, or approximately **5.6x lower cost per committed L2 transaction**.

The `229,155` lovelace L1 commitment fee is amortized more efficiently across substantially fuller blocks. This reduced the number of required L1 commitment transactions by `82.1%` and the effective fee per committed L2 transaction by `82.2%`. The overall result is **Passed ✅** for the fee-optimization benchmark.

## 📌 Reference Baseline

The before/after comparison uses the same benchmark scenario, replay corpus, host, transaction profile, provider mode, target rate, load duration, and recovery duration.

| Field | Baseline | Optimized |
| :--- | :--- | :--- |
| Git commit | `ef887af1415f4a34fc85dd1e128bcdd1039b790d` | `12a24f9d9068d84a5925e482f42a04a5de84e0f9` |
| Run started | `2026-06-23T16:59:57.155Z` | `2026-06-23T20:09:35.628Z` |
| Scenario | `fee-baseline-100-replay` | `fee-baseline-100-replay` |
| Target load | `100 TPS` | `100 TPS` |
| Load / recovery | `900s / 300s` | `900s / 300s` |
| Transaction profile | `one-to-one` | `one-to-one` |
| L1 provider / wallet | `emulator / test-wallet` | `emulator / test-wallet` |
| Replay corpus SHA256 | `4c1485e9f69df29001b9c14e8017d0bf0301b377aca02f95cfedd9cb75a3ca80` | Same |
| Host | `dev3`, 6 CPUs, 31.0 GB RAM | Same |
| Result | Passed ✅ | Passed ✅ |

Baseline evidence: [`ef887af1 fee benchmark report`](../ef887af1415f4a34fc85dd1e128bcdd1039b790d/2026-06-23T16-59-57.155Z-fee-baseline-100-replay/report.md).

## 🔬 Test Scope

| Field | Value |
| :--- | :--- |
| System under test | Sundial node through the scalability harness |
| Commit under test | `12a24f9d9068d84a5925e482f42a04a5de84e0f9` |
| Transaction profile | `one-to-one` |
| L1 provider mode | `emulator` |
| Wallet mode | `test-wallet` |
| Node endpoint | `http://localhost:3000` |
| Prometheus endpoint | `http://localhost:9090` |
| Host | `dev3` Linux x64, 6 CPUs, 31.0 GB RAM |
| Harness version | `0.1.0` |
| Evidence directory | `midgard-manager/packages/scalability-harness/benchmark-runs/12a24f9d9068d84a5925e482f42a04a5de84e0f9` |

## 🏃 Executed Run

| Run | Started | Target Profile | Duration | Result | Evidence |
| :--- | :--- | :--- | :--- | :--- | :--- |
| `fee-baseline-100-replay` | 2026-06-23T20:09:35.628Z | Replay at `100 TPS` | 900s load + 300s recovery | Passed ✅ | [`report.md`](2026-06-23T20-09-35.628Z-fee-baseline-100-replay/report.md) |

## 💰 Fee and Throughput Comparison

| Metric | Baseline `ef887af1` | Optimized `12a24f9` | Change |
| :--- | ---: | ---: | ---: |
| Committed L2 transactions | 92,015 | 92,742 | +727 (+0.8%) |
| Committed throughput | 102.23 tx/s | 103.04 tx/s | +0.81 tx/s (+0.8%) |
| Submitted L1 commitment blocks | 508 | 91 | -417 (-82.1%) |
| Committed L2 tx per submitted block | 181.13 | 1,019.14 | 5.6x |
| Total L1 commitment fees | 116,410,740 lovelace | 20,853,105 lovelace | -95,557,635 (-82.1%) |
| Last L1 commitment fee | 229,155 lovelace | 229,155 lovelace | No change |
| L1 fee per committed L2 tx | 1,265.13 lovelace | 224.85 lovelace | -1,040.28 (-82.2%) |
| Peak queue | 41 | 395 | +354 |
| Peak mempool | 646 | 1,224 | +578 |
| Final queue after recovery | 0 | 0 | No change |
| Final mempool after recovery | 0 | 0 | No change |
| Commitment failures | 0 | 0 | No change |
| Merge failures | 0 | 0 | No change |

## 🚦 Formal Criteria Assessment

| Criterion Area | Evidence | Assessment |
| :--- | :--- | :--- |
| Run classification | All 10 configured policy checks passed in the optimized run. | Passed ✅ |
| Throughput preservation | Committed throughput changed from `102.23` to `103.04` tx/s at the same `100 TPS` target. | Passed ✅ |
| L1 fee efficiency | Fee per committed L2 transaction fell from `1,265.13` to `224.85` lovelace. | Improved by 82.2% ✅ |
| Commitment density | Average committed L2 transactions per submitted block increased from `181.13` to `1,019.14`. | Improved by 5.6x ✅ |
| Commitment reliability | Both runs recorded `0` commitment failures. | Passed ✅ |
| Merge reliability | Both runs recorded `0` merge failures. | Passed ✅ |
| Queue recovery | Both runs ended recovery with queue size `0`. | Passed ✅ |
| Mempool recovery | Both runs ended recovery with mempool size `0`. | Passed ✅ |
| Accepted-to-committed latency | Both runs reported p50/p95/p99 of `15s`, bounded by the Prometheus scrape interval. | No measured regression ✅ |

## 🔍 Key Findings

1. **Fee per committed L2 transaction decreased by 82.2%.**

   The optimized run spent `20,853,105` lovelace to commit `92,742` L2 transactions, or `224.85` lovelace per transaction. The baseline spent `116,410,740` lovelace to commit `92,015` transactions, or `1,265.13` lovelace per transaction.

2. **The saving came from fuller commitment blocks, not cheaper individual L1 transactions.**

   The last L1 commitment fee was exactly `229,155` lovelace in both runs. Submitted commitment blocks fell from `508` to `91`, closely matching the reduction in total fees. Average committed L2 transactions per submitted block increased from `181.13` to `1,019.14`.

3. **Throughput and recovery remained stable.**

   Committed throughput increased slightly from `102.23` to `103.04` tx/s. Both runs completed the full load phase, passed every formal check, recorded no commitment or merge failures, and drained both queue and mempool to zero during recovery.

4. **Batching increased transient queue and mempool occupancy.**

   Peak queue size increased from `41` to `395`, and peak mempool size increased from `646` to `1,224`. This is the expected operational cost of waiting for fuller tx-only commitment batches. Neither backlog persisted after recovery, and the measured accepted-to-committed p95 remained `15s` in both runs at the available scrape resolution.

## 🔄 Fee Improvements vs `ef887af1`

The optimized node introduces an explicit batching policy for tx-only commitment windows. When a window contains L2 transaction requests but no authenticated L1 user events, the commitment worker can wait until either the configured minimum transaction count is reached or the maximum wait time expires. The benchmark used a minimum target of `1,000` transaction requests and a maximum wait of `10,000ms`.

Authenticated L1 deposits, transaction orders, and withdrawals bypass the batching delay and continue to trigger immediate commitment processing. This keeps the fee optimization scoped to tx-only windows and avoids delaying L1-driven protocol events.

The policy preserves an upper commitment bound independently of the new minimum batch target. It therefore changes when a tx-only commitment is created without changing canonical transaction encoding, block root calculation, block lifecycle states, or the L1 fee calculation.

The measured result directly reflects this behavior: the baseline submitted `508` commitment blocks containing an average of `181.13` L2 transactions each, while the optimized run submitted `91` blocks containing an average of `1,019.14` transactions each. Because each observed L1 commitment cost `229,155` lovelace in both runs, reducing the number of submitted blocks reduced total commitment fees by the same `82.1%` order of magnitude.

The node also exposes commitment-window age and batch-wait skip metrics so operators can distinguish intentional batching delay from stalled commitment progress.

## 🔧 Bottleneck Analysis

No structural bottleneck or collapse was detected in either run. The optimized run sustained the requested `100 TPS`, committed at `103.04` tx/s, and fully recovered its transient queue and mempool backlog.

The relevant tradeoff is latency versus L1 cost. Fuller commitment batches require transactions to wait in queue or mempool until the minimum batch size or maximum wait threshold is reached. At this load level, the harness detected higher peak occupancy but no p95 latency regression beyond its approximately `15s` Prometheus resolution. Lower-load and mixed L1-event workloads still require separate validation because they may reach the maximum wait threshold more often.

## 📦 Evidence Artifacts

| Run | Key Artifacts |
| :--- | :--- |
| Optimized `12a24f9` | [`report.md`](2026-06-23T20-09-35.628Z-fee-baseline-100-replay/report.md), [`evidence archive`](2026-06-23T20-09-35.628Z-fee-baseline-100-replay/2026-06-23T20-09-35.628Z-fee-baseline-100-replay.zip) |
| Baseline `ef887af1` | [`report.md`](../ef887af1415f4a34fc85dd1e128bcdd1039b790d/2026-06-23T16-59-57.155Z-fee-baseline-100-replay/report.md), [`evidence archive`](../ef887af1415f4a34fc85dd1e128bcdd1039b790d/2026-06-23T16-59-57.155Z-fee-baseline-100-replay/2026-06-23T16-59-57.155Z-fee-baseline-100-replay.zip) |

## ⚠️ Limitations

- Results are from an emulator-backed test environment and must not be presented as mainnet production fee estimates.
- The comparison contains one run per commit under a synthetic `one-to-one` transaction profile; repeated runs are needed to establish variance.
- The optimization was validated at sustained `100 TPS`. Lower throughput may produce different batch density because the maximum wait threshold will trigger more often.
- Authenticated L1 user-event behavior was not exercised because request-events mode was off.
- Accepted-to-committed latency is estimated from Prometheus cohort alignment at approximately `15s` resolution, not from per-transaction timestamps.
- Peak queue and mempool occupancy increased in the optimized run, although both returned to zero during recovery.
- The unchanged emulator fee of `229,155` lovelace per commitment should not be extrapolated to dynamic production L1 fees.

## 💡 Recommendations

1. Treat commit `12a24f9` as **passed ✅** for the `100 TPS` fee-optimization benchmark.
2. Retain the explicit maximum-wait bound so fee batching cannot defer tx-only commitments indefinitely at low traffic.
3. Run repeated `100 TPS` samples to establish variance for fee per transaction, block density, and peak mempool occupancy.
4. Add lower-throughput and authenticated L1 user-event scenarios to validate the latency bound and immediate-event bypass behavior.
5. Monitor commitment-window age, batch-wait skips, queue size, and mempool size when applying these settings outside the benchmark environment.

## 🏁 Final Disposition

**Overall result: Passed ✅.**

At equivalent throughput and under the same benchmark profile, the optimized run reduced total L1 commitment fees by `82.1%` and fee per committed L2 transaction by `82.2%`. It achieved this by increasing average commitment density from `181.13` to `1,019.14` L2 transactions per submitted block while preserving full recovery and recording `0` commitment failures and `0` merge failures. The individual L1 commitment fee did not change; the improvement is attributable to submitting substantially fewer, fuller commitment blocks.
