// Pulls every series the reliability report needs for its window from
// Prometheus (and optionally Loki). The returned object is also the frozen
// evidence payload written into the bundle.

import { LokiClient, type LokiQueryResult } from '../evidence/loki.js';
import {
  PrometheusClient,
  type PrometheusMatrixResult,
  type PrometheusVectorResult,
} from '../metrics/prometheus.js';
import type { ReliabilityConfig } from './config.js';
import { windowRangeString } from './config.js';
import {
  gaugeAvgExpr,
  latencyQuantileExpr,
  latencyRollingExpr,
  ratioAggregateExpr,
  ratioRollingExpr,
  type SloConfig,
} from './slo.js';

export type RangeSnapshot = {
  key: string;
  query: string;
  result: PrometheusMatrixResult | null;
  error?: string;
};

export type InstantSnapshot = {
  key: string;
  query: string;
  result: PrometheusVectorResult | null;
  error?: string;
};

export type SloSeries = {
  id: string;
  kind: 'ratio' | 'latency' | 'gauge_avg';
  objective: number;
  description: string;
  /** Whole-window aggregate(s). For latency, keyed by quantile ("0.95"). */
  aggregate: Record<string, InstantSnapshot>;
  /** Rolling timeseries for charts + breach detection. */
  rolling: Record<string, RangeSnapshot>;
};

export type CollectedData = {
  collectedAt: string;
  window: { from: string; to: string; label: string };
  environment: string;
  prometheusEndpoint: string;
  sloSourcePath: string;
  slo: SloSeries[];
  supportingInstant: InstantSnapshot[];
  supportingRange: RangeSnapshot[];
  loki?: {
    query: string;
    result: LokiQueryResult | null;
    error?: string;
  };
};

const SUPPORTING_RANGE_QUERIES: Record<string, string> = {
  up: 'up{job="sundial_nodes"}',
  start_time: 'max(midgard_node_start_time_seconds{job="sundial_nodes"})',
  commit_block_rate_5m: 'sum(rate(commit_block_count_total{job="sundial_nodes"}[5m]))',
  commit_commitment_failures: 'sum(commit_block_commitment_failures_total{job="sundial_nodes"})',
  merge_failures: 'sum(merge_block_failures_total{job="sundial_nodes"})',
  submit_block_failures: 'sum(submit_block_failures_total{job="sundial_nodes"})',
  tx_stream_fail: 'sum(tx_stream_fail_total{job="sundial_nodes"})',
  tx_stream_dead_letter: 'sum(tx_stream_dead_letter_total{job="sundial_nodes"})',
  tx_stream_consumer_lag: 'max(tx_stream_consumer_lag{job="sundial_nodes"})',
  mempool_tx_count: 'max(mempool_tx_count{job="sundial_nodes"})',
  l1_commitment_fees_lovelace: 'sum(l1_commitment_fees_lovelace_total{job="sundial_nodes"})',
  container_cpu: 'sum(rate(container_cpu_user_seconds_total{image!=""}[5m]))',
  container_memory_mib: 'sum(container_memory_usage_bytes{image!=""}) / 1024 / 1024',
};

const SUPPORTING_INSTANT_QUERIES: Record<string, string> = {
  build_info: 'midgard_node_build_info{job="sundial_nodes"}',
};

async function range(
  client: PrometheusClient,
  key: string,
  query: string,
  from: Date,
  to: Date,
  step: number
): Promise<RangeSnapshot> {
  try {
    return { key, query, result: await client.queryRange(query, from, to, step) };
  } catch (err) {
    return { key, query, result: null, error: err instanceof Error ? err.message : String(err) };
  }
}

async function instant(
  client: PrometheusClient,
  key: string,
  query: string,
  at: Date
): Promise<InstantSnapshot> {
  try {
    return { key, query, result: await client.queryInstant(query, at) };
  } catch (err) {
    return { key, query, result: null, error: err instanceof Error ? err.message : String(err) };
  }
}

export async function collect(
  config: ReliabilityConfig,
  slo: SloConfig,
  deps: { prometheus?: PrometheusClient; loki?: LokiClient } = {}
): Promise<CollectedData> {
  const client = deps.prometheus ?? new PrometheusClient(config.prometheusEndpoint);
  const { from, to } = config.window;
  const step = config.stepSeconds;
  const wholeWindow = windowRangeString(config.window);

  const sloSeries: SloSeries[] = [];
  for (const s of slo.slos) {
    const aggregate: Record<string, InstantSnapshot> = {};
    const rolling: Record<string, RangeSnapshot> = {};
    if (s.kind === 'ratio') {
      aggregate.ratio = await instant(
        client,
        `${s.id}:aggregate`,
        ratioAggregateExpr(s, wholeWindow),
        to
      );
      rolling.ratio = await range(
        client,
        `${s.id}:rolling`,
        ratioRollingExpr(s, config.rollingWindow),
        from,
        to,
        step
      );
      sloSeries.push({
        id: s.id,
        kind: s.kind,
        objective: s.objective,
        description: s.description,
        aggregate,
        rolling,
      });
    } else if (s.kind === 'latency') {
      for (const q of slo.latencyQuantiles) {
        aggregate[String(q)] = await instant(
          client,
          `${s.id}:p${q}`,
          latencyQuantileExpr(s, q, wholeWindow),
          to
        );
        rolling[String(q)] = await range(
          client,
          `${s.id}:p${q}:rolling`,
          latencyRollingExpr(s, q, config.rollingWindow),
          from,
          to,
          step
        );
      }
      sloSeries.push({
        id: s.id,
        kind: s.kind,
        objective: s.objectiveSeconds,
        description: s.description,
        aggregate,
        rolling,
      });
    } else {
      aggregate.ratio = await instant(
        client,
        `${s.id}:aggregate`,
        gaugeAvgExpr(s, wholeWindow),
        to
      );
      rolling.ratio = await range(
        client,
        `${s.id}:rolling`,
        gaugeAvgExpr(s, config.rollingWindow),
        from,
        to,
        step
      );
      sloSeries.push({
        id: s.id,
        kind: s.kind,
        objective: s.objective,
        description: s.description,
        aggregate,
        rolling,
      });
    }
  }

  const supportingRange = await Promise.all(
    Object.entries(SUPPORTING_RANGE_QUERIES).map(([k, q]) => range(client, k, q, from, to, step))
  );
  const supportingInstant = await Promise.all(
    Object.entries(SUPPORTING_INSTANT_QUERIES).map(([k, q]) => instant(client, k, q, to))
  );

  let loki: CollectedData['loki'];
  if (config.lokiEndpoint !== undefined) {
    const lokiClient = deps.loki ?? new LokiClient(config.lokiEndpoint);
    const query =
      '{job="containerlogs"} |~ `(?i)\\b(error|warn|panic|fatal|exception|unhandled)\\b`';
    try {
      loki = { query, result: await lokiClient.queryRange(query, from, to, 5_000) };
    } catch (err) {
      loki = { query, result: null, error: err instanceof Error ? err.message : String(err) };
    }
  }

  return {
    collectedAt: new Date().toISOString(),
    window: { from: from.toISOString(), to: to.toISOString(), label: config.window.label },
    environment: config.environment,
    prometheusEndpoint: config.prometheusEndpoint,
    sloSourcePath: slo.sourcePath,
    slo: sloSeries,
    supportingInstant,
    supportingRange,
    loki,
  };
}
