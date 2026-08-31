// Derives an incident log from the analysis + collected series: scrape gaps,
// node restarts, SLO-breach intervals and failure-counter spikes.

import type { ReliabilityAnalysis } from './analyze.js';
import type { CollectedData } from './collect.js';
import { counterIncrease, matrixToPoints } from './series.js';

export type IncidentKind = 'availability-gap' | 'restart' | 'slo-breach' | 'failure-spike';

export type Incident = {
  kind: IncidentKind;
  startedAt: string;
  endedAt: string | null;
  durationSeconds: number | null;
  severity: 'high' | 'medium' | 'low';
  summary: string;
};

const iso = (ms: number) => new Date(ms).toISOString();
const dur = (a: number, b: number) => Math.round((b - a) / 1000);

export function detectIncidents(
  analysis: ReliabilityAnalysis,
  data: CollectedData,
  stepSeconds: number
): Incident[] {
  const incidents: Incident[] = [];

  for (const gap of analysis.availability.scrapeGaps) {
    incidents.push({
      kind: 'availability-gap',
      startedAt: iso(gap.startMs),
      endedAt: iso(gap.endMs),
      durationSeconds: Math.max(stepSeconds, dur(gap.startMs, gap.endMs)),
      severity: 'high',
      summary: `Node exporter unavailable (up=0 / no scrape) for ~${Math.max(
        stepSeconds,
        dur(gap.startMs, gap.endMs)
      )}s.`,
    });
  }

  for (const t of analysis.availability.restartTimestamps) {
    incidents.push({
      kind: 'restart',
      startedAt: iso(t),
      endedAt: null,
      durationSeconds: null,
      severity: 'medium',
      summary: 'Node process restart (midgard_node_start_time_seconds changed).',
    });
  }

  for (const slo of analysis.slos) {
    for (const b of slo.breachIntervals) {
      const seconds = Math.max(stepSeconds, dur(b.startMs, b.endMs));
      incidents.push({
        kind: 'slo-breach',
        startedAt: iso(b.startMs),
        endedAt: iso(b.endMs),
        durationSeconds: seconds,
        severity: slo.objective >= 0.999 ? 'high' : 'medium',
        summary: `SLO ${slo.id} below objective ${formatObjective(slo)} for ~${seconds}s (rolling window).`,
      });
    }
  }

  // Failure-counter spikes: a single step > threshold between adjacent samples.
  const spikeSpecs: { key: string; label: string; threshold: number }[] = [
    { key: 'commit_commitment_failures', label: 'block commitment failures', threshold: 1 },
    { key: 'merge_failures', label: 'merge failures', threshold: 1 },
    { key: 'submit_block_failures', label: 'block submission failures', threshold: 3 },
    { key: 'tx_stream_fail', label: 'tx processing failures', threshold: 25 },
    { key: 'tx_stream_dead_letter', label: 'dead-letter transactions', threshold: 1 },
  ];
  for (const spec of spikeSpecs) {
    const snap = data.supportingRange.find((s) => s.key === spec.key);
    const points = matrixToPoints(snap?.result);
    for (let i = 1; i < points.length; i++) {
      const delta = points[i].v - points[i - 1].v;
      if (delta >= spec.threshold) {
        incidents.push({
          kind: 'failure-spike',
          startedAt: iso(points[i - 1].t),
          endedAt: iso(points[i].t),
          durationSeconds: dur(points[i - 1].t, points[i].t),
          severity: spec.threshold <= 1 ? 'high' : 'medium',
          summary: `Spike of ${Math.round(delta)} ${spec.label} within one sample interval.`,
        });
      }
    }
    const total = Math.round(counterIncrease(points));
    if (total > 0 && !points.some((p, i) => i > 0 && p.v - points[i - 1].v >= spec.threshold)) {
      incidents.push({
        kind: 'failure-spike',
        startedAt: data.window.from,
        endedAt: data.window.to,
        durationSeconds: null,
        severity: 'low',
        summary: `${total} ${spec.label} accumulated over the window (below per-sample spike threshold).`,
      });
    }
  }

  return incidents.sort((a, b) => a.startedAt.localeCompare(b.startedAt));
}

function formatObjective(slo: ReliabilityAnalysis['slos'][number]): string {
  return slo.kind === 'latency' ? `${slo.objective}s` : `${(slo.objective * 100).toFixed(3)}%`;
}
