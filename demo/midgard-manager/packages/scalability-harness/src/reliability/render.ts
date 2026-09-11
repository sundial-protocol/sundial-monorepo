// Renders the reliability report (internal Markdown + HTML, and a redacted
// public-safe Markdown) from the analysis model.

import type { ReliabilityAnalysis, SloResult } from './analyze.js';
import type { ChartRecord } from './charts.js';
import type { CollectedData } from './collect.js';
import type { Incident } from './incidents.js';
import { redactText } from './redact.js';

export type RenderInput = {
  analysis: ReliabilityAnalysis;
  data: CollectedData;
  incidents: Incident[];
  charts: ChartRecord[];
  manifestName: string;
  evidenceFiles: string[];
  sloSourceRelPath: string;
  rollingWindow: string;
};

const pct = (v: number | null, digits = 3): string =>
  v === null ? 'n/a' : `${(v * 100).toFixed(digits)}%`;
const secs = (v: number | null): string => (v === null ? 'n/a' : `${v.toFixed(3)}s`);

function objectiveText(s: SloResult): string {
  return s.kind === 'latency' ? `p95 ≤ ${s.objective}s` : `≥ ${pct(s.objective)}`;
}
function achievedText(s: SloResult): string {
  return s.kind === 'latency' ? secs(s.achieved) : pct(s.achieved);
}
const mark = (v: SloResult['verdict']): string =>
  v === 'met' ? '✅ met' : v === 'missed' ? '⚠️ missed' : '— no data';

function sloComplianceTable(slos: SloResult[]): string {
  const rows = slos.map(
    (s) => `| \`${s.id}\` | ${objectiveText(s)} | ${achievedText(s)} | ${mark(s.verdict)} |`
  );
  return ['| SLI | Objective | Achieved | Verdict |', '| --- | --- | --- | --- |', ...rows].join(
    '\n'
  );
}

function errorBudgetTable(slos: SloResult[]): string {
  const rows = slos
    .filter((s) => s.errorBudget)
    .map((s) => {
      const eb = s.errorBudget!;
      return `| \`${s.id}\` | ${pct(eb.budget)} | ${(eb.consumedFraction * 100).toFixed(1)}% | ${(
        eb.remainingFraction * 100
      ).toFixed(1)}% |`;
    });
  if (rows.length === 0) return '_No ratio SLIs with data._';
  return [
    '| SLI | Error budget (window) | Consumed | Remaining |',
    '| --- | --- | --- | --- |',
    ...rows,
  ].join('\n');
}

function incidentTable(incidents: Incident[]): string {
  if (incidents.length === 0) return '_No incidents detected in the window._';
  const rows = incidents.map(
    (i) =>
      `| ${i.startedAt} | ${i.endedAt ?? '—'} | ${i.durationSeconds ?? '—'} | ${i.severity} | ${i.kind} | ${i.summary} |`
  );
  return [
    '| Started (UTC) | Ended (UTC) | Duration (s) | Severity | Kind | Summary |',
    '| --- | --- | --- | --- | --- | --- |',
    ...rows,
  ].join('\n');
}

function chartSection(charts: ChartRecord[]): string {
  const withData = charts.filter((c) => c.hasData);
  if (withData.length === 0) return '_No chart data available for this window._';
  return withData.map((c) => `**${c.title}**\n\n![${c.title}](${c.relPath})`).join('\n\n');
}

function lokiExcerpt(data: CollectedData): string {
  if (!data.loki) return '_Loki not configured for this report._';
  if (data.loki.error) return `_Loki query failed: ${data.loki.error}_`;
  const streams = data.loki.result?.streams ?? [];
  const lines: string[] = [];
  for (const s of streams) {
    for (const e of s.entries.slice(0, 40)) {
      lines.push(`${new Date(Number(e.timestampNs) / 1e6).toISOString()} ${e.line}`);
    }
  }
  const total = data.loki.result?.totalEntries ?? 0;
  const shown = Math.min(lines.length, 60);
  if (lines.length === 0) return `Matched **0** error/warn log lines in the window.`;
  return (
    `Matched **${total}** error/warn log lines${data.loki.result?.truncated ? ' (truncated)' : ''}. First ${shown}:\n\n` +
    '```\n' +
    lines.slice(0, 60).join('\n') +
    '\n```'
  );
}

export function renderMarkdown(input: RenderInput, opts: { public: boolean }): string {
  const { analysis: a, data, incidents } = input;
  const out: string[] = [];

  out.push(`# Sundial Node Reliability Report — ${data.environment}`);
  out.push('');
  out.push(`**Reporting window (UTC):** ${data.window.from} → ${data.window.to}`);
  out.push('');
  out.push(`**Disposition:** ${a.disposition}`);
  out.push('');

  // 1. Executive summary
  out.push('## 1. Executive summary');
  out.push('');
  out.push(sloComplianceTable(a.slos));
  out.push('');
  out.push(
    `- Availability (scrape \`up\`): **${pct(a.availability.fraction)}**, ` +
      `downtime ~${a.availability.downtimeSeconds}s across ${a.availability.scrapeGaps.length} gap(s).`
  );
  out.push(`- Node restarts in window: **${a.availability.restarts}**.`);
  out.push(
    `- Block cadence: **${a.cadence.meanBlocksPerSecond?.toFixed(3) ?? 'n/a'} blocks/s** mean, ` +
      `CV **${a.cadence.coefficientOfVariation?.toFixed(2) ?? 'n/a'}**.`
  );
  const failTotal = Object.values(a.counterTotals).reduce((x, y) => x + y, 0);
  out.push(
    `- Pipeline failures (commitment + merge + submit + tx + dead-letter): **${failTotal}**.`
  );
  out.push('');

  // 2. Methodology
  out.push('## 2. Methodology & environment');
  out.push('');
  out.push(`- Environment: \`${data.environment}\``);
  if (!opts.public) out.push(`- Prometheus: \`${data.prometheusEndpoint}\``);
  out.push(`- SLO definitions: \`${input.sloSourceRelPath}\` (shared with the Prometheus rules).`);
  out.push(`- Rolling window for timeseries / breach detection: ${input.rollingWindow}.`);
  out.push(
    '- Framing: Google SRE SLI/SLO + error budget, applied retrospectively over the closed window; ' +
      'four golden signals / RED for the request path.'
  );
  out.push(
    '- Each SLI is evaluated as a single whole-window aggregate PromQL query at the window end.'
  );
  out.push('');
  if (a.availability.builds.length > 0) {
    out.push('Node builds seen in the window:');
    out.push('');
    out.push('| Version | Commit | First seen (UTC) |');
    out.push('| --- | --- | --- |');
    for (const b of a.availability.builds)
      out.push(`| ${b.version} | ${b.commit} | ${b.firstSeen} |`);
    out.push('');
  }

  // 3. Provenance
  out.push('## 3. Data provenance');
  out.push('');
  if (opts.public) {
    out.push(
      'The full report is generated from a frozen evidence bundle (raw Prometheus range-query ' +
        'JSON, SLO definitions, checksums). The bundle is retained internally and available on request.'
    );
  } else {
    out.push(`- Evidence bundle manifest: \`${input.manifestName}\``);
    out.push(`- Files: ${input.evidenceFiles.map((f) => `\`${f}\``).join(', ')}`);
    out.push(
      '- Reproduce: `midgard-scalability-harness regen-reliability-report <bundle-dir>` ' +
        're-renders this report from the frozen data with no network calls.'
    );
  }
  out.push('');

  // 4. Transaction success rate
  out.push('## 4. Transaction success rate');
  out.push('');
  for (const s of a.slos.filter((x) => x.kind !== 'latency')) {
    out.push(`### \`${s.id}\``);
    out.push('');
    out.push(`${s.description}`);
    out.push('');
    out.push(
      `- Objective ${objectiveText(s)} — achieved **${achievedText(s)}** → ${mark(s.verdict)}`
    );
    if (s.errorBudget) {
      out.push(
        `- Error budget consumed **${(s.errorBudget.consumedFraction * 100).toFixed(1)}%** ` +
          `(remaining ${(s.errorBudget.remainingFraction * 100).toFixed(1)}%)`
      );
    }
    if (s.breachIntervals.length > 0) {
      out.push(`- Rolling-window breaches: ${s.breachIntervals.length}`);
    }
    out.push('');
  }
  out.push('Latency SLIs:');
  out.push('');
  out.push('| SLI | p50 | p95 | p99 | Objective | Verdict |');
  out.push('| --- | --- | --- | --- | --- | --- |');
  for (const s of a.slos.filter((x) => x.kind === 'latency')) {
    out.push(
      `| \`${s.id}\` | ${secs(s.quantiles?.['0.5'] ?? null)} | ${secs(s.quantiles?.['0.95'] ?? null)} | ` +
        `${secs(s.quantiles?.['0.99'] ?? null)} | p95 ≤ ${s.objective}s | ${mark(s.verdict)} |`
    );
  }
  out.push('');

  // 5. Network stability
  out.push('## 5. Network stability');
  out.push('');
  out.push(`- Availability (\`up\`): ${pct(a.availability.fraction)} over the window.`);
  out.push(
    `- Downtime: ~${a.availability.downtimeSeconds}s across ${a.availability.scrapeGaps.length} scrape gap(s).`
  );
  out.push(`- Restarts: ${a.availability.restarts}.`);
  out.push(
    `- Block cadence: mean ${a.cadence.meanBlocksPerSecond?.toFixed(3) ?? 'n/a'} blocks/s, ` +
      `CV ${a.cadence.coefficientOfVariation?.toFixed(2) ?? 'n/a'} over ${a.cadence.samples} samples.`
  );
  out.push('');
  out.push('Failure counter totals over the window:');
  out.push('');
  out.push('| Counter | Total |');
  out.push('| --- | --- |');
  for (const [k, v] of Object.entries(a.counterTotals)) out.push(`| \`${k}\` | ${v} |`);
  out.push('');
  out.push('Resource headroom (container aggregate):');
  out.push('');
  out.push(
    `- CPU: mean ${a.resources.cpuCoresMean?.toFixed(2) ?? 'n/a'} cores, peak ${a.resources.cpuCoresPeak?.toFixed(2) ?? 'n/a'}`
  );
  out.push(
    `- Memory: mean ${a.resources.memoryMiBMean?.toFixed(0) ?? 'n/a'} MiB, peak ${a.resources.memoryMiBPeak?.toFixed(0) ?? 'n/a'} MiB`
  );
  if (a.l1FeesLovelace !== null) out.push(`- L1 commitment fees: ${a.l1FeesLovelace} lovelace`);
  out.push('');
  out.push('### Charts');
  out.push('');
  out.push(chartSection(input.charts));
  out.push('');

  // 6. Incident log
  out.push('## 6. Incident log');
  out.push('');
  if (opts.public) {
    const bySev = { high: 0, medium: 0, low: 0 } as Record<string, number>;
    for (const i of incidents) bySev[i.severity]++;
    out.push(
      `Detected ${incidents.length} incident(s): ${bySev.high} high, ${bySev.medium} medium, ${bySev.low} low. ` +
        'Full timeline with evidence is in the internal report.'
    );
  } else {
    out.push(incidentTable(incidents));
  }
  out.push('');

  // 7. Error budget
  out.push('## 7. Error-budget accounting');
  out.push('');
  out.push(errorBudgetTable(a.slos));
  out.push('');

  // 8. Limitations
  out.push('## 8. Limitations');
  out.push('');
  out.push(
    '- SLIs are aggregated from Prometheus scrape data; sub-scrape-interval events are not visible.'
  );
  out.push(
    '- `l1_commitment_success` / `merge_success` are worker-failure ratios, not on-chain reorg/rollback tracking (a chain follower is future work).'
  );
  out.push(
    '- `tx_end_to_end_success` is noisy under short windows due to pipeline lag and is clamped to 1.'
  );
  out.push('- Availability is scrape-target health (`up`), not full external synthetic probing.');
  out.push('- Testnet environment; not a mainnet capacity or reliability certification.');
  out.push('');

  if (!opts.public) {
    // 9. Appendix
    out.push('## 9. Appendix');
    out.push('');
    out.push('### Error/warn log excerpt');
    out.push('');
    out.push(lokiExcerpt(data));
    out.push('');
    out.push('### SLI queries');
    out.push('');
    out.push('| SLI | Aggregate query |');
    out.push('| --- | --- |');
    for (const s of data.slo) {
      const q = s.aggregate.ratio?.query ?? s.aggregate['0.95']?.query ?? '';
      // Escape backslashes first so a literal '\' in the query cannot combine
      // with the following escape to un-escape a table pipe.
      out.push(`| \`${s.id}\` | \`${q.replace(/\\/g, '\\\\').replace(/\|/g, '\\|')}\` |`);
    }
    out.push('');
  }

  const md = out.join('\n') + '\n';
  return opts.public ? redactText(md) : md;
}

export function renderHtml(markdown: string, title: string): string {
  // Deliberately minimal: escape, wrap fenced code, headings, tables, images.
  const esc = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  const lines = markdown.split('\n');
  const html: string[] = [];
  let inCode = false;
  let inTable = false;
  const flushTable = () => {
    if (inTable) {
      html.push('</table>');
      inTable = false;
    }
  };
  for (const line of lines) {
    if (line.startsWith('```')) {
      flushTable();
      if (!inCode) {
        html.push('<pre><code>');
        inCode = true;
      } else {
        html.push('</code></pre>');
        inCode = false;
      }
      continue;
    }
    if (inCode) {
      html.push(esc(line));
      continue;
    }
    if (/^\|.*\|$/.test(line)) {
      if (/^\|[\s:-]+\|$/.test(line.replace(/[^|:\-\s]/g, ''))) continue; // separator row
      const cells = line
        .split('|')
        .slice(1, -1)
        .map((c) => c.trim());
      if (!inTable) {
        html.push('<table>');
        inTable = true;
      }
      html.push('<tr>' + cells.map((c) => `<td>${inlineHtml(esc(c))}</td>`).join('') + '</tr>');
      continue;
    }
    flushTable();
    const h = line.match(/^(#{1,6}) (.*)$/);
    if (h) {
      html.push(`<h${h[1].length}>${inlineHtml(esc(h[2]))}</h${h[1].length}>`);
      continue;
    }
    if (line.startsWith('- ')) {
      html.push(`<p class="li">• ${inlineHtml(esc(line.slice(2)))}</p>`);
      continue;
    }
    if (line.trim() === '') {
      html.push('');
      continue;
    }
    html.push(`<p>${inlineHtml(esc(line))}</p>`);
  }
  flushTable();
  if (inCode) html.push('</code></pre>');

  return `<!doctype html><html><head><meta charset="utf-8"><title>${esc(title)}</title>
<style>
body{font:14px/1.5 -apple-system,Segoe UI,Roboto,sans-serif;max-width:900px;margin:2rem auto;padding:0 1rem;color:#1a1a1a;background:#fff}
h1{font-size:1.6rem}h2{font-size:1.25rem;margin-top:2rem;border-bottom:1px solid #ddd;padding-bottom:.2rem}h3{font-size:1.05rem}
table{border-collapse:collapse;margin:.5rem 0;width:100%}td{border:1px solid #ccc;padding:.3rem .5rem;font-size:.92em}
tr:first-child td{background:#f3f3f3;font-weight:600}
pre{background:#f6f8fa;padding:.75rem;overflow-x:auto;border-radius:4px}code{font:12px/1.4 ui-monospace,Menlo,monospace}
img{max-width:100%;height:auto;background:#141414;border-radius:4px}
p.li{margin:.15rem 0}
@media(prefers-color-scheme:dark){body{background:#141414;color:#d8d8d8}h2{border-color:#333}td{border-color:#333}tr:first-child td{background:#222}pre{background:#1c1c1c}}
</style></head><body>
${html.join('\n')}
</body></html>
`;
}

function inlineHtml(s: string): string {
  // Character classes exclude the opening delimiters ('[', '(') as well as the
  // closing ones so a run of unmatched '![' / '((' cannot force a quadratic
  // rescan across every start position.
  return s
    .replace(/!\[([^\][]*)\]\(([^()]+)\)/g, '<img alt="$1" src="$2">')
    .replace(/\[([^\][]+)\]\(([^()]+)\)/g, '<a href="$2">$1</a>')
    .replace(/`([^`]+)`/g, '<code>$1</code>')
    .replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
}
