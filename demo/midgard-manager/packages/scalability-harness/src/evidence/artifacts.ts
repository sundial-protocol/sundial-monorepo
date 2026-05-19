import { execSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { appendFile, mkdir, readFile, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path, { dirname, isAbsolute, resolve } from 'node:path';

import type { ScalabilityScenario } from '../config/scenario.js';
import type {
  LoadDriverResourceEvidence,
  LoadDriverSaturationFlags,
} from '../runner/host-resources.js';
import type { SubmissionAggregate } from '../runner/tx-generator.js';
import type { LoadEvent } from './load-events.js';
import type { LokiTierCapture } from './loki.js';
import type { TempoTierCapture } from './tempo.js';

export interface RunManifest {
  runId: string;
  startedAt: string;
  gitSha: string;
  nodeEndpoint: string;
  prometheusEndpoint: string;
  scenarioPath: string;
  harnessVersion: string;
  replayCorpusPath: string | null;
  replayCorpusSha256: string | null;
  // L1 provider mode from the scenario. Null when the scenario did not specify one.
  l1ProviderMode: string | null;
  // Wallet mode from the scenario. Null when the scenario did not specify one.
  walletMode: string | null;
  // Optional wallet/UTxO provisioning note from the scenario.
  walletProvisioningNote: string | null;
  // Loki endpoint from the scenario. Null when Loki capture is not configured.
  lokiEndpoint: string | null;
  // Tempo endpoint from the scenario. Null when Tempo capture is not configured.
  tempoEndpoint: string | null;
  // Grafana screenshot capture enabled flag and dashboard source path, when configured.
  grafanaScreenshotsEnabled: boolean;
  grafanaDashboardJsonPath: string | null;
  host: {
    hostname: string;
    platform: string;
    arch: string;
    cpus: number;
    totalMemoryBytes: number;
  };
}

export interface TierSummary {
  tierIndex: number;
  targetTps: number;
  startedAt: string;
  stoppedAt: string;
  elapsedMs: number;
  reason: string;
  submissionAggregate?: SubmissionAggregate | null;
  loadDriverResourceEvidence?: LoadDriverResourceEvidence | null;
  loadDriverSaturationFlags?: LoadDriverSaturationFlags | null;
  [key: string]: unknown;
}

function gitSha(): string {
  try {
    return execSync('git rev-parse HEAD', { encoding: 'utf8' }).trim();
  } catch {
    return 'unknown';
  }
}

function toFilesystemTimestamp(date: Date): string {
  return date.toISOString().replace(/:/g, '-');
}

function resolveReplayCorpusPath(
  replayCorpusPath: string | undefined,
  scenarioPath: string
): string | null {
  if (replayCorpusPath === undefined) {
    return null;
  }
  if (isAbsolute(replayCorpusPath)) {
    return replayCorpusPath;
  }
  return resolve(dirname(scenarioPath), replayCorpusPath);
}

async function hashReplayCorpus(
  replayCorpusPath: string | undefined,
  scenarioPath: string
): Promise<{ replayCorpusPath: string | null; replayCorpusSha256: string | null }> {
  const resolvedReplayCorpusPath = resolveReplayCorpusPath(replayCorpusPath, scenarioPath);
  if (resolvedReplayCorpusPath === null) {
    return { replayCorpusPath: null, replayCorpusSha256: null };
  }

  const corpusContent = await readFile(resolvedReplayCorpusPath);
  const replayCorpusSha256 = createHash('sha256').update(corpusContent).digest('hex');
  return {
    replayCorpusPath: resolvedReplayCorpusPath,
    replayCorpusSha256,
  };
}

export class ArtifactWriter {
  readonly runDir: string;

  private constructor(runDir: string) {
    this.runDir = runDir;
  }

  // Creates a new run directory under scenario.outputDir named <timestamp>-<runId>.
  // Use this for standalone scenario runs.
  static async create(
    scenario: ScalabilityScenario,
    scenarioPath: string,
    harnessVersion: string
  ): Promise<ArtifactWriter> {
    const startedAt = new Date();
    const timestamp = toFilesystemTimestamp(startedAt);
    const runDir = path.join(scenario.outputDir, `${timestamp}-${scenario.runId}`);

    await mkdir(runDir, { recursive: false });

    const writer = new ArtifactWriter(runDir);

    await writeFile(path.join(runDir, 'scenario.json'), JSON.stringify(scenario, null, 2));

    const replayCorpus = await hashReplayCorpus(scenario.replayCorpusPath, scenarioPath);

    const manifest: RunManifest = {
      runId: scenario.runId,
      startedAt: startedAt.toISOString(),
      gitSha: gitSha(),
      nodeEndpoint: scenario.nodeEndpoint,
      prometheusEndpoint: scenario.prometheusEndpoint,
      scenarioPath,
      harnessVersion,
      replayCorpusPath: replayCorpus.replayCorpusPath,
      replayCorpusSha256: replayCorpus.replayCorpusSha256,
      l1ProviderMode: scenario.l1ProviderMode ?? null,
      walletMode: scenario.walletMode ?? null,
      walletProvisioningNote: scenario.walletProvisioningNote ?? null,
      lokiEndpoint: scenario.lokiEndpoint ?? null,
      tempoEndpoint: scenario.tempoEndpoint ?? null,
      grafanaScreenshotsEnabled: scenario.grafanaScreenshots?.enabled ?? false,
      grafanaDashboardJsonPath: scenario.grafanaScreenshots?.dashboardJsonPath ?? null,
      host: {
        hostname: os.hostname(),
        platform: os.platform(),
        arch: os.arch(),
        cpus: os.cpus().length,
        totalMemoryBytes: os.totalmem(),
      },
    };

    await writeFile(path.join(runDir, 'run-manifest.json'), JSON.stringify(manifest, null, 2));

    return writer;
  }

  // Creates a run directory at an explicit path rather than computing one from a timestamp.
  // Use this for plan-managed runs where the caller controls directory naming.
  static async createAt(
    runDir: string,
    scenario: ScalabilityScenario,
    scenarioPath: string,
    harnessVersion: string
  ): Promise<ArtifactWriter> {
    const startedAt = new Date();
    await mkdir(runDir, { recursive: true });

    const writer = new ArtifactWriter(runDir);

    await writeFile(path.join(runDir, 'scenario.json'), JSON.stringify(scenario, null, 2));

    const replayCorpus = await hashReplayCorpus(scenario.replayCorpusPath, scenarioPath);

    const manifest: RunManifest = {
      runId: scenario.runId,
      startedAt: startedAt.toISOString(),
      gitSha: gitSha(),
      nodeEndpoint: scenario.nodeEndpoint,
      prometheusEndpoint: scenario.prometheusEndpoint,
      scenarioPath,
      harnessVersion,
      replayCorpusPath: replayCorpus.replayCorpusPath,
      replayCorpusSha256: replayCorpus.replayCorpusSha256,
      l1ProviderMode: scenario.l1ProviderMode ?? null,
      walletMode: scenario.walletMode ?? null,
      walletProvisioningNote: scenario.walletProvisioningNote ?? null,
      lokiEndpoint: scenario.lokiEndpoint ?? null,
      tempoEndpoint: scenario.tempoEndpoint ?? null,
      grafanaScreenshotsEnabled: scenario.grafanaScreenshots?.enabled ?? false,
      grafanaDashboardJsonPath: scenario.grafanaScreenshots?.dashboardJsonPath ?? null,
      host: {
        hostname: os.hostname(),
        platform: os.platform(),
        arch: os.arch(),
        cpus: os.cpus().length,
        totalMemoryBytes: os.totalmem(),
      },
    };

    await writeFile(path.join(runDir, 'run-manifest.json'), JSON.stringify(manifest, null, 2));

    return writer;
  }

  private filePath(name: string): string {
    return path.join(this.runDir, name);
  }

  async appendLoadEvent(event: LoadEvent): Promise<void> {
    await appendFile(this.filePath('load-events.jsonl'), JSON.stringify(event) + '\n');
  }

  async appendTierSummary(summary: TierSummary): Promise<void> {
    await appendFile(this.filePath('tier-summaries.jsonl'), JSON.stringify(summary) + '\n');
  }

  async writePrometheusSamples(samples: unknown): Promise<void> {
    await writeFile(this.filePath('prometheus-samples.json'), JSON.stringify(samples, null, 2));
  }

  async writeSummary(summary: unknown): Promise<void> {
    await writeFile(this.filePath('summary.json'), JSON.stringify(summary, null, 2));
  }

  async writeReport(markdown: string): Promise<void> {
    await writeFile(this.filePath('report.md'), markdown);
  }

  async writeLokiCaptures(captures: LokiTierCapture[]): Promise<void> {
    await writeFile(this.filePath('loki-captures.json'), JSON.stringify(captures, null, 2));
  }

  async writeTempoCaptures(captures: TempoTierCapture[]): Promise<void> {
    await writeFile(this.filePath('tempo-captures.json'), JSON.stringify(captures, null, 2));
  }

  async logStdout(line: string): Promise<void> {
    await appendFile(this.filePath('stdout.log'), line + '\n');
  }

  async logStderr(line: string): Promise<void> {
    await appendFile(this.filePath('stderr.log'), line + '\n');
  }
}
