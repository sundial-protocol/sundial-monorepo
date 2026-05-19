import { execSync } from 'node:child_process';
import { mkdir, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';

import type { PlanConfig } from '../config/plan.js';

export interface PlanManifest {
  planId: string;
  description: string | null;
  startedAt: string;
  gitSha: string;
  planPath: string;
  harnessVersion: string;
  scenarioPaths: string[];
  host: {
    hostname: string;
    platform: string;
    arch: string;
    cpus: number;
    totalMemoryBytes: number;
  };
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

export class PlanArtifactWriter {
  readonly planDir: string;

  private constructor(planDir: string) {
    this.planDir = planDir;
  }

  static async create(
    plan: PlanConfig,
    planPath: string,
    resolvedScenarioPaths: string[],
    harnessVersion: string
  ): Promise<PlanArtifactWriter> {
    const startedAt = new Date();
    const timestamp = toFilesystemTimestamp(startedAt);
    const absoluteOutputDir = path.isAbsolute(plan.outputDir)
      ? plan.outputDir
      : path.resolve(plan.outputDir);
    const planDir = path.join(absoluteOutputDir, `${timestamp}-${plan.planId}`);

    await mkdir(planDir, { recursive: true });

    const writer = new PlanArtifactWriter(planDir);

    await writeFile(path.join(planDir, 'plan.json'), JSON.stringify(plan, null, 2));

    const manifest: PlanManifest = {
      planId: plan.planId,
      description: plan.description ?? null,
      startedAt: startedAt.toISOString(),
      gitSha: gitSha(),
      planPath,
      harnessVersion,
      scenarioPaths: resolvedScenarioPaths,
      host: {
        hostname: os.hostname(),
        platform: os.platform(),
        arch: os.arch(),
        cpus: os.cpus().length,
        totalMemoryBytes: os.totalmem(),
      },
    };

    await writeFile(path.join(planDir, 'plan-manifest.json'), JSON.stringify(manifest, null, 2));

    return writer;
  }

  // Returns the path for a scenario run subdirectory within the plan directory.
  // index is 0-based; the directory name is 1-based and zero-padded (e.g. 01-warmup).
  scenarioRunDir(index: number, runId: string): string {
    const prefix = String(index + 1).padStart(2, '0');
    return path.join(this.planDir, `${prefix}-${runId}`);
  }

  async writePlanSummary(summary: unknown): Promise<void> {
    await writeFile(path.join(this.planDir, 'plan-summary.json'), JSON.stringify(summary, null, 2));
  }

  async writePlanReport(markdown: string): Promise<void> {
    await writeFile(path.join(this.planDir, 'plan-report.md'), markdown);
  }
}
