import { createHash } from 'node:crypto';
import { mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';

import { afterEach, describe, expect, it } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import { ArtifactWriter, type RunManifest } from '../../src/evidence/artifacts.js';

async function makeTempRoot(): Promise<string> {
  return mkdtemp(path.join(tmpdir(), 'scalability-harness-artifacts-'));
}

async function makeScenario(
  outputDir: string,
  replayCorpusPath?: string
): Promise<ScalabilityScenario> {
  return {
    runId: 'artifact-test-run',
    nodeEndpoint: 'http://localhost:3000',
    prometheusEndpoint: 'http://localhost:9090',
    outputDir,
    seed: 'artifact-test-seed',
    replayCorpusPath,
    transactionType: 'one-to-one',
    tierDurationSeconds: 60,
    recoverySeconds: 30,
    startTps: 100,
    maxTps: 100,
    stepMultiplier: 2,
    txGeneratorTaskCostSeconds: 0.2,
    retryAttempts: 0,
    retryDelayMs: 0,
    stopConditions: {
      maxConsecutiveNodeProbeFailures: 3,
      stopOnPrometheusDown: true,
      stopOnCommitmentFailure: true,
      stopOnMergeFailure: true,
    },
  };
}

const cleanupPaths: string[] = [];

afterEach(async () => {
  await Promise.all(cleanupPaths.splice(0).map((dir) => rm(dir, { recursive: true, force: true })));
});

describe('ArtifactWriter.create', () => {
  it('records replay corpus path and sha256 hash in run-manifest.json', async () => {
    const tempRoot = await makeTempRoot();
    cleanupPaths.push(tempRoot);

    const outputDir = path.join(tempRoot, 'benchmark-runs');
    await mkdir(outputDir, { recursive: true });

    const scenariosDir = path.join(tempRoot, 'scenarios');
    const corporaDir = path.join(scenariosDir, 'corpora');
    await mkdir(corporaDir, { recursive: true });

    const corpusPath = path.join(corporaDir, 'replay-corpus.json');
    const corpusContent = JSON.stringify(
      [
        {
          txId: 'replay-1',
          cborHex: '84a400',
          type: 'Midgard L2 User Transaction',
          description: 'replay tx 1',
        },
      ],
      null,
      2
    );
    await writeFile(corpusPath, corpusContent);

    const scenarioPath = path.join(scenariosDir, 'scenario.json');
    const scenario = await makeScenario(outputDir, 'corpora/replay-corpus.json');
    await writeFile(scenarioPath, JSON.stringify(scenario, null, 2));

    const writer = await ArtifactWriter.create(scenario, scenarioPath, '0.1.0');
    const manifestRaw = await readFile(path.join(writer.runDir, 'run-manifest.json'), 'utf8');
    const manifest = JSON.parse(manifestRaw) as RunManifest;

    const expectedSha = createHash('sha256').update(corpusContent).digest('hex');
    expect(manifest.replayCorpusPath).toBe(corpusPath);
    expect(manifest.replayCorpusSha256).toBe(expectedSha);
  });

  it('records null replay corpus metadata when replay corpus is not configured', async () => {
    const tempRoot = await makeTempRoot();
    cleanupPaths.push(tempRoot);

    const outputDir = path.join(tempRoot, 'benchmark-runs');
    await mkdir(outputDir, { recursive: true });

    const scenarioPath = path.join(tempRoot, 'scenario.json');
    const scenario = await makeScenario(outputDir);
    await writeFile(scenarioPath, JSON.stringify(scenario, null, 2));

    const writer = await ArtifactWriter.create(scenario, scenarioPath, '0.1.0');
    const manifestRaw = await readFile(path.join(writer.runDir, 'run-manifest.json'), 'utf8');
    const manifest = JSON.parse(manifestRaw) as RunManifest;

    expect(manifest.replayCorpusPath).toBeNull();
    expect(manifest.replayCorpusSha256).toBeNull();
  });
});
