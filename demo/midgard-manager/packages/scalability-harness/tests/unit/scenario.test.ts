import { readdir, readFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

import { ScenarioValidationError, validateScenario } from '../../src/config/scenario.js';

const VALID_SCENARIO = {
  runId: 'baseline-100-800',
  nodeEndpoint: 'http://localhost:3000',
  prometheusEndpoint: 'http://localhost:9090',
  outputDir: 'benchmark-runs',
  seed: 'test-seed',
  replayCorpusPath: 'scenarios/corpora/replay-baseline.json',
  transactionType: 'one-to-one',
  tierDurationSeconds: 60,
  recoverySeconds: 30,
  startTps: 100,
  maxTps: 800,
  stepMultiplier: 2,
  txGeneratorTaskCostSeconds: 0.2,
  retryAttempts: 3,
  retryDelayMs: 500,
  stopConditions: {
    maxConsecutiveNodeProbeFailures: 5,
    stopOnPrometheusDown: false,
    stopOnCommitmentFailure: true,
    stopOnMergeFailure: false,
  },
};

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const SCENARIOS_DIR = path.resolve(__dirname, '../../scenarios');

describe('validateScenario', () => {
  describe('valid config', () => {
    it('accepts a complete valid scenario', () => {
      expect(() => validateScenario(VALID_SCENARIO)).not.toThrow();
    });

    it('returns the scenario object on success', () => {
      const result = validateScenario(VALID_SCENARIO);
      expect(result.runId).toBe('baseline-100-800');
      expect(result.startTps).toBe(100);
      expect(result.maxTps).toBe(800);
    });

    it('accepts mixed transactionType with oneToOneRatio', () => {
      const s = { ...VALID_SCENARIO, transactionType: 'mixed', oneToOneRatio: 70 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts replayCorpusPath when provided', () => {
      const s = { ...VALID_SCENARIO, replayCorpusPath: 'corpora/replay-baseline.json' };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts pregenTransactionCount when provided', () => {
      const { replayCorpusPath: _, ...withoutCorpus } = VALID_SCENARIO;
      const s = { ...withoutCorpus, pregenTransactionCount: 100000 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('rejects pregenTransactionCount combined with replayCorpusPath', () => {
      const s = { ...VALID_SCENARIO, pregenTransactionCount: 100000 };
      expect(() => validateScenario(s)).toThrow(
        'pregenTransactionCount and replayCorpusPath are mutually exclusive'
      );
    });

    it('rejects pregenTransactionCount of 0', () => {
      const { replayCorpusPath: _, ...withoutCorpus } = VALID_SCENARIO;
      const s = { ...withoutCorpus, pregenTransactionCount: 0 };
      expect(() => validateScenario(s)).toThrow('pregenTransactionCount');
    });

    it('rejects non-integer pregenTransactionCount', () => {
      const { replayCorpusPath: _, ...withoutCorpus } = VALID_SCENARIO;
      const s = { ...withoutCorpus, pregenTransactionCount: 1000.5 };
      expect(() => validateScenario(s)).toThrow('pregenTransactionCount');
    });

    it('accepts https URLs', () => {
      const s = {
        ...VALID_SCENARIO,
        nodeEndpoint: 'https://node.example.com',
        prometheusEndpoint: 'https://prom.example.com',
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts startTps equal to maxTps (single-tier scenario)', () => {
      const s = { ...VALID_SCENARIO, startTps: 100, maxTps: 100 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts optional stopCondition fields', () => {
      const s = {
        ...VALID_SCENARIO,
        stopConditions: {
          ...VALID_SCENARIO.stopConditions,
          maxRecoveryQueueSize: 10000,
          maxRecoveryMempoolSize: 5000,
          minUsefulThroughputRatio: 0.5,
        },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts lokiPostWindowTailSeconds when provided', () => {
      const s = { ...VALID_SCENARIO, lokiPostWindowTailSeconds: 90 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts minCommitToAcceptedRatio as a node-health stop condition', () => {
      const s = {
        ...VALID_SCENARIO,
        stopConditions: { ...VALID_SCENARIO.stopConditions, minCommitToAcceptedRatio: 1.0 },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts minCommitToAcceptedRatio of 0', () => {
      const s = {
        ...VALID_SCENARIO,
        stopConditions: { ...VALID_SCENARIO.stopConditions, minCommitToAcceptedRatio: 0 },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts maxCommitmentFailureRatio as a stop condition', () => {
      const s = {
        ...VALID_SCENARIO,
        stopConditions: { ...VALID_SCENARIO.stopConditions, maxCommitmentFailureRatio: 0.0001 },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts maxUnsubmittedBlockBacklogGrowth as a stop condition', () => {
      const s = {
        ...VALID_SCENARIO,
        stopConditions: {
          ...VALID_SCENARIO.stopConditions,
          maxUnsubmittedBlockBacklogGrowth: 0,
        },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts ramp with multiply strategy', () => {
      const { stepMultiplier: _sm, ...withoutStepMultiplier } = VALID_SCENARIO;
      const s = {
        ...withoutStepMultiplier,
        ramp: { strategy: 'multiply', stepMultiplier: 1.5 },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts ramp with percent_increment strategy', () => {
      const { stepMultiplier: _sm, ...withoutStepMultiplier } = VALID_SCENARIO;
      const s = {
        ...withoutStepMultiplier,
        ramp: { strategy: 'percent_increment', percentIncrement: 25 },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts tierOverrides entries with per-tier duration or recovery overrides', () => {
      const s = {
        ...VALID_SCENARIO,
        tierOverrides: [
          { tierIndex: 0, durationSeconds: 120 },
          { tierIndex: 1, recoverySeconds: 45 },
        ],
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts zero for retryAttempts and retryDelayMs', () => {
      const s = { ...VALID_SCENARIO, retryAttempts: 0, retryDelayMs: 0 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts submitTimeoutMs when set to a positive number', () => {
      const s = { ...VALID_SCENARIO, submitTimeoutMs: 500 };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts a scenario without submitTimeoutMs (optional field)', () => {
      const { submitTimeoutMs: _, ...withoutTimeout } = { ...VALID_SCENARIO, submitTimeoutMs: 500 };
      expect(() => validateScenario(withoutTimeout)).not.toThrow();
    });

    it('accepts requestEvents mode when set to sampled', () => {
      const s = { ...VALID_SCENARIO, requestEvents: 'sampled' };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts runClassificationPolicy overrides when provided', () => {
      const s = {
        ...VALID_SCENARIO,
        runClassificationPolicy: {
          maxCollapsedTiers: 1,
          minCompletedTiers: 0,
          maxEvidenceIncompleteTiers: 2,
          minDurableThroughputRatio: 0.4,
          maxRejectedRatio: 0.03,
          maxProcessingFailedRatio: 0.02,
          maxFinalQueueSizeAfterRecovery: 10_000,
          maxFinalMempoolSizeAfterRecovery: 5_000,
        },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });

    it('accepts grafanaScreenshots config when enabled', () => {
      const s = {
        ...VALID_SCENARIO,
        grafanaScreenshots: {
          enabled: true,
          grafanaBaseUrl: 'http://localhost:3001',
          dashboardJsonPath: '../../../midgard-node/grafana/dashboard.json',
          theme: 'light',
          timezone: 'utc',
          peakCaptureCooldownSeconds: 60,
        },
      };
      expect(() => validateScenario(s)).not.toThrow();
    });
  });

  describe('missing required fields', () => {
    it('rejects null input', () => {
      expect(() => validateScenario(null)).toThrow(ScenarioValidationError);
    });

    it('rejects non-object input', () => {
      expect(() => validateScenario('not-an-object')).toThrow(ScenarioValidationError);
    });

    it('rejects missing runId', () => {
      const { runId: _r, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });

    it('rejects missing nodeEndpoint', () => {
      const { nodeEndpoint: _n, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });

    it('rejects missing stopConditions', () => {
      const { stopConditions: _sc, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });

    it('rejects missing startTps', () => {
      const { startTps: _s, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });

    it('rejects missing maxTps', () => {
      const { maxTps: _m, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });

    it('rejects missing stepMultiplier when ramp is not provided', () => {
      const { stepMultiplier: _sm, ...rest } = VALID_SCENARIO;
      expect(() => validateScenario(rest)).toThrow(ScenarioValidationError);
    });
  });

  describe('invalid URLs', () => {
    it('rejects bare hostname for nodeEndpoint', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, nodeEndpoint: 'localhost:3000' })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects ftp:// URL for nodeEndpoint', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, nodeEndpoint: 'ftp://localhost:3000' })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects non-string nodeEndpoint', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, nodeEndpoint: 3000 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects invalid prometheusEndpoint', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, prometheusEndpoint: 'localhost' })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects invalid grafanaScreenshots.grafanaBaseUrl when enabled', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          grafanaScreenshots: {
            enabled: true,
            grafanaBaseUrl: 'localhost:3001',
            dashboardJsonPath: '../../../midgard-node/grafana/dashboard.json',
          },
        })
      ).toThrow(ScenarioValidationError);
    });
  });

  describe('invalid Loki post-window tail config', () => {
    it('rejects negative lokiPostWindowTailSeconds', () => {
      const s = { ...VALID_SCENARIO, lokiPostWindowTailSeconds: -1 };
      expect(() => validateScenario(s)).toThrow(ScenarioValidationError);
    });

    it('rejects non-integer lokiPostWindowTailSeconds', () => {
      const s = { ...VALID_SCENARIO, lokiPostWindowTailSeconds: 0.5 };
      expect(() => validateScenario(s)).toThrow(ScenarioValidationError);
    });
  });

  describe('invalid numeric ranges', () => {
    it('rejects startTps of zero', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, startTps: 0 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects negative maxTps', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, maxTps: -1 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects maxTps less than startTps', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, startTps: 200, maxTps: 100 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects stepMultiplier of exactly 1', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, stepMultiplier: 1 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects stepMultiplier less than 1', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, stepMultiplier: 0.5 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects ramp.multiply stepMultiplier of exactly 1', () => {
      const { stepMultiplier: _sm, ...withoutStepMultiplier } = VALID_SCENARIO;
      expect(() =>
        validateScenario({
          ...withoutStepMultiplier,
          ramp: { strategy: 'multiply', stepMultiplier: 1 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects ramp.percent_increment of zero', () => {
      const { stepMultiplier: _sm, ...withoutStepMultiplier } = VALID_SCENARIO;
      expect(() =>
        validateScenario({
          ...withoutStepMultiplier,
          ramp: { strategy: 'percent_increment', percentIncrement: 0 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects tierDurationSeconds of zero', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, tierDurationSeconds: 0 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects non-positive txGeneratorTaskCostSeconds', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, txGeneratorTaskCostSeconds: 0 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects negative retryAttempts', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, retryAttempts: -1 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects submitTimeoutMs of zero', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, submitTimeoutMs: 0 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects submitTimeoutMs that is negative', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, submitTimeoutMs: -100 })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects stopConditions.maxConsecutiveNodeProbeFailures of zero', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: {
            ...VALID_SCENARIO.stopConditions,
            maxConsecutiveNodeProbeFailures: 0,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects minCommitToAcceptedRatio above 1', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: { ...VALID_SCENARIO.stopConditions, minCommitToAcceptedRatio: 1.1 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects minCommitToAcceptedRatio below 0', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: { ...VALID_SCENARIO.stopConditions, minCommitToAcceptedRatio: -0.1 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects non-finite minCommitToAcceptedRatio', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: {
            ...VALID_SCENARIO.stopConditions,
            minCommitToAcceptedRatio: Number.NaN,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects maxCommitmentFailureRatio above 1', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: { ...VALID_SCENARIO.stopConditions, maxCommitmentFailureRatio: 1.1 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects maxCommitmentFailureRatio below 0', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: { ...VALID_SCENARIO.stopConditions, maxCommitmentFailureRatio: -0.1 },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects non-finite maxCommitmentFailureRatio', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: {
            ...VALID_SCENARIO.stopConditions,
            maxCommitmentFailureRatio: Number.NaN,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects maxUnsubmittedBlockBacklogGrowth below 0', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          stopConditions: {
            ...VALID_SCENARIO.stopConditions,
            maxUnsubmittedBlockBacklogGrowth: -1,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects empty replayCorpusPath when provided', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          replayCorpusPath: '   ',
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects invalid requestEvents mode', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          requestEvents: 'sometimes',
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects runClassificationPolicy.minDurableThroughputRatio above 1', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          runClassificationPolicy: {
            minDurableThroughputRatio: 1.2,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects runClassificationPolicy.maxCollapsedTiers when negative', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          runClassificationPolicy: {
            maxCollapsedTiers: -1,
          },
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects tierOverrides entries without durationSeconds and recoverySeconds', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          tierOverrides: [{ tierIndex: 0 }],
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects tierOverrides entries with negative tierIndex', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          tierOverrides: [{ tierIndex: -1, durationSeconds: 120 }],
        })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects duplicate tierOverrides tierIndex values', () => {
      expect(() =>
        validateScenario({
          ...VALID_SCENARIO,
          tierOverrides: [
            { tierIndex: 1, durationSeconds: 120 },
            { tierIndex: 1, recoverySeconds: 30 },
          ],
        })
      ).toThrow(ScenarioValidationError);
    });
  });

  describe('invalid mixed-mode ratio', () => {
    it('rejects oneToOneRatio below 0', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, transactionType: 'mixed', oneToOneRatio: -1 })
      ).toThrow(ScenarioValidationError);
    });

    it('rejects oneToOneRatio above 100', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, transactionType: 'mixed', oneToOneRatio: 101 })
      ).toThrow(ScenarioValidationError);
    });

    it('accepts oneToOneRatio boundary values 0 and 100', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, transactionType: 'mixed', oneToOneRatio: 0 })
      ).not.toThrow();
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, transactionType: 'mixed', oneToOneRatio: 100 })
      ).not.toThrow();
    });
  });

  describe('invalid runId', () => {
    it('rejects empty runId', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, runId: '' })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects runId with spaces', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, runId: 'my run' })).toThrow(
        ScenarioValidationError
      );
    });

    it('rejects runId with slashes', () => {
      expect(() => validateScenario({ ...VALID_SCENARIO, runId: 'my/run' })).toThrow(
        ScenarioValidationError
      );
    });

    it('accepts alphanumeric runId with dashes and underscores', () => {
      expect(() =>
        validateScenario({ ...VALID_SCENARIO, runId: 'run-001_baseline' })
      ).not.toThrow();
    });
  });

  describe('scenario fixtures', () => {
    it('validates all scenario JSON files in scenarios/', async () => {
      const files = (await readdir(SCENARIOS_DIR)).filter((name) => name.endsWith('.json'));
      for (const fileName of files) {
        const filePath = path.join(SCENARIOS_DIR, fileName);
        const rawText = await readFile(filePath, 'utf8');
        const parsed = JSON.parse(rawText) as unknown;
        expect(() => validateScenario(parsed), fileName).not.toThrow();
      }
    });
  });
});
