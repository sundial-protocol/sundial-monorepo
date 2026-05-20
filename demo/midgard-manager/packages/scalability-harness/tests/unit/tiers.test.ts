import { describe, expect, it } from 'vitest';

import type { ScalabilityScenario } from '../../src/config/scenario.js';
import { generateTiers } from '../../src/config/tiers.js';

function makeScenario(overrides: Partial<ScalabilityScenario>): ScalabilityScenario {
  return {
    runId: 'test',
    nodeEndpoint: 'http://localhost:3000',
    prometheusEndpoint: 'http://localhost:9090',
    outputDir: 'benchmark-runs',
    seed: 'test-seed',
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
    ...overrides,
  };
}

describe('generateTiers', () => {
  describe('exact max hit', () => {
    it('produces 100 → 200 → 400 → 800 when step is ×2', () => {
      const tiers = generateTiers(makeScenario({ startTps: 100, maxTps: 800, stepMultiplier: 2 }));
      expect(tiers.map((t) => t.targetTps)).toEqual([100, 200, 400, 800]);
    });

    it('assigns sequential tier indices starting at 0', () => {
      const tiers = generateTiers(makeScenario({ startTps: 100, maxTps: 800, stepMultiplier: 2 }));
      expect(tiers.map((t) => t.tierIndex)).toEqual([0, 1, 2, 3]);
    });

    it('propagates durationSeconds and recoverySeconds from scenario', () => {
      const tiers = generateTiers(makeScenario({ tierDurationSeconds: 120, recoverySeconds: 45 }));
      for (const tier of tiers) {
        expect(tier.durationSeconds).toBe(120);
        expect(tier.recoverySeconds).toBe(45);
      }
    });

    it('applies per-tier duration/recovery overrides when provided', () => {
      const tiers = generateTiers(
        makeScenario({
          startTps: 100,
          maxTps: 400,
          stepMultiplier: 2,
          tierDurationSeconds: 120,
          recoverySeconds: 45,
          tierOverrides: [
            { tierIndex: 1, durationSeconds: 180 },
            { tierIndex: 2, recoverySeconds: 90 },
          ],
        })
      );
      expect(tiers.map((t) => [t.durationSeconds, t.recoverySeconds])).toEqual([
        [120, 45],
        [180, 45],
        [120, 90],
      ]);
    });
  });

  describe('skipped max', () => {
    it('appends maxTps as the final tier when multiplication skips over it', () => {
      const tiers = generateTiers(makeScenario({ startTps: 100, maxTps: 700, stepMultiplier: 2 }));
      expect(tiers.map((t) => t.targetTps)).toEqual([100, 200, 400, 700]);
    });

    it('includes maxTps exactly once even when it falls between multiplier steps', () => {
      const tiers = generateTiers(makeScenario({ startTps: 100, maxTps: 300, stepMultiplier: 2 }));
      const tpsValues = tiers.map((t) => t.targetTps);
      expect(tpsValues.filter((v) => v === 300)).toHaveLength(1);
      expect(tpsValues[tpsValues.length - 1]).toBe(300);
    });
  });

  describe('non-integer multiplier', () => {
    it('handles ×1.5 steps correctly and includes maxTps as final tier', () => {
      const tiers = generateTiers(
        makeScenario({ startTps: 100, maxTps: 300, stepMultiplier: 1.5 })
      );
      const tpsValues = tiers.map((t) => t.targetTps);
      expect(tpsValues[0]).toBe(100);
      expect(tpsValues[tpsValues.length - 1]).toBe(300);
      expect(tpsValues.length).toBeGreaterThan(1);
    });

    it('never produces a tier above maxTps', () => {
      const tiers = generateTiers(
        makeScenario({ startTps: 100, maxTps: 500, stepMultiplier: 1.5 })
      );
      for (const tier of tiers) {
        expect(tier.targetTps).toBeLessThanOrEqual(500);
      }
    });
  });

  describe('ramp strategies', () => {
    it('supports ramp.strategy=multiply', () => {
      const tiers = generateTiers(
        makeScenario({
          stepMultiplier: undefined,
          ramp: { strategy: 'multiply', stepMultiplier: 1.5 },
          startTps: 100,
          maxTps: 300,
        })
      );
      expect(tiers.map((t) => t.targetTps)).toEqual([100, 150, 225, 300]);
    });

    it('supports ramp.strategy=percent_increment', () => {
      const tiers = generateTiers(
        makeScenario({
          stepMultiplier: undefined,
          ramp: { strategy: 'percent_increment', percentIncrement: 25 },
          startTps: 1000,
          maxTps: 2000,
        })
      );
      expect(tiers.map((t) => t.targetTps)).toEqual([1000, 1250, 1563, 1954, 2000]);
    });
  });

  describe('single-tier scenario', () => {
    it('produces exactly one tier when startTps equals maxTps', () => {
      const tiers = generateTiers(makeScenario({ startTps: 100, maxTps: 100, stepMultiplier: 2 }));
      expect(tiers).toHaveLength(1);
      expect(tiers[0].targetTps).toBe(100);
      expect(tiers[0].tierIndex).toBe(0);
    });
  });

  describe('determinism', () => {
    it('produces the same tiers for identical inputs', () => {
      const scenario = makeScenario({ startTps: 100, maxTps: 800, stepMultiplier: 2 });
      const first = generateTiers(scenario);
      const second = generateTiers(scenario);
      expect(first).toEqual(second);
    });

    it('produces deterministic seeds in the format seed:tier:N:tps:T', () => {
      const tiers = generateTiers(
        makeScenario({ seed: 'my-seed', startTps: 100, maxTps: 200, stepMultiplier: 2 })
      );
      expect(tiers[0].seed).toBe('my-seed:tier:0:tps:100');
      expect(tiers[1].seed).toBe('my-seed:tier:1:tps:200');
    });

    it('seeds change when run seed changes', () => {
      const a = generateTiers(
        makeScenario({ seed: 'seed-a', startTps: 100, maxTps: 200, stepMultiplier: 2 })
      );
      const b = generateTiers(
        makeScenario({ seed: 'seed-b', startTps: 100, maxTps: 200, stepMultiplier: 2 })
      );
      expect(a[0].seed).not.toBe(b[0].seed);
    });

    it('tier TPS values are independent of seed', () => {
      const a = generateTiers(makeScenario({ seed: 'seed-a' }));
      const b = generateTiers(makeScenario({ seed: 'seed-b' }));
      expect(a.map((t) => t.targetTps)).toEqual(b.map((t) => t.targetTps));
    });
  });

  describe('replay slice assignment', () => {
    it('splits pre-generated corpus evenly across tiers using disjoint slices', () => {
      const tiers = generateTiers(
        makeScenario({
          startTps: 100,
          maxTps: 800,
          stepMultiplier: 2,
          pregenTransactionCount: 1_500_000,
        })
      );

      expect(tiers.map((t) => t.replayCount)).toEqual([375_000, 375_000, 375_000, 375_000]);
      expect(tiers.map((t) => t.replayStartIndex)).toEqual([0, 375_000, 750_000, 1_125_000]);
    });
  });
});
