import type { ScalabilityScenario } from './scenario.js';

export interface LoadTier {
  tierIndex: number;
  targetTps: number;
  durationSeconds: number;
  recoverySeconds: number;
  seed: string;
}

function roundTargetTps(value: number): number {
  return Math.max(1, Math.round(value));
}

function nextTargetTps(scenario: ScalabilityScenario, currentTps: number): number {
  if (scenario.ramp?.strategy === 'percent_increment') {
    const multiplier = 1 + scenario.ramp.percentIncrement / 100;
    return roundTargetTps(currentTps * multiplier);
  }
  if (scenario.ramp?.strategy === 'multiply') {
    return roundTargetTps(currentTps * scenario.ramp.stepMultiplier);
  }
  return roundTargetTps(currentTps * (scenario.stepMultiplier ?? 1));
}

function resolveTierTimings(
  scenario: ScalabilityScenario,
  tierIndex: number
): { durationSeconds: number; recoverySeconds: number } {
  const base = {
    durationSeconds: scenario.tierDurationSeconds,
    recoverySeconds: scenario.recoverySeconds,
  };
  const override = scenario.tierOverrides?.find((v) => v.tierIndex === tierIndex);
  if (override === undefined) {
    return base;
  }
  return {
    durationSeconds: override.durationSeconds ?? base.durationSeconds,
    recoverySeconds: override.recoverySeconds ?? base.recoverySeconds,
  };
}

export function generateTiers(scenario: ScalabilityScenario): LoadTier[] {
  const { startTps, maxTps, seed } = scenario;

  const tiers: LoadTier[] = [];
  let tps = roundTargetTps(startTps);
  const maxRoundedTps = roundTargetTps(maxTps);

  while (true) {
    const tierIndex = tiers.length;
    const tierTiming = resolveTierTimings(scenario, tierIndex);
    tiers.push({
      tierIndex,
      targetTps: tps,
      durationSeconds: tierTiming.durationSeconds,
      recoverySeconds: tierTiming.recoverySeconds,
      seed: `${seed}:tier:${tierIndex}:tps:${tps}`,
    });

    if (tps >= maxRoundedTps) {
      break;
    }

    const next = nextTargetTps(scenario, tps);

    if (next >= maxRoundedTps) {
      // Include maxTps as the final tier when multiplication skips over it
      tps = maxRoundedTps;
    } else {
      if (next <= tps) {
        tps = maxRoundedTps;
        continue;
      }
      tps = next;
    }
  }

  return tiers;
}
