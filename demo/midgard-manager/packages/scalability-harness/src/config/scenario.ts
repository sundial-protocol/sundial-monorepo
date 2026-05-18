const FILESYSTEM_SAFE_RUNID = /^[A-Za-z0-9_-]+$/;
const URL_PREFIX = /^https?:\/\//;

const RAMP_STRATEGIES = ['multiply', 'percent_increment'] as const;
export const REQUEST_EVENT_MODES = ['off', 'sampled', 'all'] as const;
export type RequestEventsMode = (typeof REQUEST_EVENT_MODES)[number];

export interface StopConditions {
  maxConsecutiveNodeProbeFailures: number;
  stopOnPrometheusDown: boolean;
  stopOnCommitmentFailure: boolean;
  stopOnMergeFailure: boolean;
  maxRecoveryQueueSize?: number;
  maxRecoveryMempoolSize?: number;
  minUsefulThroughputRatio?: number;
}

export interface MultiplyRamp {
  strategy: 'multiply';
  stepMultiplier: number;
}

export interface PercentIncrementRamp {
  strategy: 'percent_increment';
  percentIncrement: number;
}

export type Ramp = MultiplyRamp | PercentIncrementRamp;

export interface TierOverride {
  tierIndex: number;
  durationSeconds?: number;
  recoverySeconds?: number;
}

export interface RunClassificationPolicyConfig {
  maxCollapsedTiers?: number;
  minCompletedTiers?: number;
  maxEvidenceIncompleteTiers?: number;
  minDurableThroughputRatio?: number;
  maxRejectedRatio?: number;
  maxProcessingFailedRatio?: number;
  maxFinalQueueSizeAfterRecovery?: number;
  maxFinalMempoolSizeAfterRecovery?: number;
}

export interface ScalabilityScenario {
  runId: string;
  nodeEndpoint: string;
  prometheusEndpoint: string;
  outputDir: string;
  seed: string;
  replayCorpusPath?: string;
  transactionType: 'one-to-one' | 'multi-output' | 'mixed';
  oneToOneRatio?: number;
  tierDurationSeconds: number;
  recoverySeconds: number;
  startTps: number;
  maxTps: number;
  stepMultiplier?: number;
  ramp?: Ramp;
  tierOverrides?: TierOverride[];
  batchSize: number;
  concurrency: number;
  retryAttempts: number;
  retryDelayMs: number;
  requestEvents?: RequestEventsMode;
  runClassificationPolicy?: RunClassificationPolicyConfig;
  stopConditions: StopConditions;
}

export class ScenarioValidationError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'ScenarioValidationError';
  }
}

function assertPresent(value: unknown, field: string): void {
  if (value === undefined || value === null) {
    throw new ScenarioValidationError(`Missing required field: ${field}`);
  }
}

function assertPositiveNumber(value: unknown, field: string): void {
  assertPresent(value, field);
  if (typeof value !== 'number' || !isFinite(value) || value <= 0) {
    throw new ScenarioValidationError(`${field} must be a positive number, got: ${value}`);
  }
}

function assertNonNegativeInteger(value: unknown, field: string): void {
  assertPresent(value, field);
  if (typeof value !== 'number' || !Number.isInteger(value) || value < 0) {
    throw new ScenarioValidationError(`${field} must be a non-negative integer, got: ${value}`);
  }
}

function assertUrl(value: unknown, field: string): void {
  assertPresent(value, field);
  if (typeof value !== 'string' || !URL_PREFIX.test(value)) {
    throw new ScenarioValidationError(
      `${field} must start with http:// or https://, got: ${value}`
    );
  }
}

function assertPositiveFiniteNumber(value: unknown, field: string): void {
  if (typeof value !== 'number' || !isFinite(value) || value <= 0) {
    throw new ScenarioValidationError(`${field} must be a positive number, got: ${value}`);
  }
}

export function validateScenario(raw: unknown): ScalabilityScenario {
  if (typeof raw !== 'object' || raw === null) {
    throw new ScenarioValidationError('Scenario must be a JSON object');
  }

  const s = raw as Record<string, unknown>;

  assertPresent(s.runId, 'runId');
  if (typeof s.runId !== 'string' || s.runId.length === 0) {
    throw new ScenarioValidationError('runId must be a non-empty string');
  }
  if (!FILESYSTEM_SAFE_RUNID.test(s.runId)) {
    throw new ScenarioValidationError(
      `runId must contain only alphanumeric characters, dashes, and underscores, got: ${s.runId}`
    );
  }

  assertUrl(s.nodeEndpoint, 'nodeEndpoint');
  assertUrl(s.prometheusEndpoint, 'prometheusEndpoint');

  assertPresent(s.outputDir, 'outputDir');
  if (typeof s.outputDir !== 'string' || s.outputDir.length === 0) {
    throw new ScenarioValidationError('outputDir must be a non-empty string');
  }

  assertPresent(s.seed, 'seed');
  if (typeof s.seed !== 'string') {
    throw new ScenarioValidationError('seed must be a string');
  }

  if (s.replayCorpusPath !== undefined) {
    if (typeof s.replayCorpusPath !== 'string' || s.replayCorpusPath.trim().length === 0) {
      throw new ScenarioValidationError(
        'replayCorpusPath must be a non-empty string when provided'
      );
    }
  }

  assertPresent(s.transactionType, 'transactionType');
  if (
    s.transactionType !== 'one-to-one' &&
    s.transactionType !== 'multi-output' &&
    s.transactionType !== 'mixed'
  ) {
    throw new ScenarioValidationError(
      `transactionType must be one-to-one, multi-output, or mixed, got: ${s.transactionType}`
    );
  }

  if (s.oneToOneRatio !== undefined) {
    if (
      typeof s.oneToOneRatio !== 'number' ||
      !isFinite(s.oneToOneRatio) ||
      s.oneToOneRatio < 0 ||
      s.oneToOneRatio > 100
    ) {
      throw new ScenarioValidationError(
        `oneToOneRatio must be between 0 and 100 when provided, got: ${s.oneToOneRatio}`
      );
    }
  }

  assertPositiveNumber(s.tierDurationSeconds, 'tierDurationSeconds');
  assertPositiveNumber(s.recoverySeconds, 'recoverySeconds');
  assertPositiveNumber(s.startTps, 'startTps');
  assertPositiveNumber(s.maxTps, 'maxTps');
  assertPositiveNumber(s.batchSize, 'batchSize');
  assertPositiveNumber(s.concurrency, 'concurrency');

  if (s.ramp !== undefined) {
    if (typeof s.ramp !== 'object' || s.ramp === null) {
      throw new ScenarioValidationError('ramp must be an object when provided');
    }

    const ramp = s.ramp as Record<string, unknown>;
    assertPresent(ramp.strategy, 'ramp.strategy');
    if (
      typeof ramp.strategy !== 'string' ||
      !(RAMP_STRATEGIES as readonly string[]).includes(ramp.strategy)
    ) {
      throw new ScenarioValidationError(
        `ramp.strategy must be one of: ${RAMP_STRATEGIES.join(', ')}, got: ${ramp.strategy}`
      );
    }

    if (ramp.strategy === 'multiply') {
      assertPresent(ramp.stepMultiplier, 'ramp.stepMultiplier');
      if (
        typeof ramp.stepMultiplier !== 'number' ||
        !isFinite(ramp.stepMultiplier) ||
        ramp.stepMultiplier <= 1
      ) {
        throw new ScenarioValidationError(
          `ramp.stepMultiplier must be greater than 1, got: ${ramp.stepMultiplier}`
        );
      }
    }

    if (ramp.strategy === 'percent_increment') {
      assertPresent(ramp.percentIncrement, 'ramp.percentIncrement');
      assertPositiveFiniteNumber(ramp.percentIncrement, 'ramp.percentIncrement');
    }
  } else {
    assertPresent(s.stepMultiplier, 'stepMultiplier');
    if (
      typeof s.stepMultiplier !== 'number' ||
      !isFinite(s.stepMultiplier) ||
      s.stepMultiplier <= 1
    ) {
      throw new ScenarioValidationError(
        `stepMultiplier must be greater than 1, got: ${s.stepMultiplier}`
      );
    }
  }

  if (s.tierOverrides !== undefined) {
    if (!Array.isArray(s.tierOverrides)) {
      throw new ScenarioValidationError('tierOverrides must be an array when provided');
    }

    const seenTierIndices = new Set<number>();
    for (const [index, override] of s.tierOverrides.entries()) {
      if (typeof override !== 'object' || override === null) {
        throw new ScenarioValidationError(`tierOverrides[${index}] must be an object`);
      }
      const tierOverride = override as Record<string, unknown>;
      assertNonNegativeInteger(tierOverride.tierIndex, `tierOverrides[${index}].tierIndex`);
      const tierIndex = tierOverride.tierIndex as number;
      if (seenTierIndices.has(tierIndex)) {
        throw new ScenarioValidationError(
          `tierOverrides contains duplicate tierIndex: ${tierIndex}`
        );
      }
      seenTierIndices.add(tierIndex);

      if (
        tierOverride.durationSeconds === undefined &&
        tierOverride.recoverySeconds === undefined
      ) {
        throw new ScenarioValidationError(
          `tierOverrides[${index}] must include durationSeconds or recoverySeconds`
        );
      }

      if (tierOverride.durationSeconds !== undefined) {
        assertPositiveFiniteNumber(
          tierOverride.durationSeconds,
          `tierOverrides[${index}].durationSeconds`
        );
      }

      if (tierOverride.recoverySeconds !== undefined) {
        assertPositiveFiniteNumber(
          tierOverride.recoverySeconds,
          `tierOverrides[${index}].recoverySeconds`
        );
      }
    }
  }

  if (typeof s.maxTps === 'number' && typeof s.startTps === 'number' && s.maxTps < s.startTps) {
    throw new ScenarioValidationError(
      `maxTps (${s.maxTps}) must be greater than or equal to startTps (${s.startTps})`
    );
  }

  assertNonNegativeInteger(s.retryAttempts, 'retryAttempts');
  assertNonNegativeInteger(s.retryDelayMs, 'retryDelayMs');
  if (s.requestEvents !== undefined) {
    if (
      typeof s.requestEvents !== 'string' ||
      !(REQUEST_EVENT_MODES as readonly string[]).includes(s.requestEvents)
    ) {
      throw new ScenarioValidationError(
        `requestEvents must be one of: ${REQUEST_EVENT_MODES.join(', ')}, got: ${s.requestEvents}`
      );
    }
  }

  if (s.runClassificationPolicy !== undefined) {
    if (typeof s.runClassificationPolicy !== 'object' || s.runClassificationPolicy === null) {
      throw new ScenarioValidationError('runClassificationPolicy must be an object when provided');
    }

    const policy = s.runClassificationPolicy as Record<string, unknown>;
    if (policy.maxCollapsedTiers !== undefined) {
      assertNonNegativeInteger(
        policy.maxCollapsedTiers,
        'runClassificationPolicy.maxCollapsedTiers'
      );
    }
    if (policy.minCompletedTiers !== undefined) {
      assertNonNegativeInteger(
        policy.minCompletedTiers,
        'runClassificationPolicy.minCompletedTiers'
      );
    }
    if (policy.maxEvidenceIncompleteTiers !== undefined) {
      assertNonNegativeInteger(
        policy.maxEvidenceIncompleteTiers,
        'runClassificationPolicy.maxEvidenceIncompleteTiers'
      );
    }
    if (policy.minDurableThroughputRatio !== undefined) {
      if (
        typeof policy.minDurableThroughputRatio !== 'number' ||
        !isFinite(policy.minDurableThroughputRatio) ||
        policy.minDurableThroughputRatio < 0 ||
        policy.minDurableThroughputRatio > 1
      ) {
        throw new ScenarioValidationError(
          `runClassificationPolicy.minDurableThroughputRatio must be between 0 and 1, got: ${policy.minDurableThroughputRatio}`
        );
      }
    }
    if (policy.maxRejectedRatio !== undefined) {
      if (
        typeof policy.maxRejectedRatio !== 'number' ||
        !isFinite(policy.maxRejectedRatio) ||
        policy.maxRejectedRatio < 0 ||
        policy.maxRejectedRatio > 1
      ) {
        throw new ScenarioValidationError(
          `runClassificationPolicy.maxRejectedRatio must be between 0 and 1, got: ${policy.maxRejectedRatio}`
        );
      }
    }
    if (policy.maxProcessingFailedRatio !== undefined) {
      if (
        typeof policy.maxProcessingFailedRatio !== 'number' ||
        !isFinite(policy.maxProcessingFailedRatio) ||
        policy.maxProcessingFailedRatio < 0 ||
        policy.maxProcessingFailedRatio > 1
      ) {
        throw new ScenarioValidationError(
          `runClassificationPolicy.maxProcessingFailedRatio must be between 0 and 1, got: ${policy.maxProcessingFailedRatio}`
        );
      }
    }
    if (policy.maxFinalQueueSizeAfterRecovery !== undefined) {
      assertPositiveNumber(
        policy.maxFinalQueueSizeAfterRecovery,
        'runClassificationPolicy.maxFinalQueueSizeAfterRecovery'
      );
    }
    if (policy.maxFinalMempoolSizeAfterRecovery !== undefined) {
      assertPositiveNumber(
        policy.maxFinalMempoolSizeAfterRecovery,
        'runClassificationPolicy.maxFinalMempoolSizeAfterRecovery'
      );
    }
  }

  assertPresent(s.stopConditions, 'stopConditions');
  if (typeof s.stopConditions !== 'object' || s.stopConditions === null) {
    throw new ScenarioValidationError('stopConditions must be an object');
  }

  const sc = s.stopConditions as Record<string, unknown>;

  assertPositiveNumber(
    sc.maxConsecutiveNodeProbeFailures,
    'stopConditions.maxConsecutiveNodeProbeFailures'
  );

  assertPresent(sc.stopOnPrometheusDown, 'stopConditions.stopOnPrometheusDown');
  if (typeof sc.stopOnPrometheusDown !== 'boolean') {
    throw new ScenarioValidationError('stopConditions.stopOnPrometheusDown must be a boolean');
  }

  assertPresent(sc.stopOnCommitmentFailure, 'stopConditions.stopOnCommitmentFailure');
  if (typeof sc.stopOnCommitmentFailure !== 'boolean') {
    throw new ScenarioValidationError('stopConditions.stopOnCommitmentFailure must be a boolean');
  }

  assertPresent(sc.stopOnMergeFailure, 'stopConditions.stopOnMergeFailure');
  if (typeof sc.stopOnMergeFailure !== 'boolean') {
    throw new ScenarioValidationError('stopConditions.stopOnMergeFailure must be a boolean');
  }

  if (sc.maxRecoveryQueueSize !== undefined) {
    assertPositiveNumber(sc.maxRecoveryQueueSize, 'stopConditions.maxRecoveryQueueSize');
  }

  if (sc.maxRecoveryMempoolSize !== undefined) {
    assertPositiveNumber(sc.maxRecoveryMempoolSize, 'stopConditions.maxRecoveryMempoolSize');
  }

  if (sc.minUsefulThroughputRatio !== undefined) {
    if (
      typeof sc.minUsefulThroughputRatio !== 'number' ||
      !isFinite(sc.minUsefulThroughputRatio) ||
      sc.minUsefulThroughputRatio < 0 ||
      sc.minUsefulThroughputRatio > 1
    ) {
      throw new ScenarioValidationError(
        `stopConditions.minUsefulThroughputRatio must be between 0 and 1 when provided, got: ${sc.minUsefulThroughputRatio}`
      );
    }
  }

  return s as unknown as ScalabilityScenario;
}
