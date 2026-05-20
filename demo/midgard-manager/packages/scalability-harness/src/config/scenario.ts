const FILESYSTEM_SAFE_RUNID = /^[A-Za-z0-9_-]+$/;
const URL_PREFIX = /^https?:\/\//;

const RAMP_STRATEGIES = ['multiply', 'percent_increment'] as const;
export const REQUEST_EVENT_MODES = ['off', 'sampled', 'all'] as const;
export type RequestEventsMode = (typeof REQUEST_EVENT_MODES)[number];

export const L1_PROVIDER_MODES = ['kupmios', 'blockfrost', 'emulator', 'unknown'] as const;
export type L1ProviderMode = (typeof L1_PROVIDER_MODES)[number];

export const WALLET_MODES = ['test-wallet', 'external-key'] as const;
export type WalletMode = (typeof WALLET_MODES)[number];
export const GRAFANA_SCREENSHOT_THEMES = ['light', 'dark'] as const;
export type GrafanaScreenshotTheme = (typeof GRAFANA_SCREENSHOT_THEMES)[number];

export interface GrafanaScreenshotsConfig {
  enabled: boolean;
  grafanaBaseUrl: string;
  dashboardJsonPath: string;
  dashboardUid?: string;
  timezone?: string;
  theme?: GrafanaScreenshotTheme;
  lookbackMinutes?: number;
  viewportWidth?: number;
  viewportHeight?: number;
  waitForPanelsMs?: number;
  peakCaptureCooldownSeconds?: number;
  capturePeakEvents?: boolean;
  captureFinalPanelSet?: boolean;
}

export interface StopConditions {
  maxConsecutiveNodeProbeFailures: number;
  stopOnPrometheusDown: boolean;
  stopOnCommitmentFailure: boolean;
  stopOnMergeFailure: boolean;
  // Commitment-failure budget ratio: commitmentFailuresDelta / mempoolAcceptedDelta.
  // When provided with stopOnCommitmentFailure=true, collapse only when the observed
  // ratio exceeds this threshold. Use 0.0001 for a 0.01% budget.
  maxCommitmentFailureRatio?: number;
  maxRecoveryQueueSize?: number;
  maxRecoveryMempoolSize?: number;
  // Node-health drain check: requires committedTxDelta >= mempoolAcceptedDelta * ratio.
  // Fires when the node commits fewer transactions than it accepted during the load window,
  // indicating the node cannot sustain the incoming rate. Use 1.0 for strict drain parity.
  minCommitToAcceptedRatio?: number;
  // Load-driver adequacy check: requires mempoolAcceptedTps / targetTps >= ratio.
  // Fires when the load driver itself failed to deliver the target rate regardless of node health.
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
  maxP95InclusionLatencyMs?: number;
  maxL1FeePerCommittedTxLovelace?: number;
}

export interface ScalabilityScenario {
  runId: string;
  description?: string;
  nodeEndpoint: string;
  prometheusEndpoint: string;
  outputDir: string;
  seed: string;
  replayCorpusPath?: string;
  // Number of transactions to pre-generate before the load tiers begin.
  // When set, the harness generates a corpus in the run directory and passes
  // it to the tx-generator as the replay corpus for all tiers. The replay
  // corpus cycles, so any count > 0 works; use a count large enough to avoid
  // duplicate txId submissions during sustained high-TPS runs.
  // Cannot be combined with a static replayCorpusPath.
  pregenTransactionCount?: number;
  // L1 provider mode used for this run. Recorded in the run manifest and report
  // so before/after comparisons carry the provider context. Use "emulator" for
  // local/test runs that do not connect to a live L1 provider.
  l1ProviderMode?: L1ProviderMode;
  // Wallet mode for the load generator. "test-wallet" generates a synthetic key
  // pair and requires the node to be initialised with L2 genesis UTxOs for that
  // wallet. "external-key" passes a pre-funded key via the WALLET_PRIVATE_KEY
  // environment variable and is required for sustained high-TPS formal runs
  // against a live L1.
  walletMode?: WalletMode;
  // Free-text note about wallet/UTxO provisioning for this run. Stored in the
  // run manifest for traceability; does not affect harness behaviour.
  walletProvisioningNote?: string;
  // Loki endpoint for log evidence capture. When set, the harness queries Loki
  // after each tier's recovery window and writes loki-captures.json. Default
  // query is {job="containerlogs"} unless lokiNodeQuery overrides it.
  lokiEndpoint?: string;
  // Tempo endpoint for trace evidence capture. When set, the harness queries
  // Tempo after each tier's recovery window and writes tempo-captures.json.
  // Default service name is "midgard-node" unless tempoServiceName overrides it.
  tempoEndpoint?: string;
  // LogQL query passed to Loki query_range. Defaults to {job="containerlogs"}.
  lokiNodeQuery?: string;
  // OpenTelemetry service name used for Tempo trace search. Defaults to "midgard-node".
  tempoServiceName?: string;
  transactionType: 'one-to-one' | 'multi-output' | 'mixed';
  oneToOneRatio?: number;
  tierDurationSeconds: number;
  recoverySeconds: number;
  startTps: number;
  maxTps: number;
  stepMultiplier?: number;
  ramp?: Ramp;
  tierOverrides?: TierOverride[];
  // Estimated wall-clock cost of generating and submitting one transaction (seconds).
  // The harness derives batchSize, concurrency, and interval from this value and
  // targetTps so scenarios only need to set this once per environment.
  txGeneratorTaskCostSeconds: number;
  retryAttempts: number;
  retryDelayMs: number;
  // Per-attempt HTTP submit timeout in milliseconds. When set, the harness uses this
  // to size maxInFlight so workers recycle quickly instead of holding connections open
  // for the full retry chain. Set low (e.g. 500 ms) for high-throughput load testing.
  // Pairs with retryAttempts=1 for "fire once and move on" behaviour.
  // Defaults to the tx-generator default (5000 ms) when omitted.
  submitTimeoutMs?: number;
  requestEvents?: RequestEventsMode;
  grafanaScreenshots?: GrafanaScreenshotsConfig;
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

  if (s.description !== undefined) {
    if (typeof s.description !== 'string' || s.description.trim().length === 0) {
      throw new ScenarioValidationError('description must be a non-empty string when provided');
    }
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

  if (s.pregenTransactionCount !== undefined) {
    if (
      typeof s.pregenTransactionCount !== 'number' ||
      !Number.isInteger(s.pregenTransactionCount) ||
      s.pregenTransactionCount < 1
    ) {
      throw new ScenarioValidationError(
        `pregenTransactionCount must be a positive integer when provided, got: ${s.pregenTransactionCount}`
      );
    }
    if (s.replayCorpusPath !== undefined) {
      throw new ScenarioValidationError(
        'pregenTransactionCount and replayCorpusPath are mutually exclusive'
      );
    }
  }

  if (s.l1ProviderMode !== undefined) {
    if (
      typeof s.l1ProviderMode !== 'string' ||
      !(L1_PROVIDER_MODES as readonly string[]).includes(s.l1ProviderMode)
    ) {
      throw new ScenarioValidationError(
        `l1ProviderMode must be one of: ${L1_PROVIDER_MODES.join(', ')}, got: ${s.l1ProviderMode}`
      );
    }
  }

  if (s.walletMode !== undefined) {
    if (
      typeof s.walletMode !== 'string' ||
      !(WALLET_MODES as readonly string[]).includes(s.walletMode)
    ) {
      throw new ScenarioValidationError(
        `walletMode must be one of: ${WALLET_MODES.join(', ')}, got: ${s.walletMode}`
      );
    }
  }

  if (s.walletProvisioningNote !== undefined) {
    if (typeof s.walletProvisioningNote !== 'string') {
      throw new ScenarioValidationError('walletProvisioningNote must be a string when provided');
    }
  }

  if (s.lokiEndpoint !== undefined) {
    assertUrl(s.lokiEndpoint, 'lokiEndpoint');
  }

  if (s.tempoEndpoint !== undefined) {
    assertUrl(s.tempoEndpoint, 'tempoEndpoint');
  }

  if (s.lokiNodeQuery !== undefined) {
    if (typeof s.lokiNodeQuery !== 'string' || s.lokiNodeQuery.trim().length === 0) {
      throw new ScenarioValidationError('lokiNodeQuery must be a non-empty string when provided');
    }
  }

  if (s.tempoServiceName !== undefined) {
    if (typeof s.tempoServiceName !== 'string' || s.tempoServiceName.trim().length === 0) {
      throw new ScenarioValidationError(
        'tempoServiceName must be a non-empty string when provided'
      );
    }
  }

  if (s.grafanaScreenshots !== undefined) {
    if (typeof s.grafanaScreenshots !== 'object' || s.grafanaScreenshots === null) {
      throw new ScenarioValidationError('grafanaScreenshots must be an object when provided');
    }

    const gs = s.grafanaScreenshots as Record<string, unknown>;
    assertPresent(gs.enabled, 'grafanaScreenshots.enabled');
    if (typeof gs.enabled !== 'boolean') {
      throw new ScenarioValidationError('grafanaScreenshots.enabled must be a boolean');
    }

    if (gs.enabled) {
      assertUrl(gs.grafanaBaseUrl, 'grafanaScreenshots.grafanaBaseUrl');

      assertPresent(gs.dashboardJsonPath, 'grafanaScreenshots.dashboardJsonPath');
      if (typeof gs.dashboardJsonPath !== 'string' || gs.dashboardJsonPath.trim().length === 0) {
        throw new ScenarioValidationError(
          'grafanaScreenshots.dashboardJsonPath must be a non-empty string'
        );
      }

      if (gs.dashboardUid !== undefined) {
        if (typeof gs.dashboardUid !== 'string' || gs.dashboardUid.trim().length === 0) {
          throw new ScenarioValidationError(
            'grafanaScreenshots.dashboardUid must be a non-empty string when provided'
          );
        }
      }
    }

    if (gs.timezone !== undefined) {
      if (typeof gs.timezone !== 'string' || gs.timezone.trim().length === 0) {
        throw new ScenarioValidationError(
          'grafanaScreenshots.timezone must be a non-empty string when provided'
        );
      }
    }

    if (gs.theme !== undefined) {
      if (
        typeof gs.theme !== 'string' ||
        !(GRAFANA_SCREENSHOT_THEMES as readonly string[]).includes(gs.theme)
      ) {
        throw new ScenarioValidationError(
          `grafanaScreenshots.theme must be one of: ${GRAFANA_SCREENSHOT_THEMES.join(', ')}, got: ${gs.theme}`
        );
      }
    }

    if (gs.lookbackMinutes !== undefined) {
      assertPositiveFiniteNumber(gs.lookbackMinutes, 'grafanaScreenshots.lookbackMinutes');
    }

    if (gs.viewportWidth !== undefined) {
      assertPositiveFiniteNumber(gs.viewportWidth, 'grafanaScreenshots.viewportWidth');
    }

    if (gs.viewportHeight !== undefined) {
      assertPositiveFiniteNumber(gs.viewportHeight, 'grafanaScreenshots.viewportHeight');
    }

    if (gs.waitForPanelsMs !== undefined) {
      assertPositiveFiniteNumber(gs.waitForPanelsMs, 'grafanaScreenshots.waitForPanelsMs');
    }

    if (gs.peakCaptureCooldownSeconds !== undefined) {
      assertPositiveFiniteNumber(
        gs.peakCaptureCooldownSeconds,
        'grafanaScreenshots.peakCaptureCooldownSeconds'
      );
    }

    if (gs.capturePeakEvents !== undefined && typeof gs.capturePeakEvents !== 'boolean') {
      throw new ScenarioValidationError(
        'grafanaScreenshots.capturePeakEvents must be a boolean when provided'
      );
    }

    if (gs.captureFinalPanelSet !== undefined && typeof gs.captureFinalPanelSet !== 'boolean') {
      throw new ScenarioValidationError(
        'grafanaScreenshots.captureFinalPanelSet must be a boolean when provided'
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
  assertPositiveNumber(s.txGeneratorTaskCostSeconds, 'txGeneratorTaskCostSeconds');

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
  if (s.submitTimeoutMs !== undefined) {
    assertPositiveNumber(s.submitTimeoutMs, 'submitTimeoutMs');
  }
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
    if (policy.maxP95InclusionLatencyMs !== undefined) {
      assertPositiveNumber(
        policy.maxP95InclusionLatencyMs,
        'runClassificationPolicy.maxP95InclusionLatencyMs'
      );
    }
    if (policy.maxL1FeePerCommittedTxLovelace !== undefined) {
      assertPositiveNumber(
        policy.maxL1FeePerCommittedTxLovelace,
        'runClassificationPolicy.maxL1FeePerCommittedTxLovelace'
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
    if (
      typeof sc.maxRecoveryQueueSize !== 'number' ||
      !isFinite(sc.maxRecoveryQueueSize) ||
      sc.maxRecoveryQueueSize < 0
    ) {
      throw new ScenarioValidationError(
        `stopConditions.maxRecoveryQueueSize must be a non-negative number when provided, got: ${sc.maxRecoveryQueueSize}`
      );
    }
  }

  if (sc.maxRecoveryMempoolSize !== undefined) {
    if (
      typeof sc.maxRecoveryMempoolSize !== 'number' ||
      !isFinite(sc.maxRecoveryMempoolSize) ||
      sc.maxRecoveryMempoolSize < 0
    ) {
      throw new ScenarioValidationError(
        `stopConditions.maxRecoveryMempoolSize must be a non-negative number when provided, got: ${sc.maxRecoveryMempoolSize}`
      );
    }
  }

  if (sc.maxCommitmentFailureRatio !== undefined) {
    if (
      typeof sc.maxCommitmentFailureRatio !== 'number' ||
      !isFinite(sc.maxCommitmentFailureRatio) ||
      sc.maxCommitmentFailureRatio < 0 ||
      sc.maxCommitmentFailureRatio > 1
    ) {
      throw new ScenarioValidationError(
        `stopConditions.maxCommitmentFailureRatio must be between 0 and 1 when provided, got: ${sc.maxCommitmentFailureRatio}`
      );
    }
  }

  if (sc.minCommitToAcceptedRatio !== undefined) {
    if (
      typeof sc.minCommitToAcceptedRatio !== 'number' ||
      !isFinite(sc.minCommitToAcceptedRatio) ||
      sc.minCommitToAcceptedRatio < 0 ||
      sc.minCommitToAcceptedRatio > 1
    ) {
      throw new ScenarioValidationError(
        `stopConditions.minCommitToAcceptedRatio must be between 0 and 1 when provided, got: ${sc.minCommitToAcceptedRatio}`
      );
    }
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
