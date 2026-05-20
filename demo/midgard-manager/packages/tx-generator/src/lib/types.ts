import { Network, UTxO } from '@lucid-evolution/lucid';

// Transaction Types
export type TransactionType = 'one-to-one' | 'multi-output' | 'mixed';
export type RequestEventsMode = 'off' | 'sampled' | 'all';

// Node Client Configuration
export interface MidgardNodeConfig {
  baseUrl: string;
  retryAttempts?: number;
  retryDelay?: number;
  // Per-request timeout for /submit calls. Defaults to AVAILABILITY_TIMEOUT (5000 ms).
  // Set to a small value (e.g. 500–2000 ms) for load-test scenarios so workers
  // fail fast and recycle quickly instead of blocking for the full retry chain.
  submitTimeoutMs?: number;
  enableLogs?: boolean;
  skipAvailabilityCheck?: boolean;
}

// Transaction Generator Configuration
export interface TransactionGeneratorConfig {
  // Node settings
  nodeEndpoint: string;
  nodeRetryAttempts?: number;
  nodeRetryDelay?: number;
  nodeSubmitTimeoutMs?: number;
  nodeEnableLogs?: boolean;

  // Network settings
  network: Network;
  initialUTxO: UTxO;

  // Wallet settings
  walletSeedOrPrivateKey: string;

  // Transaction settings
  transactionType: TransactionType;
  oneToOneRatio?: number;
  generationSeed?: string;
  replayCorpusPath?: string;
  // 0-based replay starting index into the corpus (transaction index for JSON,
  // non-empty line index for JSONL). Used by scalability harness tier slicing.
  replayStartIndex?: number;
  // Maximum number of replay transactions to consume from replayStartIndex.
  replayCount?: number;

  // Batch settings
  batchSize: number;
  interval: number;
  concurrency: number;
  targetTps?: number;
  maxInFlight?: number;
  generationConcurrency?: number;
  preparedQueueCapacity?: number;
  autoStopAfterBatch?: boolean;

  // Output settings
  outputDir?: string;
  requestEvents?: RequestEventsMode;
}

// Serialized Transaction Format
export interface SerializedMidgardTransaction {
  cborHex: string;
  description: string;
  txId: string;
  type: string;
}

// Default configuration values
export const DEFAULT_CONFIG: TransactionGeneratorConfig = {
  // Node defaults
  nodeEndpoint: 'http://localhost:3000',
  nodeRetryAttempts: 3,
  nodeRetryDelay: 1000,
  nodeEnableLogs: true,

  // Network defaults
  network: 'Preview' as Network,
  initialUTxO: {
    txHash: '0'.repeat(64),
    outputIndex: 0,
    assets: { lovelace: 10_000_000_000n },
    address: '', // Will be derived from wallet
    datum: null,
    datumHash: null,
    scriptRef: null,
  },

  // Wallet defaults - must be provided
  walletSeedOrPrivateKey: '',

  // Transaction defaults
  transactionType: 'mixed',
  oneToOneRatio: 70,
  generationSeed: undefined,
  replayCorpusPath: undefined,
  replayStartIndex: undefined,
  replayCount: undefined,

  // Batch defaults
  batchSize: 10,
  interval: 5,
  concurrency: 5,
  targetTps: undefined,
  maxInFlight: undefined,
  generationConcurrency: undefined,
  preparedQueueCapacity: undefined,
  autoStopAfterBatch: false,

  // Output defaults
  outputDir: 'generated-transactions',
  requestEvents: 'off',
};

// Constants
export const TRANSACTION_CONSTANTS = {
  MIN_LOVELACE_OUTPUT: 1_000_000n,
  OUTPUTS_PER_DISTRIBUTION: 20,
  GC_PAUSE_INTERVAL: {
    ONE_TO_ONE: 1000,
    MULTI_OUTPUT: 250,
  },
  NODE_DEFAULTS: {
    RETRY_ATTEMPTS: 3,
    RETRY_DELAY: 1000,
    AVAILABILITY_TIMEOUT: 5000,
    // Per-request submit timeout (ms). Distinct from the availability check timeout.
    // Set short (e.g. 500-2000 ms) for load-test scenarios to free workers quickly.
    SUBMIT_TIMEOUT_MS: 5000,
  },
} as const;

/**
 * Validates the transaction generator configuration
 * @throws Error if configuration is invalid
 */
export const validateGeneratorConfig = (config: TransactionGeneratorConfig): void => {
  // Node validation
  if (!config.nodeEndpoint) {
    throw new Error('Node endpoint is required');
  }
  if (!config.nodeEndpoint.startsWith('http')) {
    throw new Error('Node endpoint must start with http:// or https://');
  }
  if (config.nodeRetryAttempts !== undefined && config.nodeRetryAttempts < 0) {
    throw new Error('Node retry attempts must be non-negative');
  }
  if (config.nodeRetryDelay !== undefined && config.nodeRetryDelay < 0) {
    throw new Error('Node retry delay must be non-negative');
  }

  // Wallet validation
  if (!config.walletSeedOrPrivateKey) {
    throw new Error('Wallet private key or seed phrase is required');
  }

  // Transaction validation
  if (config.oneToOneRatio !== undefined) {
    if (config.oneToOneRatio < 0 || config.oneToOneRatio > 100) {
      throw new Error('One-to-one ratio must be between 0 and 100');
    }
  }
  if (config.generationSeed !== undefined && config.generationSeed.trim().length === 0) {
    throw new Error('Generation seed must not be empty when provided');
  }
  if (config.replayCorpusPath !== undefined && config.replayCorpusPath.trim().length === 0) {
    throw new Error('Replay corpus path must not be empty when provided');
  }
  if (
    config.replayStartIndex !== undefined &&
    (!Number.isFinite(config.replayStartIndex) || config.replayStartIndex < 0)
  ) {
    throw new Error('Replay start index must be a non-negative number when provided');
  }
  if (
    config.replayCount !== undefined &&
    (!Number.isFinite(config.replayCount) || config.replayCount < 0)
  ) {
    throw new Error('Replay count must be a non-negative number when provided');
  }

  // Batch validation
  if (config.batchSize < 1) {
    throw new Error('Batch size must be at least 1');
  }
  if (config.interval < 0) {
    throw new Error('Interval must be non-negative');
  }
  if (config.concurrency < 1) {
    throw new Error('Concurrency must be at least 1');
  }
  if (config.targetTps !== undefined && config.targetTps <= 0) {
    throw new Error('targetTps must be positive when provided');
  }
  if (config.maxInFlight !== undefined && config.maxInFlight < 1) {
    throw new Error('maxInFlight must be at least 1 when provided');
  }
  if (config.generationConcurrency !== undefined && config.generationConcurrency < 1) {
    throw new Error('generationConcurrency must be at least 1 when provided');
  }
  if (config.preparedQueueCapacity !== undefined && config.preparedQueueCapacity < 1) {
    throw new Error('preparedQueueCapacity must be at least 1 when provided');
  }

  if (
    config.requestEvents !== undefined &&
    config.requestEvents !== 'off' &&
    config.requestEvents !== 'sampled' &&
    config.requestEvents !== 'all'
  ) {
    throw new Error('requestEvents must be one of: off, sampled, all');
  }
};
