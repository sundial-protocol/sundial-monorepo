/**
 * Configuration for the transaction generator
 */
export interface TransactionGeneratorConfig {
  // Node settings
  nodeEndpoint: string;
  nodeRetryAttempts?: number;
  nodeRetryDelay?: number;
  nodeEnableLogs?: boolean;

  // Wallet settings
  walletPrivateKey: string;

  // Transaction settings
  transactionType: 'one-to-one' | 'multi-output' | 'mixed';
  oneToOneRatio?: number; // Percentage of one-to-one transactions (0-100) when using 'mixed' type

  // Batch settings
  batchSize: number;
  interval: number; // in seconds
  concurrency: number;

  // Scheduling settings
  autoStopAfterBatch?: boolean; // Automatically stop after generating one batch (for scheduled jobs)
}

/**
 * Default configuration values
 */
export const DEFAULT_CONFIG: TransactionGeneratorConfig = {
  // Node defaults
  nodeEndpoint: 'http://localhost:3000',
  nodeRetryAttempts: 3,
  nodeRetryDelay: 1000,
  nodeEnableLogs: true,

  // Wallet defaults
  walletPrivateKey: '', // Will be filled from wallet config

  // Transaction defaults
  transactionType: 'mixed',
  oneToOneRatio: 70, // 70% one-to-one, 30% multi-output

  // Batch defaults
  batchSize: 10,
  interval: 5,
  concurrency: 5,
  autoStopAfterBatch: false,
};

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
  if (!config.walletPrivateKey) {
    throw new Error('Wallet private key is required');
  }

  // Transaction validation
  if (config.oneToOneRatio !== undefined) {
    if (config.oneToOneRatio < 0 || config.oneToOneRatio > 100) {
      throw new Error('One-to-one ratio must be between 0 and 100');
    }
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
  if (config.concurrency > 20) {
    throw new Error('Concurrency must not exceed 20');
  }
};
