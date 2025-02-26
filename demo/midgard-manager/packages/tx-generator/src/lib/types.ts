/**
 * Configuration for the transaction generator
 */
export interface TransactionGeneratorConfig {
  // Core settings
  nodeEndpoint: string;
  walletPrivateKey: string;

  // Transaction settings
  transactionType: "one-to-one" | "multi-output" | "mixed";
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
  nodeEndpoint: "http://localhost:3000",
  walletPrivateKey: "", // Will be filled from wallet config
  transactionType: "mixed",
  oneToOneRatio: 70, // 70% one-to-one, 30% multi-output
  batchSize: 10,
  interval: 5,
  concurrency: 5,
  autoStopAfterBatch: false,
};
