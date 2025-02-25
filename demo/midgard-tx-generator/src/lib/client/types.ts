import { UTxO } from '@lucid-evolution/lucid';

/**
 * Core types for Midgard transaction generation and submission
 */

export interface SerializedMidgardTransaction {
  type: 'Midgard L2 User Transaction';
  description: string;
  cborHex: string;
  txId: string;
}

export interface TransactionGeneratorConfig {
  network: string;
  initialUTxO: UTxO;
  nodeConfig: {
    baseUrl: string;
    retryAttempts?: number;
    retryDelay?: number;
  };
}

export interface GeneratorResult {
  success: boolean;
  transaction?: SerializedMidgardTransaction;
  error?: string;
}

// Types matching midgard-node API responses
export interface NodeTxResponse {
  message: string;
  status?: string;
}

export interface NodeUtxoResponse {
  utxos: UTxO[];
}

export interface NodeBlockResponse {
  hashes: string[];
}
