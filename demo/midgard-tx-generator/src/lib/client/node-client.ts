import { Effect } from 'effect';
import { metrics } from '../scheduler/metrics.js';

export interface MidgardNodeConfig {
  baseUrl: string;
  retryAttempts?: number;
  retryDelay?: number;
}

// Simple error types for better error handling
export type SubmitTxError =
  | { _tag: 'NetworkError'; error: string }
  | { _tag: 'ValidationError'; error: string }
  | { _tag: 'UnknownError'; error: string };

export class MidgardNodeClient {
  private readonly baseUrl: string;
  private readonly retryAttempts: number;
  private readonly retryDelay: number;

  constructor(config: MidgardNodeConfig) {
    this.baseUrl = config.baseUrl;
    this.retryAttempts = config.retryAttempts ?? 3;
    this.retryDelay = config.retryDelay ?? 1000;
  }

  /**
   * Check node availability by making a dummy request
   */
  async isAvailable(): Promise<boolean> {
    try {
      // Try to fetch a dummy transaction status - if the node is up, it will return 404
      // If the node is down, it will throw a connection error
      const response = await fetch(
        `${this.baseUrl}/tx?tx_hash=${'0'.repeat(64)}`
      );
      return response.status === 404;
    } catch (error) {
      return false;
    }
  }

  /**
   * Submit a transaction to the node with retries
   */
  submitTransaction(cborHex: string) {
    const startTime = Date.now();

    return Effect.tryPromise({
      try: async () => {
        // First check if node is available
        if (!(await this.isAvailable())) {
          throw new Error('Node is not available');
        }

        let attempts = 0;
        while (attempts < this.retryAttempts) {
          try {
            const response = await fetch(
              `${this.baseUrl}/submit?tx_cbor=${encodeURIComponent(cborHex)}`,
              {
                method: 'POST',
                headers: {
                  'Content-Type': 'text/plain',
                },
              }
            );

            if (!response.ok) {
              const error = await response.json();
              throw new Error(
                error.message || `Unexpected status: ${response.status}`
              );
            }

            const result = await response.json();
            metrics.recordSubmission(true, 1, Date.now() - startTime);
            return result;
          } catch (error) {
            attempts++;
            if (attempts === this.retryAttempts) {
              metrics.recordSubmission(false, 1, Date.now() - startTime);
              throw error;
            }
            await new Promise((resolve) =>
              setTimeout(resolve, this.retryDelay)
            );
          }
        }
      },
      catch: (error: unknown): SubmitTxError => {
        metrics.recordSubmission(false, 1, Date.now() - startTime);

        // Handle known error types
        if (error instanceof Error) {
          if (
            error.message === 'Node is not available' ||
            error instanceof TypeError
          ) {
            return { _tag: 'NetworkError', error: error.message };
          }
          return { _tag: 'ValidationError', error: error.message };
        }

        // Handle unknown errors
        return { _tag: 'UnknownError', error: String(error) };
      },
    });
  }

  /**
   * Get transaction status from the node
   */
  getTransactionStatus(txHash: string) {
    return Effect.tryPromise({
      try: async () => {
        const response = await fetch(`${this.baseUrl}/tx?tx_hash=${txHash}`);
        if (!response.ok) {
          throw new Error(`Unexpected status: ${response.status}`);
        }
        const data = await response.json();
        return data;
      },
      catch: (error: unknown): SubmitTxError => {
        if (error instanceof TypeError) {
          return { _tag: 'NetworkError', error: error.message };
        }
        if (error instanceof Error) {
          return { _tag: 'ValidationError', error: error.message };
        }
        return { _tag: 'UnknownError', error: String(error) };
      },
    });
  }
}
