import { Effect } from 'effect';

import { logFailedTransaction, logSubmittedTransaction } from '../../utils/logging.js';
import { MidgardNodeConfig, TRANSACTION_CONSTANTS } from '../types.js';

// Simple error types for better error handling
export type SubmitTxError =
  | { _tag: 'NetworkError'; error: string }
  | { _tag: 'ValidationError'; error: string }
  | { _tag: 'UnknownError'; error: string };

export type SubmitTransactionStatus = 'SUBMITTED' | 'NODE_UNAVAILABLE' | 'ERROR';
export type SubmitResponseClass =
  | 'submitted'
  | 'node_unavailable'
  | 'http_error'
  | 'timed_out'
  | 'network_error'
  | 'unknown_error';
export type SubmitErrorClass = 'http_error' | 'timed_out' | 'network_error' | 'unknown_error';

export interface SubmitTransactionResult {
  status: SubmitTransactionStatus;
  txId?: string;
  error?: string;
  errorClass?: SubmitErrorClass;
  message?: string;
  httpStatusCode?: number;
  responseClass: SubmitResponseClass;
  latencyMs: number;
  attempts: number;
  retriesUsed: number;
}

interface SubmitTransactionOptions {
  skipAvailabilityCheck?: boolean;
}

class SubmitHttpError extends Error {
  readonly httpStatusCode: number;

  constructor(httpStatusCode: number, message: string) {
    super(message);
    this.name = 'SubmitHttpError';
    this.httpStatusCode = httpStatusCode;
  }
}

function classifySubmitFailure(err: unknown): {
  responseClass: SubmitResponseClass;
  errorClass: SubmitErrorClass;
  errorMessage: string;
  httpStatusCode?: number;
} {
  if (err instanceof SubmitHttpError) {
    return {
      responseClass: 'http_error',
      errorClass: 'http_error',
      errorMessage: err.message,
      httpStatusCode: err.httpStatusCode,
    };
  }

  if (err instanceof DOMException && err.name === 'AbortError') {
    return {
      responseClass: 'timed_out',
      errorClass: 'timed_out',
      errorMessage: 'request timed out',
    };
  }

  if (err instanceof TypeError) {
    return {
      responseClass: 'network_error',
      errorClass: 'network_error',
      errorMessage: err.message,
    };
  }

  if (err instanceof Error) {
    return {
      responseClass: 'unknown_error',
      errorClass: 'unknown_error',
      errorMessage: err.message,
    };
  }

  return {
    responseClass: 'unknown_error',
    errorClass: 'unknown_error',
    errorMessage: String(err),
  };
}

export class MidgardNodeClient {
  private readonly baseUrl: string;
  private readonly retryAttempts: number;
  private readonly retryDelay: number;
  private readonly enableLogs: boolean;
  private readonly skipAvailabilityCheck: boolean;

  constructor(config: MidgardNodeConfig) {
    this.baseUrl = config.baseUrl;
    this.retryAttempts = config.retryAttempts ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_ATTEMPTS;
    this.retryDelay = config.retryDelay ?? TRANSACTION_CONSTANTS.NODE_DEFAULTS.RETRY_DELAY;
    this.enableLogs = config.enableLogs ?? true;
    this.skipAvailabilityCheck = config.skipAvailabilityCheck ?? false;
  }

  /**
   * Check node availability by making a dummy request
   */
  async isAvailable(
    timeoutMs: number = TRANSACTION_CONSTANTS.NODE_DEFAULTS.AVAILABILITY_TIMEOUT
  ): Promise<boolean> {
    try {
      // Create an AbortController to allow for timeouts
      const controller = new AbortController();
      const signal = controller.signal;

      // Set a timeout if provided
      let timeoutId: NodeJS.Timeout | undefined;
      if (timeoutMs) {
        timeoutId = setTimeout(() => controller.abort(), timeoutMs);
      }

      try {
        // Try to fetch a dummy transaction status - if the node is up, it will return 404
        // If the node is down, it will throw a connection error
        const response = await fetch(`${this.baseUrl}/tx?tx_hash=${'0'.repeat(64)}`, { signal });

        // Clear timeout
        if (timeoutId) clearTimeout(timeoutId);

        return response.status === 404;
      } catch (error) {
        // Clear timeout to prevent memory leaks
        if (timeoutId) clearTimeout(timeoutId);

        // Check if this was a timeout abort or a different error
        if (error instanceof DOMException && error.name === 'AbortError') {
          return false;
        }

        throw error; // Re-throw other errors to be caught by the outer try/catch
      }
    } catch {
      return false;
    }
  }

  /**
   * Submit a transaction to the node with retries
   */
  async submitTransaction(
    cborHex: string,
    txType: string = 'Transaction',
    options: SubmitTransactionOptions = {}
  ): Promise<SubmitTransactionResult> {
    const startedAtMs = Date.now();
    const skipAvailabilityCheck =
      this.skipAvailabilityCheck || options.skipAvailabilityCheck === true;

    if (!skipAvailabilityCheck) {
      const isNodeAvailable = await this.isAvailable();
      if (!isNodeAvailable) {
        return {
          status: 'NODE_UNAVAILABLE',
          message: 'Node is not available - transaction will be stored locally',
          responseClass: 'node_unavailable',
          latencyMs: Date.now() - startedAtMs,
          attempts: 0,
          retriesUsed: 0,
        };
      }
    }

    let attempts = 0;
    while (attempts < this.retryAttempts) {
      attempts += 1;
      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(
          () => controller.abort(),
          TRANSACTION_CONSTANTS.NODE_DEFAULTS.AVAILABILITY_TIMEOUT
        );
        let response: Response;
        try {
          response = await fetch(`${this.baseUrl}/submit?tx_cbor=${encodeURIComponent(cborHex)}`, {
            method: 'POST',
            headers: {
              'Content-Type': 'text/plain',
            },
            signal: controller.signal,
          });
        } finally {
          clearTimeout(timeoutId);
        }

        if (!response.ok) {
          let errorMessage = `Unexpected status: ${response.status}`;
          try {
            const payload = await response.json();
            if (payload && typeof payload.message === 'string') {
              errorMessage = payload.message;
            }
          } catch {
            // Best effort only.
          }
          throw new SubmitHttpError(response.status, errorMessage);
        }

        const result = await response.json();
        const txId = typeof result?.txId === 'string' ? result.txId : undefined;
        if (this.enableLogs && txId !== undefined) {
          logSubmittedTransaction(txId, txType);
        }
        return {
          status: 'SUBMITTED',
          txId,
          responseClass: 'submitted',
          httpStatusCode: response.status,
          latencyMs: Date.now() - startedAtMs,
          attempts,
          retriesUsed: Math.max(attempts - 1, 0),
        };
      } catch (error) {
        if (attempts >= this.retryAttempts) {
          const classified = classifySubmitFailure(error);
          if (this.enableLogs) {
            logFailedTransaction('unknown', txType, classified.errorMessage);
          }
          return {
            status: 'ERROR',
            error: classified.errorMessage,
            errorClass: classified.errorClass,
            responseClass: classified.responseClass,
            httpStatusCode: classified.httpStatusCode,
            latencyMs: Date.now() - startedAtMs,
            attempts,
            retriesUsed: Math.max(attempts - 1, 0),
          };
        }
        await new Promise((resolve) => setTimeout(resolve, this.retryDelay));
      }
    }

    return {
      status: 'ERROR',
      error: 'All retry attempts failed',
      errorClass: 'unknown_error',
      responseClass: 'unknown_error',
      latencyMs: Date.now() - startedAtMs,
      attempts,
      retriesUsed: Math.max(attempts - 1, 0),
    };
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
