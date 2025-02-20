import { Effect, Schedule } from 'effect';
import { MidgardNodeClient, SubmitTxError } from '../client/node-client.js';
import { metrics } from './metrics.js';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../generators/index.js';
import { Network } from '@lucid-evolution/lucid';
import type { UTxO } from '@lucid-evolution/lucid';
import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';

// Internal constants for multi-output transactions
const OUTPUTS_PER_DISTRIBUTION = 20;
const FINAL_UTXO_COUNT = 1;

export interface TransactionConfig {
  oneToOne: {
    transactionsPerBatch: number; // Number of 1-1 transactions to generate per batch
  };
  multiOutput: {
    transactionsPerBatch: number; // Number of complex transactions to generate per batch
  };
}

export interface SchedulerConfig {
  nodeClient: MidgardNodeClient;
  network: Network;
  initialUTxO: UTxO;
  intervalMs: number;
  transactionConfig: TransactionConfig;
  walletSeedOrPrivateKey: string;
  outputDir?: string;
}

const DEFAULT_CONFIG: TransactionConfig = {
  oneToOne: {
    transactionsPerBatch: 10, // Default: 10 transactions per batch
  },
  multiOutput: {
    transactionsPerBatch: 10, // Default: 10 complex transactions per batch
  },
};

/**
 * Creates a transaction generator that alternates between one-to-one and multi-output transactions.
 * If node is unreachable, transactions are written to files in the output directory.
 */
export const createScheduledGenerator = async (config: SchedulerConfig) => {
  const {
    nodeClient,
    network,
    initialUTxO,
    intervalMs,
    transactionConfig = DEFAULT_CONFIG,
    walletSeedOrPrivateKey,
    outputDir = 'generated-transactions',
  } = config;

  await mkdir(join(process.cwd(), outputDir), { recursive: true });
  let isOneToOne = true;

  const generateAndSubmitBatch = Effect.gen(function* (_) {
    try {
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
      const txs = isOneToOne
        ? yield* _(
            Effect.promise(() =>
              generateOneToOneTransactions({
                network,
                initialUTxO,
                txsCount: transactionConfig.oneToOne.transactionsPerBatch,
                walletSeedOrPrivateKey,
              })
            )
          )
        : yield* _(
            Effect.promise(() =>
              generateMultiOutputTransactions({
                network,
                initialUTxO,
                utxosCount:
                  OUTPUTS_PER_DISTRIBUTION *
                  transactionConfig.multiOutput.transactionsPerBatch,
                finalUtxosCount: FINAL_UTXO_COUNT,
                walletSeedOrPrivateKey,
              })
            )
          );

      if (!txs || !Array.isArray(txs)) {
        throw new Error('Failed to generate transactions');
      }

      const nodeAvailable = yield* _(
        Effect.promise(() => nodeClient.isAvailable())
      );

      if (!nodeAvailable) {
        const filename = `${isOneToOne ? 'one-to-one' : 'multi-output'}-${timestamp}.json`;
        const filepath = join(process.cwd(), outputDir, filename);

        yield* _(
          Effect.promise(() =>
            writeFile(filepath, JSON.stringify(txs, null, 2))
          )
        );

        console.log(`Node unavailable - transactions written to ${filepath}`);
        metrics.recordSubmission(false, txs.length);
      } else {
        try {
          const submissionStart = Date.now();
          yield* _(
            Effect.promise(async () => {
              for (const tx of txs) {
                const result = await Effect.runPromise(
                  nodeClient.submitTransaction(tx.cborHex)
                );
                if (result._tag !== 'ok') {
                  throw new Error(result.error);
                }
              }
            })
          );
          const submissionEnd = Date.now();

          metrics.recordSubmission(
            true,
            txs.length,
            submissionEnd - submissionStart
          );
          console.log(`Submitted ${txs.length} transactions`);
        } catch (submitError) {
          console.error('Failed to submit transactions:', submitError);

          const filename = `${isOneToOne ? 'one-to-one' : 'multi-output'}-${timestamp}.json`;
          const filepath = join(process.cwd(), outputDir, filename);

          yield* _(
            Effect.promise(() =>
              writeFile(filepath, JSON.stringify(txs, null, 2))
            )
          );

          console.log(
            `Failed submission - transactions written to ${filepath}`
          );
          metrics.recordSubmission(false, txs.length);
        }
      }

      isOneToOne = !isOneToOne;
    } catch (error) {
      console.error('Failed to generate transactions:', error);
      metrics.recordSubmission(false, 0);
    }
  });

  const scheduledEffect = generateAndSubmitBatch.pipe(
    Effect.schedule(Schedule.spaced(intervalMs))
  );

  return {
    start: () => Effect.runPromise(scheduledEffect),
    getMetrics: () => metrics.getStats(),
    stop: () => {
      console.log('Stopping generator...');
      return metrics.getStats();
    },
  };
};
