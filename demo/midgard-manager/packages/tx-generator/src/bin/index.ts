#!/usr/bin/env node

import { Network } from '@lucid-evolution/lucid';
import {
  generateCorpus,
  generateTestWallet,
  getAddressFromPrivateKey,
} from '@midgard-manager/tx-builder';
import chalk from 'chalk';
import { Command } from 'commander';

import { getGeneratorStatus, startGenerator, stopGenerator } from '../lib/scheduler/scheduler.js';

interface GeneratorOptions {
  endpoint: string;
  type: 'one-to-one' | 'multi-output' | 'mixed';
  ratio: string;
  batchSize: string;
  interval: string;
  concurrency: string;
  targetTps?: string;
  maxInFlight?: string;
  generationConcurrency?: string;
  preparedQueueCapacity?: string;
  testWallet: boolean;
  privateKey?: string;
  network?: string;
  outputDir?: string;
  seed?: string;
  replayCorpusPath?: string;
  replayStartIndex?: string;
  replayCount?: string;
  retryAttempts?: string;
  retryDelayMs?: string;
  submitTimeoutMs?: string;
  requestEvents?: 'off' | 'sampled' | 'all';
}

const program = new Command();

// Setup CLI metadata
program
  .name('midgard-tx-generator')
  .description('Transaction generator for Midgard L2')
  .version('0.1.0');

// Start command
program
  .command('start')
  .description('Start the transaction generator')
  .option('-e, --endpoint <url>', 'Node endpoint URL', 'http://localhost:3000')
  .option('-t, --type <type>', 'Transaction type (one-to-one, multi-output, mixed)', 'mixed')
  .option('-r, --ratio <number>', 'Percentage of one-to-one transactions in mixed mode', '70')
  .option('-b, --batch-size <number>', 'Number of transactions per batch', '10')
  .option('-i, --interval <seconds>', 'Interval between batches in seconds', '5')
  .option('-c, --concurrency <number>', 'Number of concurrent batches', '5')
  .option('--target-tps <number>', 'Submission token bucket target TPS')
  .option('--max-in-flight <number>', 'Maximum concurrent in-flight submissions')
  .option('--generation-concurrency <number>', 'Concurrent transaction generation workers')
  .option('--prepared-queue-capacity <number>', 'Prepared submission queue capacity')
  .option('--test-wallet', 'Generate a test wallet for transactions', false)
  .option('-k, --private-key <key>', 'Wallet private key (required if --test-wallet is not used)')
  .option('-n, --network <network>', 'Network to use (Preview/Mainnet)', 'Preview')
  .option(
    '-o, --output-dir <dir>',
    'Directory to save transactions when node is unavailable',
    'generated-transactions'
  )
  .option('--seed <seed>', 'Deterministic seed for reproducible transaction generation')
  .option('--retry-attempts <number>', 'Node submission retry attempts (1 = no retry)', '3')
  .option('--retry-delay-ms <number>', 'Delay between submission retries in milliseconds', '1000')
  .option(
    '--submit-timeout-ms <number>',
    'Per-attempt submit timeout in ms; set low (e.g. 500) for fast-fail load testing',
    '5000'
  )
  .option('--request-events <mode>', 'Per-request submission event mode (off|sampled|all)', 'off')
  .option(
    '--replay-corpus-path <path>',
    'Replay transactions from a JSON corpus (array or { transactions: [] })'
  )
  .option(
    '--replay-start-index <number>',
    '0-based start index in replay corpus (transaction index for JSON, non-empty line index for JSONL)'
  )
  .option('--replay-count <number>', 'Maximum number of replay entries to consume')
  .action(async (options: GeneratorOptions) => {
    try {
      let walletSeedOrPrivateKey = options.privateKey;
      let initialUTxO;

      if (options.testWallet) {
        console.log(chalk.yellow('Generating test wallet...'));
        const wallet = await generateTestWallet();
        walletSeedOrPrivateKey = wallet.privateKey;

        // Create initial UTxO with test funds
        initialUTxO = {
          txHash: Buffer.from(Array(32).fill(0)).toString('hex'),
          outputIndex: 0,
          address: wallet.address,
          assets: {
            lovelace: 10_000_000_000n,
          },
          datum: null,
          datumHash: null,
          scriptRef: null,
        };

        console.log(chalk.gray(`Generated test wallet with address: ${wallet.address}`));
      } else if (!walletSeedOrPrivateKey) {
        console.error(chalk.red('Error: Either --private-key or --test-wallet must be provided'));
        process.exit(1);
      }

      console.log(chalk.blue('\nStarting transaction generator with configuration:'));
      console.log(chalk.gray(`Node Endpoint: ${options.endpoint}`));
      console.log(chalk.gray(`Network: ${options.network}`));
      console.log(chalk.gray(`Transaction Type: ${options.type}`));
      if (options.type === 'mixed') {
        console.log(chalk.gray(`One-to-One Ratio: ${options.ratio}%`));
      }
      console.log(chalk.gray(`Batch Size: ${options.batchSize}`));
      console.log(chalk.gray(`Interval: ${options.interval} seconds`));
      console.log(chalk.gray(`Concurrency: ${options.concurrency}\n`));
      if (options.targetTps !== undefined) {
        console.log(chalk.gray(`Target TPS: ${options.targetTps}`));
      }
      if (options.maxInFlight !== undefined) {
        console.log(chalk.gray(`Max In Flight: ${options.maxInFlight}`));
      }
      if (options.generationConcurrency !== undefined) {
        console.log(chalk.gray(`Generation Concurrency: ${options.generationConcurrency}`));
      }
      if (options.preparedQueueCapacity !== undefined) {
        console.log(chalk.gray(`Prepared Queue Capacity: ${options.preparedQueueCapacity}`));
      }
      console.log(chalk.gray(`Retry Attempts: ${options.retryAttempts}`));
      console.log(chalk.gray(`Retry Delay: ${options.retryDelayMs}ms`));
      console.log(
        chalk.gray(`Submit Timeout: ${options.submitTimeoutMs ?? '5000'}ms (per attempt)`)
      );
      console.log(chalk.gray(`Request Events: ${options.requestEvents}`));
      if (options.seed) {
        console.log(chalk.gray(`Generation Seed: ${options.seed}`));
      }
      if (options.replayCorpusPath) {
        console.log(chalk.gray(`Replay Corpus Path: ${options.replayCorpusPath}`));
        if (options.replayStartIndex !== undefined) {
          console.log(chalk.gray(`Replay Start Index: ${options.replayStartIndex}`));
        }
        if (options.replayCount !== undefined) {
          console.log(chalk.gray(`Replay Count: ${options.replayCount}`));
        }
      }

      await startGenerator({
        nodeEndpoint: options.endpoint,
        network: options.network as Network,
        initialUTxO,
        walletSeedOrPrivateKey,
        transactionType: options.type,
        oneToOneRatio: parseInt(options.ratio),
        batchSize: parseInt(options.batchSize),
        interval: parseFloat(options.interval),
        concurrency: parseInt(options.concurrency),
        targetTps:
          options.targetTps !== undefined ? Number.parseFloat(options.targetTps) : undefined,
        maxInFlight:
          options.maxInFlight !== undefined ? Number.parseInt(options.maxInFlight, 10) : undefined,
        generationConcurrency:
          options.generationConcurrency !== undefined
            ? Number.parseInt(options.generationConcurrency, 10)
            : undefined,
        preparedQueueCapacity:
          options.preparedQueueCapacity !== undefined
            ? Number.parseInt(options.preparedQueueCapacity, 10)
            : undefined,
        nodeRetryAttempts: parseInt(options.retryAttempts ?? '3'),
        nodeRetryDelay: parseInt(options.retryDelayMs ?? '1000'),
        nodeSubmitTimeoutMs: parseInt(options.submitTimeoutMs ?? '5000'),
        outputDir: options.outputDir,
        requestEvents: options.requestEvents ?? 'off',
        generationSeed: options.seed,
        replayCorpusPath: options.replayCorpusPath,
        replayStartIndex:
          options.replayStartIndex !== undefined
            ? Number.parseInt(options.replayStartIndex, 10)
            : undefined,
        replayCount:
          options.replayCount !== undefined ? Number.parseInt(options.replayCount, 10) : undefined,
      });

      console.log(chalk.green('\nGenerator started successfully!'));
      console.log(chalk.gray('Press Ctrl+C to stop'));

      process.on('SIGINT', async () => {
        console.log(chalk.yellow('\nStopping generator...'));
        await stopGenerator();
        process.exit(0);
      });
    } catch (error) {
      console.error(chalk.red('\nFailed to start generator:'), error);
      process.exit(1);
    }
  });

// Status command
program
  .command('status')
  .description('Get the current status of the transaction generator')
  .action(() => {
    const status = getGeneratorStatus();
    console.log(chalk.blue('\nTransaction Generator Status:'));
    console.log(chalk.gray(JSON.stringify(status, null, 2)));
  });

// Generate-corpus command — generates transactions without submitting them.
// The output file is a replay corpus usable with --replay-corpus-path.
program
  .command('generate-corpus')
  .description('Pre-generate transactions and write them to a JSON corpus file (no submission)')
  .requiredOption('-o, --output <path>', 'Path to write the corpus JSON file')
  .requiredOption('-n, --count <number>', 'Number of transactions to generate')
  .option('-t, --type <type>', 'Transaction type (one-to-one, multi-output, mixed)', 'one-to-one')
  .option('-r, --ratio <number>', 'Percentage of one-to-one transactions in mixed mode', '70')
  .option('-s, --seed <seed>', 'Deterministic seed (default: random)')
  .option('--network <network>', 'Network (Preview/Mainnet)', 'Preview')
  .option('--test-wallet', 'Generate a fresh test wallet private key', false)
  .option('-k, --private-key <key>', 'Wallet private key')
  .action(
    async (opts: {
      output: string;
      count: string;
      type: 'one-to-one' | 'multi-output' | 'mixed';
      ratio: string;
      seed?: string;
      network?: string;
      testWallet: boolean;
      privateKey?: string;
    }) => {
      const count = parseInt(opts.count, 10);
      if (!Number.isFinite(count) || count < 1) {
        console.error(chalk.red('--count must be a positive integer'));
        process.exit(1);
      }

      const network = (opts.network ?? 'Preview') as 'Preview' | 'Mainnet';
      const networkId: 0 | 1 = network === 'Mainnet' ? 1 : 0;

      let walletSeedOrPrivateKey: string;
      let address: string;

      if (opts.testWallet) {
        const wallet = await generateTestWallet();
        walletSeedOrPrivateKey = wallet.privateKey;
        address = wallet.address;
        console.log(chalk.gray(`Generated test wallet: ${address}`));
      } else if (opts.privateKey) {
        walletSeedOrPrivateKey = opts.privateKey;
        address = getAddressFromPrivateKey(walletSeedOrPrivateKey, networkId);
      } else {
        console.error(chalk.red('Either --private-key or --test-wallet must be provided'));
        process.exit(1);
        return;
      }

      const seed = opts.seed ?? Math.random().toString(36).slice(2);
      const initialUTxO = {
        txHash: '0'.repeat(64),
        outputIndex: 0,
        address,
        assets: { lovelace: 10_000_000_000n },
        datum: null,
        datumHash: null,
        scriptRef: null,
      };

      console.log(chalk.blue(`Generating ${count} transactions...`));
      console.log(chalk.gray(`Type: ${opts.type} | Seed: ${seed} | Output: ${opts.output}`));

      let lastPct = -1;
      await generateCorpus({
        count,
        walletSeedOrPrivateKey,
        transactionType: opts.type,
        oneToOneRatio: parseInt(opts.ratio, 10),
        network,
        initialUTxO,
        seed,
        outputPath: opts.output,
        onProgress(generated: number, total: number) {
          const pct = Math.floor((generated / total) * 100);
          if (pct !== lastPct && pct % 5 === 0) {
            lastPct = pct;
            const filled = Math.floor(pct / 5);
            const bar = '='.repeat(filled) + '-'.repeat(20 - filled);
            process.stdout.write(`\r  [${bar}] ${pct}% (${generated}/${total})`);
          }
        },
      });

      process.stdout.write('\n');
      console.log(chalk.green(`Done — corpus written to ${opts.output}`));
    }
  );

// Parse command line arguments
program.parse();
