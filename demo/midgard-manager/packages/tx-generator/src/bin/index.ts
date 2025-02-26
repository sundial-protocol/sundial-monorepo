#!/usr/bin/env node

import chalk from 'chalk';
import { Command } from 'commander';

import { getGeneratorStatus, startGenerator, stopGenerator } from '../lib/scheduler/scheduler.js';
import { generateTestWallet } from '../utils/test-utils.js';

interface GeneratorOptions {
  endpoint: string;
  type: 'one-to-one' | 'multi-output' | 'mixed';
  ratio: string;
  batchSize: string;
  interval: string;
  concurrency: string;
  testWallet: boolean;
  privateKey?: string;
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
  .option('--test-wallet', 'Generate a test wallet for transactions', false)
  .option('-k, --private-key <key>', 'Wallet private key (required if --test-wallet is not used)')
  .action(async (options: GeneratorOptions) => {
    try {
      let privateKey = options.privateKey;

      if (options.testWallet) {
        console.log(chalk.yellow('Generating test wallet...'));
        const { privateKey: testKey } = await generateTestWallet();
        privateKey = testKey;
      } else if (!privateKey) {
        console.error(chalk.red('Error: Either --private-key or --test-wallet must be provided'));
        process.exit(1);
      }

      console.log(chalk.blue('\nStarting transaction generator with configuration:'));
      console.log(chalk.gray(`Node Endpoint: ${options.endpoint}`));
      console.log(chalk.gray(`Transaction Type: ${options.type}`));
      if (options.type === 'mixed') {
        console.log(chalk.gray(`One-to-One Ratio: ${options.ratio}%`));
      }
      console.log(chalk.gray(`Batch Size: ${options.batchSize}`));
      console.log(chalk.gray(`Interval: ${options.interval} seconds`));
      console.log(chalk.gray(`Concurrency: ${options.concurrency}\n`));

      await startGenerator({
        nodeEndpoint: options.endpoint,
        walletPrivateKey: privateKey,
        transactionType: options.type,
        oneToOneRatio: parseInt(options.ratio),
        batchSize: parseInt(options.batchSize),
        interval: parseInt(options.interval) * 1000,
        concurrency: parseInt(options.concurrency),
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

// Parse command line arguments
program.parse();
