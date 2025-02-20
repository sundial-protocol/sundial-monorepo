#!/usr/bin/env node

import { Network } from '@lucid-evolution/lucid';
import { MidgardNodeClient } from '../lib/client/node-client.js';
import { createScheduledGenerator } from '../lib/scheduler/scheduler.js';
import { generateTestWallet } from '../utils/test-utils.js';

async function main() {
  try {
    const { privateKey, testUTxO } = await generateTestWallet();

    // Initialize node client
    const nodeClient = new MidgardNodeClient({
      baseUrl: process.env.MIDGARD_NODE_URL || 'http://localhost:3000',
    });

    // Parse transaction configuration
    const transactionConfig = {
      oneToOne: {
        transactionsPerBatch: parseInt(process.env.ONE_TO_ONE_TXS || '10', 10),
      },
      multiOutput: {
        transactionsPerBatch: parseInt(process.env.COMPLEX_TXS || '10', 10),
      },
    };

    // Calculate multi-output transaction count
    const multiOutputTxCount =
      transactionConfig.multiOutput.transactionsPerBatch * 2; // Distribution + Collection phases

    // Create and start the transaction generator
    const generator = await createScheduledGenerator({
      nodeClient,
      network: (process.env.CARDANO_NETWORK || 'Preview') as Network,
      initialUTxO: testUTxO,
      intervalMs: parseInt(process.env.TX_INTERVAL_MS || '5000', 10),
      transactionConfig,
      walletSeedOrPrivateKey: privateKey,
      outputDir: process.env.OUTPUT_DIR || 'generated-transactions',
    });

    console.log('\nMidgard Transaction Generator\n');

    // Network Settings
    console.log('Network Settings:');
    console.log('  Network:', process.env.CARDANO_NETWORK || 'Preview');
    console.log(
      '  Node URL:',
      process.env.MIDGARD_NODE_URL || 'http://localhost:3000'
    );
    console.log(
      '  Generation Interval:',
      process.env.TX_INTERVAL_MS || '5000',
      'ms'
    );
    console.log();

    // Transaction Pattern
    console.log('Transaction Generation Pattern (per cycle):');
    console.log('\n1. Simple Transactions:');
    console.log(
      `  • ${transactionConfig.oneToOne.transactionsPerBatch} one-to-one transactions`
    );
    console.log('  • Each transaction: 1 input → 1 output');

    console.log('\n2. Complex Transactions:');
    console.log(
      `  • ${transactionConfig.multiOutput.transactionsPerBatch} complex transaction pairs, generating:`
    );
    console.log(
      `    - ${transactionConfig.multiOutput.transactionsPerBatch} distribution txs (1 → 20 outputs)`
    );
    console.log(
      `    - ${transactionConfig.multiOutput.transactionsPerBatch} collection txs (20 → 1 output)`
    );
    console.log(`    = ${multiOutputTxCount} total complex transactions`);

    const totalTx =
      transactionConfig.oneToOne.transactionsPerBatch + multiOutputTxCount;
    console.log(`\nTotal Transactions Per Cycle: ${totalTx}`);

    // Wallet Info
    console.log('\nTest Wallet:');
    console.log('  Address:', testUTxO.address);
    console.log('  Balance:', testUTxO.assets.lovelace.toString(), 'lovelace');
    console.log(); // Empty line for spacing

    // Handle graceful shutdown
    process.on('SIGINT', () => {
      console.log('\nReceived SIGINT. Stopping generator...');
      const stats = generator.stop();
      console.log('Final stats:', stats);
      process.exit(0);
    });

    // Start generating transactions
    await generator.start();
  } catch (error) {
    console.error('Error running transaction generator:', error);
    process.exit(1);
  }
}

main();
