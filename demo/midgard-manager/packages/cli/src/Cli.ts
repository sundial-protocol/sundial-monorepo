import * as Command from '@effect/cli/Command';

import { interactiveCommand } from './cli/interactive/index.js';
import { generateTxCommand, stopTxCommand, txStatusCommand } from './commands/generate-tx.js';
import { configureNodeCommand, nodeStatusCommand } from './commands/node.js';
import { scheduleTxCommand } from './commands/scheduler/schedule-tx.js';
import { walletCommand } from './commands/wallet.js';

/**
 * Main CLI help text
 */
const helpText = `Midgard Manager - CLI tool for Cardano development

CONFIGURATION:

Configuration is stored in the project's config directory:
- Main settings: config/settings.json
- Wallet configs: config/wallets/default.json

Example configuration structure:
{
  "node": {
    "endpoint": "http://localhost:3000"
  },
  "generator": {
    "enabled": true,
    "maxConcurrent": 10,
    "batchSize": 100,
    "intervalMs": 1000,
    "transactionType": "mixed",
    "oneToOneRatio": 70,
    "defaultWallet": "test"
  },
  "logging": {
    "level": "info",
    "format": "pretty"
  }
}

EXAMPLES:
  
# Interactive mode
$ pnpm start interactive
  
# Wallet management
$ pnpm start wallet list                                 # List all wallets
$ pnpm start wallet details test                         # Show wallet details
$ pnpm start wallet add myWallet --private-key <key>     # Add a new wallet
$ pnpm start wallet remove myWallet                      # Remove a wallet
  
# Transaction generation
$ pnpm start tx generate --interactive                   # Interactive setup
$ pnpm start tx generate --type mixed --batch-size 10    # Direct configuration
$ pnpm start tx status                                   # Check generator status
$ pnpm start tx stop                                     # Stop the generator
  
# Scheduled transactions
$ pnpm start tx schedule add --name "Hourly Test" --interactive    # Add schedule
$ pnpm start tx schedule list                                      # List schedules
$ pnpm start tx schedule toggle <id>                               # Enable/disable
$ pnpm start tx schedule remove <id>                               # Remove schedule
  
# Node commands
$ pnpm start node status                                # Check node status
$ pnpm start node configure --interactive               # Configure node settings`;

/**
 * Group transaction commands
 */
const txCommands = Command.make('tx')
  .pipe(Command.withDescription('Transaction generator operations'))
  .pipe(
    Command.withSubcommands([
      // Instead of renaming, we'll use the original commands since they have the right functionality
      generateTxCommand,
      stopTxCommand,
      txStatusCommand,
      scheduleTxCommand,
    ])
  );

/**
 * Group node commands
 */
const nodeCommands = Command.make('node')
  .pipe(Command.withDescription('Midgard node operations'))
  .pipe(Command.withSubcommands([nodeStatusCommand, configureNodeCommand]));

/**
 * Create the main command with subcommands
 */
const mainCommand = Command.make('midgard-manager')
  .pipe(Command.withDescription(helpText))
  .pipe(
    Command.withSubcommands([
      // Interactive mode
      interactiveCommand,

      // Command groups
      nodeCommands,
      txCommands,
      walletCommand,
    ])
  );

/**
 * Export the run function
 */
export const run = Command.run(mainCommand, {
  name: 'Midgard Manager',
  version: '0.1.0',
});
