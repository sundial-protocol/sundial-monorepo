import { Command } from '@effect/cli';
import { NodeContext, NodeRuntime } from '@effect/platform-node';
import { Effect } from 'effect';

import { run } from './Cli.js';
import { generateTxCommand, stopTxCommand, txStatusCommand } from './commands/generate-tx.js';
import { walletCommand } from './commands/wallet.js';

// Main CLI
const cli = Command.make('midgard-manager')
  .pipe(Command.withDescription('Midgard Manager CLI'))
  .pipe(
    Command.withSubcommands([
      // Add our new commands
      walletCommand,
      generateTxCommand,
      stopTxCommand,
      txStatusCommand,

      // If you have existing commands, add them here:
      // existingCommand1,
      // existingCommand2,
    ])
  );

// Run the CLI
Effect.runPromise(
  run(process.argv).pipe(
    Effect.provide(NodeContext.layer),
    NodeRuntime.runMain({ disableErrorReporting: true })
  )
);
