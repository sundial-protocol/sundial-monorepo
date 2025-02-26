import * as Command from "@effect/cli/Command";
import { interactiveCommand } from "./cli/interactive/index.js";
import { walletCommand } from "./commands/wallet.js";
import {
  generateTxCommand,
  stopTxCommand,
  txStatusCommand,
} from "./commands/generate-tx.js";
import { nodeStatusCommand, configureNodeCommand } from "./commands/node.js";
import { scheduleTxCommand } from "./commands/scheduler/schedule-tx.js";

/**
 * Main CLI help text
 */
const helpText = `Midgard Manager - CLI tool for Cardano development
  
EXAMPLES:
  
# Interactive mode (recommended for new users)
$ midgard interactive
  
# Wallet management
$ midgard wallet list
$ midgard wallet details test
$ midgard wallet add myWallet --private-key ed25519_sk...
  
# Transaction generation
$ midgard tx generate --interactive
$ midgard tx status
$ midgard tx stop
  
# Scheduled transactions
$ midgard tx schedule add --name "Hourly Test" --interactive
$ midgard tx schedule list
  
# Node commands
$ midgard node status
$ midgard node configure --interactive
`;

/**
 * Group transaction commands
 */
const txCommands = Command.make("tx")
  .pipe(Command.withDescription("Transaction generator operations"))
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
const nodeCommands = Command.make("node")
  .pipe(Command.withDescription("Midgard node operations"))
  .pipe(Command.withSubcommands([nodeStatusCommand, configureNodeCommand]));

/**
 * Create the main command with subcommands
 */
const mainCommand = Command.make("midgard-manager")
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
  name: "Midgard Manager",
  version: "0.1.0",
});
