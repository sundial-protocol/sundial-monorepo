import { Command } from "@effect/cli";
import { walletCommand } from "./commands/wallet.js";
import {
  generateTxCommand,
  stopTxCommand,
  txStatusCommand,
} from "./commands/generate-tx.js";

// Main CLI
const cli = Command.make("midgard-manager")
  .pipe(Command.withDescription("Midgard Manager CLI"))
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
Command.run(cli, {
  name: "midgard-manager",
  version: "0.1.0",
});
