import { Args, Command, Options } from '@effect/cli';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';

import { addWallet, getWallet, listWallets, removeWallet } from '../config/wallets.js';

// Validate ed25519 private key format
const PRIVATE_KEY_REGEX = /^ed25519_sk[a-zA-Z0-9]+$/;

/**
 * Add a new wallet
 */
const addCommand = Command.make(
  'add',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to add')),
    privateKey: Options.text('private-key').pipe(
      Options.withDescription('Private key for the wallet (ed25519 format)')
    ),
  },
  ({ name, privateKey }) => {
    return pipe(
      Effect.tryPromise(async () => {
        // Validate wallet name
        if (!name || name.trim() === '') {
          console.error(chalk.red('❌ Wallet name cannot be empty'));
          return;
        }

        // Validate private key format
        if (!PRIVATE_KEY_REGEX.test(privateKey)) {
          console.error(chalk.red('❌ Invalid private key format. Expected ed25519_sk format'));
          return;
        }

        // Check if wallet already exists
        const existingWallet = await getWallet(name);
        if (existingWallet) {
          const overwrite = await Effect.runPromise(
            Effect.tryPromise(() =>
              import('@inquirer/prompts').then(({ confirm }) =>
                confirm({
                  message: `Wallet '${name}' already exists. Overwrite?`,
                  default: false,
                })
              )
            )
          );

          if (!overwrite) {
            console.log(chalk.yellow('⚠️ Operation cancelled'));
            return;
          }
        }

        await addWallet(name, privateKey);
        console.log(chalk.green(`✓ Wallet '${name}' added successfully`));
      })
    );
  }
);

/**
 * Command to remove a wallet
 * Usage: midgard-manager wallet remove <name>
 */
const removeCommand = Command.make(
  'remove',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to remove')),
  },
  ({ name }) => {
    return pipe(
      Effect.tryPromise(async () => {
        if (name === 'test') {
          console.error(chalk.red('❌ Cannot remove the default test wallet'));
          return;
        }

        // Check if wallet exists
        const wallet = await getWallet(name);
        if (!wallet) {
          console.error(chalk.red(`❌ Wallet '${name}' not found`));
          return;
        }

        // Confirm deletion
        const confirm = await Effect.runPromise(
          Effect.tryPromise(() =>
            import('@inquirer/prompts').then(({ confirm }) =>
              confirm({
                message: `Are you sure you want to remove wallet '${name}'?`,
                default: false,
              })
            )
          )
        );

        if (!confirm) {
          console.log(chalk.yellow('⚠️ Operation cancelled'));
          return;
        }

        await removeWallet(name);
        console.log(chalk.green(`✓ Wallet '${name}' removed successfully`));
      })
    );
  }
);

/**
 * Command to list all wallets
 * Usage: midgard-manager wallet list
 */
const listCommand = Command.make('list', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const wallets = await listWallets();

      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets configured'));
        return;
      }

      console.log(chalk.blue('Available wallets:'));
      wallets.forEach((name) => {
        const isDefault = name === 'test';
        console.log(` ${chalk.green('•')} ${name}${isDefault ? chalk.gray(' (default)') : ''}`);
      });

      console.log();
      console.log(chalk.gray('For details on a specific wallet, use:'));
      console.log(chalk.gray(`$ midgard-manager wallet details <name>`));
    })
  );
});

/**
 * Command to show details of a specific wallet
 * Usage: midgard-manager wallet details <name>
 */
const detailsCommand = Command.make(
  'details',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to show details for')),
  },
  ({ name }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const wallet = await getWallet(name);

        if (!wallet) {
          console.error(chalk.red(`❌ Wallet '${name}' not found`));
          return;
        }

        console.log(chalk.blue(`Details for wallet: ${name}`));

        // Show masked private key for security
        const maskedKey =
          wallet.privateKey.substring(0, 10) +
          '...' +
          wallet.privateKey.substring(wallet.privateKey.length - 5);

        console.log(chalk.gray(`Private Key: ${maskedKey}`));
        console.log();
        console.log(chalk.gray('To use this wallet with transaction generation:'));
        console.log(chalk.gray(`$ midgard-manager generate-tx --wallet ${name}`));
      })
    );
  }
);

/**
 * Main wallet command group
 * Usage: midgard-manager wallet <subcommand>
 */
export const walletCommand = Command.make('wallet')
  .pipe(Command.withDescription('Manage wallets for transaction signing'))
  .pipe(Command.withSubcommands([addCommand, removeCommand, listCommand, detailsCommand]));
