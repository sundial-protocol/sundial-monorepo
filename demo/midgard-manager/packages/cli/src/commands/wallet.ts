import { Args, Command, Options } from '@effect/cli';
import { MidgardNodeClient } from '@midgard-manager/tx-generator';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';

import {
  createWallet,
  getWallet,
  importWallet,
  listWallets,
  removeWallet,
} from '../config/wallets.js';

export const maskKey = (privateKey: string) =>
  `${privateKey.substring(0, 10)}...${privateKey.slice(-5)}`;

const endpointOption = Options.text('endpoint')
  .pipe(Options.withDescription('Midgard node endpoint URL'))
  .pipe(Options.withDefault('http://localhost:3000'));

const nameArg = Args.text({ name: 'NAME' }).pipe(Args.withDescription('Wallet name'));

/**
 * Command to generate a new wallet
 */
const createCommand = Command.make('create', { name: nameArg }, ({ name }) => {
  return pipe(
    Effect.tryPromise(async () => {
      try {
        const wallet = await createWallet(name);
        console.log(chalk.green(`✓ Created wallet: ${name}`));
        console.log(chalk.gray(`Address: ${wallet.address}`));
        console.log(chalk.gray(`Private Key: ${maskKey(wallet.privateKey)}`));
        console.log();
        console.log(chalk.gray('Fund it from the faucet, then check its balance with:'));
        console.log(chalk.gray(`$ midgard wallet balance ${name}`));
      } catch (error) {
        console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
      }
    })
  );
}).pipe(Command.withDescription('Generate a new wallet with a fresh keypair'));

/**
 * Command to import an existing private key as a wallet
 */
const importCommand = Command.make(
  'import',
  {
    name: nameArg,
    privateKey: Options.text('private-key').pipe(
      Options.withDescription('ed25519 private key in bech32 format (ed25519_sk...)')
    ),
  },
  ({ name, privateKey }) => {
    return pipe(
      Effect.tryPromise(async () => {
        try {
          const wallet = await importWallet(name, privateKey);
          console.log(chalk.green(`✓ Imported wallet: ${name}`));
          console.log(chalk.gray(`Address: ${wallet.address}`));
        } catch (error) {
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
        }
      })
    );
  }
).pipe(
  Command.withDescription(
    'Import an existing private key as a named wallet (not for browser-wallet keys — CIP-30 never exposes those)'
  )
);

/**
 * Command to show a wallet's address
 */
const addressCommand = Command.make('address', { name: nameArg }, ({ name }) => {
  return pipe(
    Effect.tryPromise(async () => {
      const wallet = await getWallet(name);
      if (!wallet) {
        console.error(chalk.red(`❌ Wallet '${name}' not found`));
        return;
      }
      console.log(wallet.address);
    })
  );
}).pipe(Command.withDescription("Print a wallet's address"));

/**
 * Command to show a wallet's spendable L2 balance
 */
const balanceCommand = Command.make(
  'balance',
  { name: nameArg, endpoint: endpointOption },
  ({ name, endpoint }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const wallet = await getWallet(name);
        if (!wallet) {
          console.error(chalk.red(`❌ Wallet '${name}' not found`));
          return;
        }

        try {
          const client = new MidgardNodeClient({ baseUrl: endpoint, enableLogs: false });
          const utxos = await client.getUtxos(wallet.address);
          const lovelace = utxos.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n);

          console.log(chalk.blue(`Balance for ${name} (${wallet.address}):`));
          console.log(`  ${(Number(lovelace) / 1_000_000).toFixed(6)} sBTC`);
          console.log(chalk.gray(`  (${lovelace} lovelace across ${utxos.length} UTxO(s))`));
        } catch (error) {
          console.error(
            chalk.red(`Failed to fetch balance: ${error instanceof Error ? error.message : error}`)
          );
          console.log(chalk.gray(`Is the node reachable at ${endpoint}?`));
        }
      })
    );
  }
).pipe(Command.withDescription("Check a wallet's spendable testnet sBTC balance"));

/**
 * Command to list all wallets
 */
const listCommand = Command.make('list', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const wallets = await listWallets();
      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets found'));
        console.log(chalk.gray('Create one with: midgard wallet create <name>'));
        return;
      }

      console.log(chalk.blue('Available wallets:'));
      for (const walletName of wallets) {
        const wallet = await getWallet(walletName);
        if (wallet) {
          console.log(` ${chalk.green('•')} ${walletName} — ${wallet.address}`);
        }
      }
    })
  );
}).pipe(Command.withDescription('List all wallets'));

/**
 * Command to remove a wallet
 */
const removeCommand = Command.make('remove', { name: nameArg }, ({ name }) => {
  return pipe(
    Effect.tryPromise(async () => {
      try {
        await removeWallet(name);
        console.log(chalk.green(`✓ Removed wallet: ${name}`));
      } catch (error) {
        console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
      }
    })
  );
}).pipe(Command.withDescription('Remove a wallet'));

/**
 * Main wallet command group
 */
export const walletCommand = Command.make('wallet')
  .pipe(Command.withDescription('Manage wallets for signing and sending L2 transactions'))
  .pipe(
    Command.withSubcommands([
      createCommand,
      importCommand,
      listCommand,
      addressCommand,
      balanceCommand,
      removeCommand,
    ])
  );
