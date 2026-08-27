import { Command, Options } from '@effect/cli';
import { Lucid } from '@lucid-evolution/lucid';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';

import { getWallet } from '../config/wallets.js';
import { MidgardNodeProvider } from '../services/midgard-provider.js';

// sBTC displays with the same 6-decimal convention as ADA/lovelace (see
// internal-docs/testnet-user-guide.md: 1,000,000 lovelace = 1 sBTC).
export const parseSbtcToLovelace = (amount: string): bigint => {
  const [whole, fraction = ''] = amount.split('.');
  if (!/^\d+$/.test(whole) || !/^\d*$/.test(fraction) || fraction.length > 6) {
    throw new Error(`Invalid amount: ${amount} (expected a decimal like 1.5, up to 6 decimals)`);
  }
  const paddedFraction = fraction.padEnd(6, '0');
  return BigInt(whole) * 1_000_000n + BigInt(paddedFraction || '0');
};

export const sendCommand = Command.make(
  'send',
  {
    from: Options.text('from').pipe(Options.withDescription('Wallet name to send from')),
    to: Options.text('to').pipe(Options.withDescription('Recipient addr_test1... address')),
    amount: Options.text('amount').pipe(
      Options.withDescription('Amount in sBTC to send (e.g. 1.5)')
    ),
    endpoint: Options.text('endpoint')
      .pipe(Options.withDescription('Midgard node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),
  },
  ({ from, to, amount, endpoint }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const wallet = await getWallet(from);
        if (!wallet) {
          console.error(chalk.red(`❌ Wallet '${from}' not found`));
          console.log(chalk.gray(`Create it with: midgard wallet create ${from}`));
          return;
        }

        let lovelace: bigint;
        try {
          lovelace = parseSbtcToLovelace(amount);
        } catch (error) {
          console.error(chalk.red(error instanceof Error ? error.message : 'Invalid amount'));
          return;
        }

        console.log(
          chalk.gray(`Sending ${amount} sBTC from ${from} (${wallet.address}) to ${to}...`)
        );

        try {
          const lucid = await Lucid(new MidgardNodeProvider(endpoint), 'Preprod');
          lucid.selectWallet.fromPrivateKey(wallet.privateKey);

          const tx = await lucid.newTx().pay.ToAddress(to, { lovelace }).complete();
          const signed = await tx.sign.withPrivateKey(wallet.privateKey).complete();
          const txHash = await signed.submit();

          console.log(chalk.green(`✓ Sent ${amount} sBTC to ${to}`));
          console.log(chalk.gray(`Transaction hash: ${txHash}`));
          console.log(chalk.gray(`Check it with: midgard tx-lookup ${txHash}`));
        } catch (error) {
          console.error(
            chalk.red(`Failed to send: ${error instanceof Error ? error.message : error}`)
          );
          console.log(chalk.gray('Is the node running? Check with: midgard node node-status'));
        }
      })
    );
  }
).pipe(
  Command.withDescription(
    'Send testnet sBTC to another address (signs with a locally stored private key — not a browser wallet)'
  )
);
