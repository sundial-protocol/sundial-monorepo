import { Args, Command, Options } from '@effect/cli';
import { MidgardNodeClient } from '@midgard-manager/tx-generator';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';

export const txLookupCommand = Command.make(
  'tx-lookup',
  {
    hash: Args.text({ name: 'HASH' }).pipe(Args.withDescription('Transaction hash to look up')),
    endpoint: Options.text('endpoint')
      .pipe(Options.withDescription('Midgard node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),
  },
  ({ hash, endpoint }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const client = new MidgardNodeClient({ baseUrl: endpoint, enableLogs: false });
        const result = await Effect.runPromise(Effect.either(client.getTransactionStatus(hash)));

        if (result._tag === 'Left') {
          // The client throws on any non-2xx response, including a 404
          // "not found" — that's an expected outcome here, not a real error.
          if (result.left.error.includes('404')) {
            console.log(chalk.yellow(`Not found: ${hash}`));
            console.log(
              chalk.gray('It may still be queued for processing — try again in a few seconds.')
            );
            return;
          }
          console.error(chalk.red(`❌ Lookup failed for ${hash}: ${result.left.error}`));
          return;
        }

        console.log(chalk.green(`✓ Found on ${endpoint} (mempool or immutable storage)`));
        console.log(chalk.gray(`Transaction hash: ${hash}`));
      })
    );
  }
).pipe(Command.withDescription('Look up an L2 transaction by hash'));
