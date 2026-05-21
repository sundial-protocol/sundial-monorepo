import { createWriteStream } from 'node:fs';
import { mkdir } from 'node:fs/promises';
import { dirname } from 'node:path';

import { Network, UTxO } from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import { generateMultiOutputTransactions } from './generators/multi-output.js';
import { generateOneToOneTransactions } from './generators/one-to-one.js';
import { LucidPool } from './lucid-pool.js';
import { createSeededRandom, randomHex, randomInt } from './random.js';
import { SerializedMidgardTransaction, TransactionType } from './types.js';

// Lucid WASM transaction signing is synchronous from Node.js's perspective.
// More than this many concurrent workers saturates the event loop and starves
// AbortController timers during submission. Keep generation pre-separated from
// submission so neither phase blocks the other.
const CORPUS_GENERATION_CONCURRENCY = 16;
const OUTPUT_INDEX_UPPER_EXCLUSIVE = 1001;

export interface CorpusGenerateConfig {
  count: number;
  walletSeedOrPrivateKey: string;
  transactionType: TransactionType;
  oneToOneRatio?: number;
  network: Network;
  initialUTxO: UTxO;
  seed: string;
  outputPath: string;
  onProgress?: (generated: number, total: number) => void;
}

// Each worker gets its own isolated RNG stream so the corpus is fully
// deterministic regardless of event-loop scheduling order.
function workerSeed(masterSeed: string, workerIndex: number): string {
  return `${masterSeed}:gen:${workerIndex}`;
}

function workerUTxO(baseUTxO: UTxO, masterRandom: () => number): UTxO {
  return {
    ...baseUTxO,
    txHash: randomHex(masterRandom, 64).toUpperCase(),
    outputIndex: randomInt(masterRandom, OUTPUT_INDEX_UPPER_EXCLUSIVE),
  };
}

export async function generateCorpus(config: CorpusGenerateConfig): Promise<void> {
  const {
    count,
    walletSeedOrPrivateKey,
    transactionType,
    oneToOneRatio = 70,
    network,
    initialUTxO,
    seed,
    outputPath,
    onProgress,
  } = config;

  const needsPool = transactionType === 'one-to-one' || transactionType === 'mixed';
  const lucidPool = needsPool
    ? await LucidPool.create(
        CORPUS_GENERATION_CONCURRENCY,
        walletSeedOrPrivateKey,
        initialUTxO.address,
        initialUTxO.assets,
        network
      )
    : null;

  await mkdir(dirname(outputPath), { recursive: true });

  const stream = createWriteStream(outputPath, { encoding: 'utf8' });
  const limiter = pLimit(CORPUS_GENERATION_CONCURRENCY);
  const masterRandom = createSeededRandom(seed);
  let generated = 0;
  let reservedTasks = 0;
  let writeChain: Promise<void> = Promise.resolve();

  const reserveTask = (): number | null => {
    if (reservedTasks >= count) {
      return null;
    }
    const taskIndex = reservedTasks;
    reservedTasks += 1;
    return taskIndex;
  };

  // Serialize writes so only one drain listener can exist at a time.
  // This avoids MaxListenersExceededWarning when many generation workers
  // produce transactions concurrently.
  const writeLine = async (tx: SerializedMidgardTransaction): Promise<void> => {
    const line = JSON.stringify(tx) + '\n';
    const nextWrite = writeChain.then(async () => {
      if (!stream.write(line)) {
        await new Promise<void>((resolve) => stream.once('drain', resolve));
      }
    });
    writeChain = nextWrite;
    await nextWrite;
  };

  const runWorker = async (): Promise<void> => {
    while (true) {
      const workerIndex = reserveTask();
      if (workerIndex === null) {
        return;
      }

      await limiter(async () => {
        const wRandom = createSeededRandom(workerSeed(seed, workerIndex));
        const wUTxO = workerUTxO(initialUTxO, masterRandom);
        const useOneToOne =
          transactionType === 'one-to-one' ||
          (transactionType === 'mixed' && wRandom() * 100 < oneToOneRatio);

        let txs: SerializedMidgardTransaction[];
        if (useOneToOne) {
          const pooledLucid = lucidPool !== null ? await lucidPool.acquire() : undefined;
          try {
            txs = await generateOneToOneTransactions({
              network,
              initialUTxO: wUTxO,
              txsCount: 1,
              walletSeedOrPrivateKey,
              random: wRandom,
              deterministicStartMs: 0,
              lucid: pooledLucid,
            });
          } finally {
            if (pooledLucid !== undefined && lucidPool !== null) {
              lucidPool.release(pooledLucid);
            }
          }
        } else {
          txs = await generateMultiOutputTransactions({
            network,
            initialUTxO: wUTxO,
            utxosCount: 20,
            finalUtxosCount: 1,
            walletSeedOrPrivateKey,
            random: wRandom,
          });
        }

        for (const tx of txs) {
          if (generated >= count) break;
          await writeLine(tx);
          generated += 1;
          onProgress?.(generated, count);
        }
      });
    }
  };

  const workers = Array.from({ length: CORPUS_GENERATION_CONCURRENCY }, () => runWorker());
  await Promise.all(workers);

  await new Promise<void>((resolve, reject) => {
    stream.on('error', reject);
    stream.end(() => resolve());
  });
}
