import { mkdir, writeFile } from 'node:fs/promises';
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

  const transactions: SerializedMidgardTransaction[] = [];
  let generated = 0;

  const limiter = pLimit(CORPUS_GENERATION_CONCURRENCY);
  const masterRandom = createSeededRandom(seed);

  const workers = Array.from({ length: count }, (_, workerIndex) =>
    limiter(async () => {
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
        transactions.push(tx);
        generated += 1;
        onProgress?.(generated, count);
      }
    })
  );

  await Promise.all(workers);

  await mkdir(dirname(outputPath), { recursive: true });
  await writeFile(outputPath, JSON.stringify({ transactions }, null, 2));
}
