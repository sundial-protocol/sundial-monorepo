import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const { mockSubmitTransaction, mockIsAvailable, mockOneToOne, mockMultiOutput } = vi.hoisted(
  () => ({
    mockSubmitTransaction: vi.fn(),
    mockIsAvailable: vi.fn(),
    mockOneToOne: vi.fn(),
    mockMultiOutput: vi.fn(),
  })
);

vi.mock('../../src/lib/client/node-client', () => ({
  MidgardNodeClient: vi.fn().mockImplementation(() => ({
    submitTransaction: mockSubmitTransaction,
    isAvailable: mockIsAvailable,
  })),
}));

vi.mock('../../src/lib/generators/index.js', () => ({
  generateOneToOneTransactions: mockOneToOne,
  generateMultiOutputTransactions: mockMultiOutput,
}));

import {
  getGeneratorStatus,
  startGenerator,
  stopGenerator,
} from '../../src/lib/scheduler/scheduler';

const waitForStop = async (maxMs = 2000) => {
  const deadline = Date.now() + maxMs;
  while (getGeneratorStatus().running && Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};

const getGenerationPlan = () => {
  const oneToOneInputs = mockOneToOne.mock.calls.map((call) => {
    const config = call[0];
    return `${config.initialUTxO.txHash}:${config.initialUTxO.outputIndex}`;
  });
  const multiOutputInputs = mockMultiOutput.mock.calls.map((call) => {
    const config = call[0];
    return `${config.initialUTxO.txHash}:${config.initialUTxO.outputIndex}`;
  });

  return {
    oneToOneInputs,
    multiOutputInputs,
  };
};

describe('Deterministic generation and replay', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockIsAvailable.mockResolvedValue(true);
    mockSubmitTransaction.mockResolvedValue({ txId: 'submitted' });

    mockOneToOne.mockImplementation(async (config) => [
      {
        txId: `one-${config.initialUTxO.txHash}`,
        cborHex: `one-${config.initialUTxO.txHash}`,
        type: 'Midgard L2 User Transaction',
        description: 'deterministic one-to-one',
      },
    ]);

    mockMultiOutput.mockImplementation(async (config) => [
      {
        txId: `multi-${config.initialUTxO.txHash}`,
        cborHex: `multi-${config.initialUTxO.txHash}`,
        type: 'Midgard L2 User Transaction',
        description: 'deterministic multi-output',
      },
    ]);
  });

  afterEach(async () => {
    await stopGenerator();
  });

  it('uses the same generation plan for repeated runs with the same seed', async () => {
    const config = {
      walletSeedOrPrivateKey: 'seeded_test_key',
      nodeEndpoint: 'http://localhost:3000',
      transactionType: 'mixed' as const,
      oneToOneRatio: 40,
      batchSize: 6,
      interval: 0,
      concurrency: 2,
      autoStopAfterBatch: true,
      generationSeed: 'm42-seed-replayable',
    };

    await startGenerator(config);
    await waitForStop();
    const firstPlan = getGenerationPlan();

    mockOneToOne.mockClear();
    mockMultiOutput.mockClear();
    mockSubmitTransaction.mockClear();

    await startGenerator(config);
    await waitForStop();
    const secondPlan = getGenerationPlan();

    expect(secondPlan).toEqual(firstPlan);
    expect(secondPlan.oneToOneInputs.length + secondPlan.multiOutputInputs.length).toBe(
      config.batchSize
    );
  });

  it('replays a corpus without invoking live generators', async () => {
    const tempDir = await mkdtemp(join(tmpdir(), 'm42-corpus-'));
    const corpusPath = join(tempDir, 'corpus.json');
    await writeFile(
      corpusPath,
      JSON.stringify(
        [
          {
            txId: 'replay-1',
            cborHex: 'cbor-replay-1',
            type: 'Midgard L2 User Transaction',
            description: 'replay tx 1',
          },
          {
            txId: 'replay-2',
            cborHex: 'cbor-replay-2',
            type: 'Midgard L2 User Transaction',
            description: 'replay tx 2',
          },
        ],
        null,
        2
      )
    );

    try {
      await startGenerator({
        walletSeedOrPrivateKey: 'seeded_test_key',
        nodeEndpoint: 'http://localhost:3000',
        transactionType: 'mixed',
        batchSize: 6,
        interval: 0,
        concurrency: 1,
        autoStopAfterBatch: true,
        replayCorpusPath: corpusPath,
      });
      await waitForStop();
      const status = getGeneratorStatus();

      expect(mockOneToOne).not.toHaveBeenCalled();
      expect(mockMultiOutput).not.toHaveBeenCalled();
      expect(mockSubmitTransaction).not.toHaveBeenCalled();
      expect(status.transactionsGenerated).toBe(2);
      expect(status.transactionsSubmitted).toBe(0);
      expect(status.transactionsFailed).toBe(2);
    } finally {
      await rm(tempDir, { recursive: true, force: true });
    }
  });
});
