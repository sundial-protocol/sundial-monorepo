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
  createDeterministicTaskPlan,
  getGeneratorStatus,
  startGenerator,
  stopGenerator,
  waitForGeneratorStop,
} from '../../src/lib/scheduler/scheduler';

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

  it('builds the same deterministic task plan for repeated runs with the same seed', async () => {
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

    const firstPlan = createDeterministicTaskPlan({
      initialUTxO: {
        txHash: '0'.repeat(64),
        outputIndex: 0,
        assets: { lovelace: 10_000_000_000n },
        address: '',
        datum: null,
        datumHash: null,
        scriptRef: null,
      },
      batchSize: config.batchSize,
      transactionType: config.transactionType,
      oneToOneRatio: config.oneToOneRatio,
      generationSeed: config.generationSeed,
    });
    const secondPlan = createDeterministicTaskPlan({
      initialUTxO: {
        txHash: '0'.repeat(64),
        outputIndex: 0,
        assets: { lovelace: 10_000_000_000n },
        address: '',
        datum: null,
        datumHash: null,
        scriptRef: null,
      },
      batchSize: config.batchSize,
      transactionType: config.transactionType,
      oneToOneRatio: config.oneToOneRatio,
      generationSeed: config.generationSeed,
    });

    expect(secondPlan).toEqual(firstPlan);
    expect(secondPlan.taskPlans).toHaveLength(config.batchSize);
    expect(
      secondPlan.taskPlans.map(
        (taskPlan) => `${taskPlan.initialUTxO.txHash}:${taskPlan.initialUTxO.outputIndex}`
      )
    ).toMatchInlineSnapshot(`
      [
        "316B90D6B600E91B5D572523BD638AD4112AA6CA4AEC53207EB829B8F935A89E:575",
        "9A2A5244B06778AF5DB946778ADF0F493CA2817F473029CA2F31D4F2EC01EF2E:146",
        "CAA4DE8FDF0991C0EF0EBB2083C1EDFFCDBD06348A686AA4E54A9E956158B2A9:923",
        "838AC823157EBC9344BC73CC132C650FB375DDAD4CAA3C4A95BBED1E15EC49B6:783",
        "1BA465CF47DB4CB1C3FACDE687DB2906169859B689086624668CFEE14527A54D:292",
        "2CD3F635AB69BD7362EE472220C9EDCF6789FC34AB5266CF5E2ABE01391BF932:986",
      ]
    `);
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
      await waitForGeneratorStop();
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
