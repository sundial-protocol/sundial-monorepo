import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const { mockSubmitTransaction, mockIsAvailable, mockInspectGeneratedTransaction } = vi.hoisted(
  () => ({
    mockSubmitTransaction: vi.fn(),
    mockIsAvailable: vi.fn(),
    mockInspectGeneratedTransaction: vi.fn(),
  })
);

vi.mock('../../src/lib/client/node-client', () => ({
  MidgardNodeClient: vi.fn().mockImplementation(() => ({
    submitTransaction: mockSubmitTransaction,
    isAvailable: mockIsAvailable,
  })),
}));

vi.mock('../../src/lib/generators/index.js', () => ({
  generateOneToOneTransactions: vi.fn().mockResolvedValue([
    { txId: 'tx_1', cborHex: 'cbor_1', type: 'one-to-one', description: '' },
    { txId: 'tx_2', cborHex: 'cbor_2', type: 'one-to-one', description: '' },
  ]),
  generateMultiOutputTransactions: vi.fn().mockResolvedValue([]),
}));

vi.mock('../../src/lib/scheduler/transaction-inspector.js', () => ({
  inspectGeneratedTransaction: mockInspectGeneratedTransaction,
}));

import {
  getGeneratorStatus,
  startGenerator,
  stopGenerator,
} from '../../src/lib/scheduler/scheduler';

const baseConfig = {
  walletSeedOrPrivateKey: 'test_seed_phrase_for_scheduler_tests',
  nodeEndpoint: 'http://localhost:3000',
  transactionType: 'one-to-one' as const,
  batchSize: 1,
  interval: 0,
  concurrency: 1,
  autoStopAfterBatch: true,
  outputDir: undefined,
};

const waitForStop = async (maxMs = 2000) => {
  const deadline = Date.now() + maxMs;
  while (getGeneratorStatus().running && Date.now() < deadline) {
    await new Promise((r) => setTimeout(r, 10));
  }
};

describe('Scheduler submission outcome counting (H-31)', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockIsAvailable.mockResolvedValue(true);
    mockSubmitTransaction.mockResolvedValue({ txId: 'default_success' });
    mockInspectGeneratedTransaction.mockImplementation((tx) => ({
      transaction: tx,
      inspection: {
        computedTxIdHex: tx.txId,
        cborByteSize: 1,
        midgardByteSize: 1,
        validation: { status: 'accepted' },
        shape: null,
      },
    }));
  });

  afterEach(async () => {
    await stopGenerator();
  });

  it('counts all successful submissions as submitted and none as failed', async () => {
    mockSubmitTransaction
      .mockResolvedValueOnce({ txId: 'success_1' })
      .mockResolvedValueOnce({ txId: 'success_2' });

    await startGenerator(baseConfig);
    await waitForStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(2);
    expect(status.transactionsFailed).toBe(0);
  });

  it('does not count ERROR responses as submitted', async () => {
    mockSubmitTransaction
      .mockResolvedValueOnce({ txId: 'success_1' })
      .mockResolvedValueOnce({ status: 'ERROR', error: 'node rejected tx' });

    await startGenerator(baseConfig);
    await waitForStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(1);
    expect(status.transactionsFailed).toBe(1);
  });

  it('does not count all-ERROR batch as any submitted', async () => {
    mockSubmitTransaction
      .mockResolvedValueOnce({ status: 'ERROR', error: 'fail 1' })
      .mockResolvedValueOnce({ status: 'ERROR', error: 'fail 2' });

    await startGenerator(baseConfig);
    await waitForStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(0);
    expect(status.transactionsFailed).toBe(2);
  });

  it('does not count NODE_UNAVAILABLE as submitted or failed', async () => {
    mockSubmitTransaction.mockResolvedValueOnce({
      status: 'NODE_UNAVAILABLE',
      message: 'node is down',
    });

    await startGenerator(baseConfig);
    await waitForStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(0);
    expect(status.transactionsFailed).toBe(0);
  });

  it('does not submit transactions rejected by shared inspection', async () => {
    mockInspectGeneratedTransaction
      .mockImplementationOnce((tx) => ({
        transaction: tx,
        inspection: {
          computedTxIdHex: tx.txId,
          cborByteSize: 1,
          midgardByteSize: null,
          validation: {
            status: 'rejected',
            rejectCode: 'E_TX_HASH_MISMATCH',
            detail: 'provided tx id does not match cbor hash',
          },
          shape: null,
        },
      }))
      .mockImplementationOnce((tx) => ({
        transaction: tx,
        inspection: {
          computedTxIdHex: tx.txId,
          cborByteSize: 1,
          midgardByteSize: 1,
          validation: { status: 'accepted' },
          shape: null,
        },
      }));

    await startGenerator(baseConfig);
    await waitForStop();

    const status = getGeneratorStatus();
    expect(mockSubmitTransaction).toHaveBeenCalledTimes(1);
    expect(mockSubmitTransaction).toHaveBeenCalledWith('cbor_2');
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(1);
    expect(status.transactionsFailed).toBe(1);
  });
});
