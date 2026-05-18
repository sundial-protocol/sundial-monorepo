import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const {
  mockSubmitTransaction,
  mockIsAvailable,
  mockInspectGeneratedTransaction,
  generatedOneToOneTransactionsFromGenerator,
} = vi.hoisted(() => ({
  mockSubmitTransaction: vi.fn(),
  mockIsAvailable: vi.fn(),
  mockInspectGeneratedTransaction: vi.fn(),
  // Intentional: generator output fixture has two transactions so tests
  // assert scheduler counting against generator output, not batch-size limits.
  generatedOneToOneTransactionsFromGenerator: [
    { txId: 'tx_1', cborHex: 'cbor_1', type: 'one-to-one', description: '' },
    { txId: 'tx_2', cborHex: 'cbor_2', type: 'one-to-one', description: '' },
  ],
}));

vi.mock('../../src/lib/client/node-client', () => ({
  MidgardNodeClient: vi.fn().mockImplementation(() => ({
    submitTransaction: mockSubmitTransaction,
    isAvailable: mockIsAvailable,
  })),
}));

vi.mock('../../src/lib/generators/index.js', () => ({
  generateOneToOneTransactions: vi
    .fn()
    .mockResolvedValue(generatedOneToOneTransactionsFromGenerator),
  generateMultiOutputTransactions: vi.fn().mockResolvedValue([]),
}));

vi.mock('../../src/lib/scheduler/transaction-inspector.js', () => ({
  inspectGeneratedTransaction: mockInspectGeneratedTransaction,
}));

import {
  getGeneratorStatus,
  startGenerator,
  stopGenerator,
  waitForGeneratorStop,
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

describe('Scheduler submission outcome counting (H-31)', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockIsAvailable.mockResolvedValue(true);
    mockSubmitTransaction.mockResolvedValue({
      status: 'SUBMITTED',
      responseClass: 'submitted',
      txId: 'default_success',
      latencyMs: 5,
      attempts: 1,
      retriesUsed: 0,
    });
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
      .mockResolvedValueOnce({
        status: 'SUBMITTED',
        responseClass: 'submitted',
        txId: 'success_1',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      })
      .mockResolvedValueOnce({
        status: 'SUBMITTED',
        responseClass: 'submitted',
        txId: 'success_2',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      });

    await startGenerator(baseConfig);
    await waitForGeneratorStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(2);
    expect(status.transactionsFailed).toBe(0);
  });

  it('does not count ERROR responses as submitted', async () => {
    mockSubmitTransaction
      .mockResolvedValueOnce({
        status: 'SUBMITTED',
        responseClass: 'submitted',
        txId: 'success_1',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      })
      .mockResolvedValueOnce({
        status: 'ERROR',
        responseClass: 'http_error',
        error: 'node rejected tx',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      });

    await startGenerator(baseConfig);
    await waitForGeneratorStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(1);
    expect(status.transactionsFailed).toBe(1);
  });

  it('does not count all-ERROR batch as any submitted', async () => {
    mockSubmitTransaction
      .mockResolvedValueOnce({
        status: 'ERROR',
        responseClass: 'http_error',
        error: 'fail 1',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      })
      .mockResolvedValueOnce({
        status: 'ERROR',
        responseClass: 'http_error',
        error: 'fail 2',
        latencyMs: 5,
        attempts: 1,
        retriesUsed: 0,
      });

    await startGenerator(baseConfig);
    await waitForGeneratorStop();

    const status = getGeneratorStatus();
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(0);
    expect(status.transactionsFailed).toBe(2);
  });

  it('does not count NODE_UNAVAILABLE as submitted or failed', async () => {
    mockSubmitTransaction.mockResolvedValueOnce({
      status: 'NODE_UNAVAILABLE',
      responseClass: 'node_unavailable',
      message: 'node is down',
      latencyMs: 5,
      attempts: 0,
      retriesUsed: 0,
    });

    await startGenerator(baseConfig);
    await waitForGeneratorStop();

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
    await waitForGeneratorStop();

    const status = getGeneratorStatus();
    expect(mockSubmitTransaction).toHaveBeenCalledTimes(1);
    expect(mockSubmitTransaction).toHaveBeenCalledWith('cbor_2');
    expect(status.transactionsGenerated).toBe(2);
    expect(status.transactionsSubmitted).toBe(1);
    expect(status.transactionsFailed).toBe(1);
  });
});
