import { mkdtemp, readFile, rm, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const mocks = vi.hoisted(() => {
  return {
    generateOneToOne: vi.fn(),
    generateMultiOutput: vi.fn(),
    poolCreate: vi.fn(),
  };
});

vi.mock('../../src/generators/one-to-one.js', () => ({
  generateOneToOneTransactions: mocks.generateOneToOne,
}));

vi.mock('../../src/generators/multi-output.js', () => ({
  generateMultiOutputTransactions: mocks.generateMultiOutput,
}));

vi.mock('../../src/lucid-pool.js', () => ({
  LucidPool: {
    create: mocks.poolCreate,
  },
}));

import { generateCorpus } from '../../src/corpus-generator.js';

const baseInitialUtxO = {
  txHash: 'A'.repeat(64),
  outputIndex: 0,
  address: 'addr_test1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq',
  assets: { lovelace: 50_000_000n },
};

describe('generateCorpus', () => {
  let tempRoot: string;
  let poolAcquire: ReturnType<typeof vi.fn>;
  let poolRelease: ReturnType<typeof vi.fn>;

  beforeEach(async () => {
    tempRoot = await mkdtemp(join(tmpdir(), 'tx-builder-corpus-tests-'));
    vi.clearAllMocks();

    poolAcquire = vi.fn(async () => ({ id: 'pooled-lucid' }));
    poolRelease = vi.fn();
    mocks.poolCreate.mockResolvedValue({
      acquire: poolAcquire,
      release: poolRelease,
    });

    mocks.generateOneToOne.mockImplementation(async ({ initialUTxO }) => [
      {
        type: 'Midgard L2 User Transaction',
        description: 'one-to-one',
        cborHex: `cbor-${initialUTxO.txHash.slice(0, 8)}-${initialUTxO.outputIndex}`,
        txId: `tx-${initialUTxO.txHash.slice(0, 8)}-${initialUTxO.outputIndex}`,
      },
    ]);

    mocks.generateMultiOutput.mockImplementation(async ({ initialUTxO }) => [
      {
        type: 'Midgard L2 User Transaction',
        description: 'multi-output',
        cborHex: `multi-${initialUTxO.txHash.slice(0, 8)}-${initialUTxO.outputIndex}`,
        txId: `multi-tx-${initialUTxO.txHash.slice(0, 8)}-${initialUTxO.outputIndex}`,
      },
    ]);
  });

  afterEach(async () => {
    await rm(tempRoot, { recursive: true, force: true });
  });

  it('creates LucidPool for one-to-one corpus generation', async () => {
    const outputPath = join(tempRoot, 'one-to-one.ndjson');

    await generateCorpus({
      count: 5,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-one-to-one',
      outputPath,
    });

    expect(mocks.poolCreate).toHaveBeenCalledTimes(1);
    expect(mocks.generateOneToOne).toHaveBeenCalledTimes(5);
    expect(poolAcquire).toHaveBeenCalledTimes(5);
    expect(poolRelease).toHaveBeenCalledTimes(5);
  });

  it('does not create LucidPool for multi-output corpus generation', async () => {
    const outputPath = join(tempRoot, 'multi-output.ndjson');

    await generateCorpus({
      count: 4,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'multi-output',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-multi-output',
      outputPath,
    });

    expect(mocks.poolCreate).not.toHaveBeenCalled();
    expect(mocks.generateMultiOutput).toHaveBeenCalledTimes(4);
    expect(mocks.generateOneToOne).not.toHaveBeenCalled();
  });

  it('uses one-to-one path only when mixed ratio is 100', async () => {
    const outputPath = join(tempRoot, 'mixed-100.ndjson');

    await generateCorpus({
      count: 6,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'mixed',
      oneToOneRatio: 100,
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-mixed-100',
      outputPath,
    });

    expect(mocks.generateOneToOne).toHaveBeenCalledTimes(6);
    expect(mocks.generateMultiOutput).toHaveBeenCalledTimes(0);
  });

  it('uses multi-output path only when mixed ratio is 0', async () => {
    const outputPath = join(tempRoot, 'mixed-0.ndjson');

    await generateCorpus({
      count: 6,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'mixed',
      oneToOneRatio: 0,
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-mixed-0',
      outputPath,
    });

    expect(mocks.generateOneToOne).toHaveBeenCalledTimes(0);
    expect(mocks.generateMultiOutput).toHaveBeenCalledTimes(6);
  });

  it('writes one NDJSON line per generated transaction', async () => {
    const outputPath = join(tempRoot, 'lines.ndjson');

    await generateCorpus({
      count: 7,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-lines',
      outputPath,
    });

    const contents = await readFile(outputPath, 'utf8');
    const lines = contents.trim().split('\n');

    expect(lines).toHaveLength(7);
    lines.forEach((line) => {
      const parsed = JSON.parse(line);
      expect(parsed.type).toBe('Midgard L2 User Transaction');
    });
  });

  it('reports progress for every generated transaction', async () => {
    const outputPath = join(tempRoot, 'progress.ndjson');
    const progressSpy = vi.fn();

    await generateCorpus({
      count: 4,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'multi-output',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-progress',
      outputPath,
      onProgress: progressSpy,
    });

    expect(progressSpy).toHaveBeenCalledTimes(4);
    expect(progressSpy.mock.calls[0]).toEqual([1, 4]);
    expect(progressSpy.mock.calls[3]).toEqual([4, 4]);
  });

  it('creates parent output directories recursively', async () => {
    const outputPath = join(tempRoot, 'deep', 'nested', 'corpus.ndjson');

    await generateCorpus({
      count: 2,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'multi-output',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-dir-create',
      outputPath,
    });

    const fileStat = await stat(outputPath);
    expect(fileStat.isFile()).toBe(true);
  });

  it('is deterministic for same seed and config', async () => {
    const outputPathA = join(tempRoot, 'deterministic-a.ndjson');
    const outputPathB = join(tempRoot, 'deterministic-b.ndjson');

    await generateCorpus({
      count: 5,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'same-seed',
      outputPath: outputPathA,
    });

    const firstRun = await readFile(outputPathA, 'utf8');

    await generateCorpus({
      count: 5,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'same-seed',
      outputPath: outputPathB,
    });

    const secondRun = await readFile(outputPathB, 'utf8');

    expect(secondRun).toBe(firstRun);
  });

  it('changes generated output when seed changes', async () => {
    const outputPathA = join(tempRoot, 'seed-a.ndjson');
    const outputPathB = join(tempRoot, 'seed-b.ndjson');

    await generateCorpus({
      count: 5,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-A',
      outputPath: outputPathA,
    });

    await generateCorpus({
      count: 5,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'one-to-one',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-B',
      outputPath: outputPathB,
    });

    const first = await readFile(outputPathA, 'utf8');
    const second = await readFile(outputPathB, 'utf8');

    expect(second).not.toBe(first);
  });

  it('releases pooled lucid instance when one-to-one generation throws', async () => {
    const outputPath = join(tempRoot, 'one-to-one-error.ndjson');
    mocks.generateOneToOne.mockRejectedValueOnce(new Error('generation failed'));

    await expect(
      generateCorpus({
        count: 1,
        walletSeedOrPrivateKey: 'ed25519_sk1mock',
        transactionType: 'one-to-one',
        network: 'Custom',
        initialUTxO: baseInitialUtxO,
        seed: 'seed-error',
        outputPath,
      })
    ).rejects.toThrow('generation failed');

    expect(poolAcquire).toHaveBeenCalledTimes(1);
    expect(poolRelease).toHaveBeenCalledTimes(1);
  });

  it('supports count=0 by creating an empty output file', async () => {
    const outputPath = join(tempRoot, 'zero-count.ndjson');

    await generateCorpus({
      count: 0,
      walletSeedOrPrivateKey: 'ed25519_sk1mock',
      transactionType: 'multi-output',
      network: 'Custom',
      initialUTxO: baseInitialUtxO,
      seed: 'seed-zero',
      outputPath,
    });

    const contents = await readFile(outputPath, 'utf8');
    expect(contents).toBe('');
    expect(mocks.generateMultiOutput).not.toHaveBeenCalled();
  });
});
