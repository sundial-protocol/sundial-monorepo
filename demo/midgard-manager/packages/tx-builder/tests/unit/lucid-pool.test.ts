import { beforeEach, describe, expect, it, vi } from 'vitest';

const mocks = vi.hoisted(() => {
  return {
    lucidFactory: vi.fn(),
    emulatorCtor: vi.fn(),
  };
});

vi.mock('@lucid-evolution/lucid', () => {
  class MockEmulator {
    public ledger: Record<string, unknown> = {};

    constructor(accounts: unknown[]) {
      mocks.emulatorCtor(accounts);
    }
  }

  return {
    Emulator: MockEmulator,
    Lucid: mocks.lucidFactory,
    PROTOCOL_PARAMETERS_DEFAULT: {
      minFeeA: 44,
      minFeeB: 155381,
      priceMem: 0.0577,
      priceStep: 0.0000721,
      coinsPerUtxoByte: 4310n,
    },
  };
});

import { LucidPool } from '../../src/lucid-pool.js';

describe('LucidPool', () => {
  let lucidId = 0;

  beforeEach(() => {
    lucidId = 0;
    vi.clearAllMocks();
    mocks.lucidFactory.mockImplementation(async () => ({ id: `lucid-${++lucidId}` }));
  });

  it('creates the requested number of pre-initialized instances', async () => {
    await LucidPool.create(
      3,
      'ed25519_sk1mock',
      'addr_test1main',
      { lovelace: 5_000_000n },
      'Custom'
    );

    expect(mocks.lucidFactory).toHaveBeenCalledTimes(3);
    expect(mocks.emulatorCtor).toHaveBeenCalledTimes(3);
  });

  it('initializes Emulator account with provided wallet, address and assets', async () => {
    await LucidPool.create(
      1,
      'ed25519_sk1key',
      'addr_test1abc',
      { lovelace: 8_000_000n },
      'Preview'
    );

    const accounts = mocks.emulatorCtor.mock.calls[0]?.[0] as Array<Record<string, unknown>>;
    expect(accounts).toHaveLength(1);
    expect(accounts[0]).toMatchObject({
      seedPhrase: '',
      address: 'addr_test1abc',
      privateKey: 'ed25519_sk1key',
      assets: { lovelace: 8_000_000n },
    });
  });

  it('passes zero-fee protocol parameters to Lucid initializer', async () => {
    await LucidPool.create(1, 'ed25519_sk1k', 'addr_test1def', { lovelace: 1_000_000n }, 'Testnet');

    const options = mocks.lucidFactory.mock.calls[0]?.[2] as {
      presetProtocolParameters: Record<string, unknown>;
    };

    expect(options.presetProtocolParameters.minFeeA).toBe(0);
    expect(options.presetProtocolParameters.minFeeB).toBe(0);
    expect(options.presetProtocolParameters.priceMem).toBe(0);
    expect(options.presetProtocolParameters.priceStep).toBe(0);
    expect(options.presetProtocolParameters.coinsPerUtxoByte).toBe(0n);
  });

  it('acquire returns an available instance immediately', async () => {
    const pool = await LucidPool.create(
      1,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );

    const instance = await pool.acquire();

    expect(instance).toEqual({ id: 'lucid-1' });
  });

  it('acquire waits when no instances are available and resolves after release', async () => {
    const pool = await LucidPool.create(
      1,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );
    const first = await pool.acquire();

    const pendingAcquire = pool.acquire();
    const resolution = pendingAcquire.then((value) => value.id);

    pool.release(first);

    await expect(resolution).resolves.toBe('lucid-1');
  });

  it('release pushes instance back when no waiters exist', async () => {
    const pool = await LucidPool.create(
      1,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );
    const first = await pool.acquire();

    pool.release(first);
    const reacquired = await pool.acquire();

    expect(reacquired).toBe(first);
  });

  it('serves waiters in FIFO order', async () => {
    const pool = await LucidPool.create(
      1,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );

    const initial = await pool.acquire();
    const waiterOne = pool.acquire();
    const waiterTwo = pool.acquire();

    pool.release(initial);
    const firstResolved = await waiterOne;

    pool.release({ id: 'manual-instance' });
    const secondResolved = await waiterTwo;

    expect(firstResolved).toEqual({ id: 'lucid-1' });
    expect(secondResolved).toEqual({ id: 'manual-instance' });
  });

  it('supports empty pools created with size 0', async () => {
    const pool = await LucidPool.create(
      0,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );

    const pending = pool.acquire();
    pool.release({ id: 'injected' });

    await expect(pending).resolves.toEqual({ id: 'injected' });
  });

  it('propagates initialization failures', async () => {
    mocks.lucidFactory.mockRejectedValueOnce(new Error('lucid init failed'));

    await expect(
      LucidPool.create(1, 'ed25519_sk1k', 'addr', { lovelace: 1_000_000n }, 'Mainnet')
    ).rejects.toThrow('lucid init failed');
  });

  it('retains all created instances for repeated acquire/release cycles', async () => {
    const pool = await LucidPool.create(
      2,
      'ed25519_sk1k',
      'addr',
      { lovelace: 1_000_000n },
      'Mainnet'
    );

    const first = await pool.acquire();
    const second = await pool.acquire();

    pool.release(first);
    pool.release(second);

    const a = await pool.acquire();
    const b = await pool.acquire();

    expect(new Set([a.id, b.id])).toEqual(new Set(['lucid-1', 'lucid-2']));
  });
});
