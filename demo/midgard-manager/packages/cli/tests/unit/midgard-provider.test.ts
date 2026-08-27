import { beforeEach, describe, expect, it, vi } from 'vitest';

const mocks = vi.hoisted(() => ({
  getUtxos: vi.fn(),
  submitTransaction: vi.fn(),
  fromCborHex: vi.fn(),
  hashTransaction: vi.fn(),
}));

vi.mock('@lucid-evolution/lucid', () => ({
  PROTOCOL_PARAMETERS_DEFAULT: {
    minFeeA: 1,
    minFeeB: 2,
    maxTxSize: 16384,
    coinsPerUtxoByte: 4310n,
  },
  CML: {
    Transaction: { from_cbor_hex: mocks.fromCborHex },
    hash_transaction: mocks.hashTransaction,
  },
}));

vi.mock('@midgard-manager/tx-generator', () => ({
  MidgardNodeClient: vi.fn().mockImplementation(() => ({
    getUtxos: mocks.getUtxos,
    submitTransaction: mocks.submitTransaction,
  })),
}));

import { MidgardNodeClient } from '@midgard-manager/tx-generator';

import { MidgardNodeProvider } from '../../src/services/midgard-provider.js';

beforeEach(() => {
  vi.clearAllMocks();
});

describe('MidgardNodeProvider', () => {
  it('constructs a MidgardNodeClient for the given endpoint with logging off', () => {
    new MidgardNodeProvider('http://node:3000');

    expect(MidgardNodeClient).toHaveBeenCalledWith({
      baseUrl: 'http://node:3000',
      enableLogs: false,
    });
  });

  it('overrides the fee parameters the node validates against', async () => {
    const params = await new MidgardNodeProvider('http://node:3000').getProtocolParameters();

    expect(params.minFeeA).toBe(44);
    expect(params.minFeeB).toBe(155381);
    // still spreads the Lucid defaults for everything else
    expect(params.maxTxSize).toBe(16384);
  });

  it('only supports getUtxos queries by address string', async () => {
    const provider = new MidgardNodeProvider('http://node:3000');

    await expect(provider.getUtxos({ type: 'Key', hash: 'abc' } as never)).rejects.toThrow(
      'only supports querying by address'
    );
  });

  it('delegates getUtxos by address to the client', async () => {
    const utxos = [
      { txHash: 'a', outputIndex: 0, assets: { lovelace: 1n }, address: 'addr_test1' },
    ];
    mocks.getUtxos.mockResolvedValue(utxos);
    const provider = new MidgardNodeProvider('http://node:3000');

    await expect(provider.getUtxos('addr_test1')).resolves.toBe(utxos);
    expect(mocks.getUtxos).toHaveBeenCalledWith('addr_test1');
  });

  describe('submitTx', () => {
    it('returns the hash computed from the transaction body on success', async () => {
      mocks.submitTransaction.mockResolvedValue({ status: 'SUBMITTED' });
      mocks.fromCborHex.mockReturnValue({ body: () => 'tx-body' });
      mocks.hashTransaction.mockReturnValue({ to_hex: () => 'deadbeefhash' });
      const provider = new MidgardNodeProvider('http://node:3000');

      await expect(provider.submitTx('cborhex')).resolves.toBe('deadbeefhash');
      expect(mocks.fromCborHex).toHaveBeenCalledWith('cborhex');
      expect(mocks.hashTransaction).toHaveBeenCalledWith('tx-body');
    });

    it('throws the node error when the submission is not accepted', async () => {
      mocks.submitTransaction.mockResolvedValue({ status: 'ERROR', error: 'phase-a rejection' });
      const provider = new MidgardNodeProvider('http://node:3000');

      await expect(provider.submitTx('cborhex')).rejects.toThrow('phase-a rejection');
    });

    it('falls back to message, then to the status, when no error field is present', async () => {
      mocks.submitTransaction.mockResolvedValueOnce({
        status: 'ERROR',
        message: 'queue saturated',
      });
      const provider = new MidgardNodeProvider('http://node:3000');
      await expect(provider.submitTx('cborhex')).rejects.toThrow('queue saturated');

      mocks.submitTransaction.mockResolvedValueOnce({ status: 'TIMEOUT' });
      await expect(provider.submitTx('cborhex')).rejects.toThrow('Submit failed: TIMEOUT');
    });
  });

  describe('unimplemented Provider methods', () => {
    // These throw synchronously rather than returning a rejected promise.
    const p = new MidgardNodeProvider('http://node:3000');

    it('throws for the operations the CLI does not need', () => {
      expect(() => p.getUtxosWithUnit()).toThrow('getUtxosWithUnit is not implemented');
      expect(() => p.getUtxoByUnit()).toThrow('getUtxoByUnit is not implemented');
      expect(() => p.getUtxosByOutRef()).toThrow('getUtxosByOutRef is not implemented');
      expect(() => p.getDelegation()).toThrow('getDelegation is not implemented');
      expect(() => p.getDatum()).toThrow('getDatum is not implemented');
      expect(() => p.evaluateTx()).toThrow('evaluateTx is not implemented');
      expect(() => p.awaitTx()).toThrow('midgard tx-lookup');
    });
  });
});
