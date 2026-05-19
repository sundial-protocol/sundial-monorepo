import { EventEmitter } from 'node:events';

import { beforeEach, describe, expect, it, vi } from 'vitest';

const mocks = vi.hoisted(() => {
  return {
    walletFromSeed: vi.fn(),
    fromNormalBytes: vi.fn(),
    fromBech32: vi.fn(),
    generateEmulatorAccountFromPrivateKey: vi.fn(),
    credentialNewPubKey: vi.fn(),
    enterpriseAddressNew: vi.fn(),
  };
});

vi.mock('@lucid-evolution/lucid', () => ({
  CML: {
    PrivateKey: {
      from_normal_bytes: mocks.fromNormalBytes,
      from_bech32: mocks.fromBech32,
    },
    Credential: {
      new_pub_key: mocks.credentialNewPubKey,
    },
    EnterpriseAddress: {
      new: mocks.enterpriseAddressNew,
    },
  },
  generateEmulatorAccountFromPrivateKey: mocks.generateEmulatorAccountFromPrivateKey,
  walletFromSeed: mocks.walletFromSeed,
}));

import {
  generateTestWallet,
  getAddressFromPrivateKey,
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  waitWritable,
} from '../../src/utils.js';

const createPrivateKeyStub = (bech32: string, publicKeyHashHex = 'public_key_hash_hex') => ({
  to_bech32: () => bech32,
  to_public: () => ({
    hash: () => ({
      to_hex: () => publicKeyHashHex,
    }),
  }),
});

describe('utils helpers', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('parseUnknownKeytoBech32PrivateKey', () => {
    it('throws when unknown key is not a string', () => {
      expect(() => parseUnknownKeytoBech32PrivateKey(12345)).toThrow(
        'Expected a string value for the private key'
      );
    });

    it('converts seed phrase input using walletFromSeed', () => {
      mocks.walletFromSeed.mockReturnValue({ paymentKey: 'seed-derived-key' });

      const result = parseUnknownKeytoBech32PrivateKey('  one two three four  ');

      expect(result).toBe('seed-derived-key');
      expect(mocks.walletFromSeed).toHaveBeenCalledWith('one two three four', {
        accountIndex: 0,
        addressType: 'Base',
      });
    });

    it('converts hex key bytes through CML.from_normal_bytes', () => {
      mocks.fromNormalBytes.mockReturnValue(createPrivateKeyStub('bech32-from-normal-bytes'));

      const result = parseUnknownKeytoBech32PrivateKey('ed25519_skdeadbeef');

      expect(result).toBe('bech32-from-normal-bytes');
      expect(mocks.fromNormalBytes).toHaveBeenCalledTimes(1);
      const firstArg = mocks.fromNormalBytes.mock.calls[0]?.[0];
      expect(Buffer.isBuffer(firstArg)).toBe(true);
      expect(firstArg).toEqual(Buffer.from('ed25519_skdeadbeef'.substring(4), 'hex'));
    });

    it('falls back to CML.from_bech32 when normal-bytes parsing throws', () => {
      mocks.fromNormalBytes.mockImplementation(() => {
        throw new Error('invalid normal bytes');
      });
      mocks.fromBech32.mockReturnValue(createPrivateKeyStub('bech32-fallback'));

      const result = parseUnknownKeytoBech32PrivateKey('ed25519_skbech32_input_key');

      expect(result).toBe('bech32-fallback');
      expect(mocks.fromBech32).toHaveBeenCalledWith('ed25519_skbech32_input_key');
    });

    it('trims key in fallback bech32 parsing path', () => {
      mocks.fromNormalBytes.mockImplementation(() => {
        throw new Error('invalid normal bytes');
      });
      mocks.fromBech32.mockReturnValue(createPrivateKeyStub('trimmed-key'));

      parseUnknownKeytoBech32PrivateKey('   ed25519_sktrimmed   ');

      expect(mocks.fromBech32).toHaveBeenCalledWith('ed25519_sktrimmed');
    });

    it('propagates error when fallback bech32 parsing fails', () => {
      mocks.fromNormalBytes.mockImplementation(() => {
        throw new Error('invalid normal bytes');
      });
      mocks.fromBech32.mockImplementation(() => {
        throw new Error('invalid bech32');
      });

      expect(() => parseUnknownKeytoBech32PrivateKey('ed25519_skbad')).toThrow('invalid bech32');
    });
  });

  describe('getPublicKeyHashFromPrivateKey', () => {
    it('returns public key hash in hex', () => {
      mocks.fromBech32.mockReturnValue(createPrivateKeyStub('bech32-key', 'abcd1234'));

      const result = getPublicKeyHashFromPrivateKey('ed25519_sk1mock');

      expect(result).toBe('abcd1234');
      expect(mocks.fromBech32).toHaveBeenCalledWith('ed25519_sk1mock');
    });
  });

  describe('waitWritable', () => {
    it('resolves immediately when stream is writable', async () => {
      const writable = {
        writable: true,
        once: vi.fn(),
      } as NodeJS.WritableStream;

      await expect(waitWritable(writable)).resolves.toBeUndefined();
      expect(writable.once).not.toHaveBeenCalled();
    });

    it('waits for drain event when stream is not writable', async () => {
      const emitter = new EventEmitter();
      const writable = {
        writable: false,
        once: emitter.once.bind(emitter),
      } as NodeJS.WritableStream;

      const promise = waitWritable(writable);
      setTimeout(() => emitter.emit('drain'), 0);

      await expect(promise).resolves.toBeUndefined();
    });
  });

  describe('generateTestWallet', () => {
    it('returns the generated emulator account credentials', async () => {
      mocks.generateEmulatorAccountFromPrivateKey.mockResolvedValue({
        privateKey: 'test-private-key',
        address: 'addr_test1mock',
      });

      await expect(generateTestWallet()).resolves.toEqual({
        privateKey: 'test-private-key',
        address: 'addr_test1mock',
      });
    });

    it('calls emulator account generator with empty options', async () => {
      mocks.generateEmulatorAccountFromPrivateKey.mockResolvedValue({
        privateKey: 'pk',
        address: 'addr',
      });

      await generateTestWallet();

      expect(mocks.generateEmulatorAccountFromPrivateKey).toHaveBeenCalledWith({});
    });
  });

  describe('getAddressFromPrivateKey', () => {
    it('builds enterprise address for networkId=0', () => {
      const credential = { kind: 'credential' };
      const pubKeyHash = { kind: 'pub-key-hash' };
      mocks.fromBech32.mockReturnValue({
        to_public: () => ({ hash: () => pubKeyHash }),
      });
      mocks.credentialNewPubKey.mockReturnValue(credential);
      mocks.enterpriseAddressNew.mockReturnValue({
        to_address: () => ({
          to_bech32: () => 'addr_test_network_0',
        }),
      });

      const result = getAddressFromPrivateKey('ed25519_sk1abc', 0);

      expect(result).toBe('addr_test_network_0');
      expect(mocks.credentialNewPubKey).toHaveBeenCalledWith(pubKeyHash);
      expect(mocks.enterpriseAddressNew).toHaveBeenCalledWith(0, credential);
    });

    it('builds enterprise address for networkId=1', () => {
      const credential = { kind: 'credential-mainnet' };
      const pubKeyHash = { kind: 'pub-key-hash-mainnet' };
      mocks.fromBech32.mockReturnValue({
        to_public: () => ({ hash: () => pubKeyHash }),
      });
      mocks.credentialNewPubKey.mockReturnValue(credential);
      mocks.enterpriseAddressNew.mockReturnValue({
        to_address: () => ({
          to_bech32: () => 'addr_main_network_1',
        }),
      });

      const result = getAddressFromPrivateKey('ed25519_sk1xyz', 1);

      expect(result).toBe('addr_main_network_1');
      expect(mocks.enterpriseAddressNew).toHaveBeenCalledWith(1, credential);
    });
  });
});
