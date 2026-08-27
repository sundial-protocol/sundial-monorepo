import { beforeEach, describe, expect, it, vi } from 'vitest';

const mocks = vi.hoisted(() => ({
  mkdir: vi.fn(),
  readFile: vi.fn(),
  writeFile: vi.fn(),
  generateEmulatorAccountFromPrivateKey: vi.fn(),
  privateKeyFromBech32: vi.fn(),
}));

vi.mock('fs/promises', () => ({
  mkdir: mocks.mkdir,
  readFile: mocks.readFile,
  writeFile: mocks.writeFile,
}));

vi.mock('@lucid-evolution/lucid', () => ({
  generateEmulatorAccountFromPrivateKey: mocks.generateEmulatorAccountFromPrivateKey,
  CML: {
    PrivateKey: { from_bech32: mocks.privateKeyFromBech32 },
    Credential: { new_pub_key: vi.fn((hash: unknown) => ({ credential: hash })) },
    EnterpriseAddress: {
      new: vi.fn((networkId: number) => ({
        to_address: () => ({ to_bech32: () => `addr_test_network_${networkId}` }),
      })),
    },
  },
}));

import {
  addWallet,
  createWallet,
  getWallet,
  importWallet,
  listWallets,
  loadWallets,
  removeWallet,
  type WalletConfig,
} from '../../src/config/wallets.js';

/** Back the mocked filesystem with an in-memory wallet store. */
const useStore = (initial: WalletConfig = {}) => {
  let store: WalletConfig = structuredClone(initial);
  mocks.readFile.mockImplementation(async () => JSON.stringify(store));
  mocks.writeFile.mockImplementation(async (_path: string, data: string) => {
    store = JSON.parse(data);
  });
  return {
    get: () => store,
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  mocks.mkdir.mockResolvedValue(undefined);
  mocks.privateKeyFromBech32.mockReturnValue({
    to_public: () => ({ hash: () => 'pub-key-hash' }),
  });
});

describe('loadWallets', () => {
  it('returns an empty config when the file does not exist yet', async () => {
    mocks.readFile.mockRejectedValue(Object.assign(new Error('missing'), { code: 'ENOENT' }));

    await expect(loadWallets()).resolves.toEqual({});
  });

  it('parses an existing wallet file', async () => {
    useStore({ alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' } });

    await expect(loadWallets()).resolves.toEqual({
      alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' },
    });
  });

  it('wraps unexpected read errors', async () => {
    mocks.readFile.mockRejectedValue(Object.assign(new Error('boom'), { code: 'EACCES' }));

    await expect(loadWallets()).rejects.toThrow('Failed to load wallet configuration');
  });
});

describe('createWallet', () => {
  it('generates a fresh keypair and persists it', async () => {
    const store = useStore();
    mocks.generateEmulatorAccountFromPrivateKey.mockReturnValue({
      privateKey: 'ed25519_sk1generated',
      address: 'addr_test_generated',
    });

    const wallet = await createWallet('alice');

    expect(wallet).toEqual({
      name: 'alice',
      privateKey: 'ed25519_sk1generated',
      address: 'addr_test_generated',
      description: undefined,
      isTestOnly: true,
    });
    expect(store.get().alice).toMatchObject({
      privateKey: 'ed25519_sk1generated',
      address: 'addr_test_generated',
      isTestOnly: true,
    });
  });

  it('refuses to overwrite an existing wallet', async () => {
    useStore({ alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' } });

    await expect(createWallet('alice')).rejects.toThrow("Wallet 'alice' already exists");
    expect(mocks.writeFile).not.toHaveBeenCalled();
  });
});

describe('importWallet', () => {
  it('rejects a key that is not in ed25519_sk bech32 form', async () => {
    useStore();

    await expect(importWallet('alice', 'not-a-key')).rejects.toThrow('Invalid private key format');
    await expect(importWallet('alice', 'ed25519_sk')).rejects.toThrow('Invalid private key format');
    expect(mocks.writeFile).not.toHaveBeenCalled();
  });

  it('derives the address from the private key and persists it', async () => {
    const store = useStore();

    const wallet = await importWallet('bob', 'ed25519_sk1validKey123');

    expect(mocks.privateKeyFromBech32).toHaveBeenCalledWith('ed25519_sk1validKey123');
    expect(wallet.address).toBe('addr_test_network_0');
    expect(store.get().bob).toMatchObject({
      privateKey: 'ed25519_sk1validKey123',
      address: 'addr_test_network_0',
      isTestOnly: true,
    });
  });

  it('refuses to overwrite an existing wallet', async () => {
    useStore({ bob: { name: 'bob', privateKey: 'ed25519_sk1b', address: 'addr2' } });

    await expect(importWallet('bob', 'ed25519_sk1validKey123')).rejects.toThrow(
      "Wallet 'bob' already exists"
    );
  });
});

describe('default-wallet protection', () => {
  it('addWallet refuses to modify a default wallet', async () => {
    useStore({ main: { privateKey: 'ed25519_sk1m', address: 'addrM', isDefault: true } });

    await expect(addWallet('main', 'ed25519_sk1x', 'addrX')).rejects.toThrow(
      'Cannot modify a default wallet'
    );
  });

  it('removeWallet refuses to remove a default wallet', async () => {
    useStore({ main: { privateKey: 'ed25519_sk1m', address: 'addrM', isDefault: true } });

    await expect(removeWallet('main')).rejects.toThrow('Cannot remove a default wallet');
  });
});

describe('removeWallet / getWallet / listWallets', () => {
  it('removes a non-default wallet', async () => {
    const store = useStore({
      alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' },
      bob: { name: 'bob', privateKey: 'ed25519_sk1b', address: 'addr2' },
    });

    await removeWallet('alice');

    expect(store.get()).toEqual({
      bob: { name: 'bob', privateKey: 'ed25519_sk1b', address: 'addr2' },
    });
  });

  it('getWallet returns the wallet or null', async () => {
    useStore({ alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' } });

    await expect(getWallet('alice')).resolves.toMatchObject({ address: 'addr1' });
    await expect(getWallet('ghost')).resolves.toBeNull();
  });

  it('listWallets returns every wallet name', async () => {
    useStore({
      alice: { name: 'alice', privateKey: 'ed25519_sk1a', address: 'addr1' },
      bob: { name: 'bob', privateKey: 'ed25519_sk1b', address: 'addr2' },
    });

    await expect(listWallets()).resolves.toEqual(['alice', 'bob']);
  });
});
