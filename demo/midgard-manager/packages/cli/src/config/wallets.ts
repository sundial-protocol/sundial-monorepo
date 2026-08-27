import { CML, generateEmulatorAccountFromPrivateKey } from '@lucid-evolution/lucid';
import { mkdir, readFile, writeFile } from 'fs/promises';
import { join } from 'path';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

// Get the directory path relative to the project
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const PROJECT_ROOT = join(__dirname, '../../../..'); // Simplified path to project root
const CONFIG_DIR = join(PROJECT_ROOT, 'config/wallets');
const WALLET_CONFIG_PATH = join(CONFIG_DIR, 'default.json');

// Same ed25519 bech32 key format Lucid Evolution's private-key wallet
// selection expects.
const PRIVATE_KEY_REGEX = /^ed25519_sk[a-zA-Z0-9]+$/;

// Enterprise (no staking part), testnet networkId=0 — matches the address
// shape generateEmulatorAccountFromPrivateKey produces, and what
// sundial-node's Phase-A validation expects for a non-Mainnet network.
const deriveAddressFromPrivateKey = (privateKey: string): string => {
  const pubKeyHash = CML.PrivateKey.from_bech32(privateKey).to_public().hash();
  return CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(pubKeyHash))
    .to_address()
    .to_bech32(undefined);
};

export interface WalletConfig {
  [name: string]: {
    name?: string;
    privateKey: string;
    address: string;
    description?: string;
    isDefault?: boolean;
    isTestOnly?: boolean;
  };
}

/**
 * Load wallet configuration from disk
 * Creates default config if it doesn't exist
 */
export const loadWallets = async (): Promise<WalletConfig> => {
  // Ensure the wallets directory exists
  await mkdir(CONFIG_DIR, { recursive: true });

  try {
    const data = await readFile(WALLET_CONFIG_PATH, 'utf-8');
    return JSON.parse(data);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
      // No wallets created yet.
      return {};
    }
    console.error('Error loading wallets:', error);
    throw new Error('Failed to load wallet configuration');
  }
};

/**
 * Save wallet configuration to disk
 */
const saveWallets = async (wallets: WalletConfig): Promise<void> => {
  await mkdir(CONFIG_DIR, { recursive: true });
  await writeFile(WALLET_CONFIG_PATH, JSON.stringify(wallets, null, 2));
};

/**
 * Add a test wallet to the configuration
 */
export const addWallet = async (
  name: string,
  privateKey: string,
  address: string,
  description?: string
): Promise<void> => {
  const wallets = await loadWallets();

  // Check if trying to modify a default wallet
  if (wallets[name]?.isDefault) {
    throw new Error('Cannot modify a default wallet');
  }

  wallets[name] = {
    name,
    privateKey,
    address,
    description,
    isTestOnly: true,
  };
  await saveWallets(wallets);
};

/**
 * Remove a test wallet from the configuration
 */
export const removeWallet = async (name: string): Promise<void> => {
  const wallets = await loadWallets();

  // Check if trying to remove a default wallet
  if (wallets[name]?.isDefault) {
    throw new Error('Cannot remove a default wallet');
  }

  delete wallets[name];
  await saveWallets(wallets);
};

/**
 * Get a wallet by name
 */
export const getWallet = async (name: string): Promise<WalletConfig[string] | null> => {
  const wallets = await loadWallets();
  return wallets[name] || null;
};

/**
 * List all wallet names
 */
export const listWallets = async (): Promise<string[]> => {
  const wallets = await loadWallets();
  return Object.keys(wallets);
};

/**
 * Generate a fresh keypair and register it as a new wallet.
 */
export const createWallet = async (
  name: string,
  description?: string
): Promise<WalletConfig[string]> => {
  const wallets = await loadWallets();
  if (wallets[name]) {
    throw new Error(`Wallet '${name}' already exists`);
  }

  const { privateKey, address } = generateEmulatorAccountFromPrivateKey({});
  await addWallet(name, privateKey, address, description);
  return { name, privateKey, address, description, isTestOnly: true };
};

/**
 * Register an existing private key as a named wallet, deriving its address.
 */
export const importWallet = async (
  name: string,
  privateKey: string,
  description?: string
): Promise<WalletConfig[string]> => {
  if (!PRIVATE_KEY_REGEX.test(privateKey)) {
    throw new Error('Invalid private key format. Expected ed25519_sk... (bech32)');
  }

  const wallets = await loadWallets();
  if (wallets[name]) {
    throw new Error(`Wallet '${name}' already exists`);
  }

  const address = deriveAddressFromPrivateKey(privateKey);
  await addWallet(name, privateKey, address, description);
  return { name, privateKey, address, description, isTestOnly: true };
};
