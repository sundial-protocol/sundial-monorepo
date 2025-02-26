import { readFile, writeFile, mkdir } from "fs/promises";
import { join } from "path";
import { fileURLToPath } from "url";
import { dirname } from "path";

// Get the directory path relative to the monorepo
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const MONOREPO_ROOT = join(__dirname, "../../../../../..");  // Fixed path to reach the actual monorepo root
const PROJECT_ROOT = join(MONOREPO_ROOT, "demo/midgard-manager");

// Store wallets in a standard location within the project structure
const CONFIG_DIR = join(PROJECT_ROOT, "config/wallets");
const WALLET_CONFIG_PATH = join(CONFIG_DIR, "default.json");

// Default test wallet that's always available for transaction generation
// WARNING: This is only for testing/development - never use with real funds!

const DEFAULT_WALLETS = {
  test: {
    privateKey: "ed25519_sk1lqglg27l7j7u80y488z352yxjr7auzm9wgwctf9mdsceq4qruqus0u2td6",
    description: "Default test wallet for transaction generation",
    isTestOnly: true
  }
};

export interface WalletConfig {
  [name: string]: {
    privateKey: string;
    description?: string;
    isTestOnly: boolean;
  };
}

/**
 * Load wallet configuration from disk
 * Creates default config if it doesn't exist
 */
export const loadWallets = async (): Promise<WalletConfig> => {
  try {
    // Ensure the test-wallets directory exists
    await mkdir(CONFIG_DIR, { recursive: true });
    
    const data = await readFile(WALLET_CONFIG_PATH, "utf-8");
    const wallets = JSON.parse(data);
    
    // Always ensure the default test wallet exists and can't be modified
    wallets.test = DEFAULT_WALLETS.test;
    
    return wallets;
  } catch (error) {
    // If file doesn't exist, create default
    await saveWallets(DEFAULT_WALLETS);
    return DEFAULT_WALLETS;
  }
};

/**
 * Save wallet configuration to disk
 */
const saveWallets = async (wallets: WalletConfig): Promise<void> => {
  // Always ensure the default test wallet exists and can't be modified
  wallets.test = DEFAULT_WALLETS.test;
  
  await mkdir(CONFIG_DIR, { recursive: true });
  await writeFile(WALLET_CONFIG_PATH, JSON.stringify(wallets, null, 2));
};

/**
 * Add a test wallet to the configuration
 */
export const addWallet = async (
  name: string,
  privateKey: string,
  description?: string
): Promise<void> => {
  if (name === 'test') {
    throw new Error("Cannot modify the default test wallet");
  }
  
  const wallets = await loadWallets();
  wallets[name] = { 
    privateKey, 
    description,
    isTestOnly: true 
  };
  await saveWallets(wallets);
};

/**
 * Remove a test wallet from the configuration
 */
export const removeWallet = async (name: string): Promise<void> => {
  if (name === 'test') {
    throw new Error("Cannot remove the default test wallet");
  }
  
  const wallets = await loadWallets();
  delete wallets[name];
  await saveWallets(wallets);
};

/**
 * Get a wallet by name
 */
export const getWallet = async (
  name: string
): Promise<{ privateKey: string; description?: string; isTestOnly: boolean } | null> => {
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
