import { readFile, writeFile, mkdir } from "fs/promises";
import { join } from "path";
import { homedir } from "os";

const CONFIG_DIR = join(homedir(), ".midgard-manager");
const WALLET_CONFIG_PATH = join(CONFIG_DIR, "wallets.json");

// Default test wallet that works out of the box
const DEFAULT_WALLETS = {
  test: {
    // This is a valid formatted test private key (though not functional - DO NOT USE FOR REAL TRANSACTIONS)
    privateKey:
      "ed25519_sk1lqglg27l7j7u80y488z352yxjr7auzm9wgwctf9mdsceq4qruqus0u2td6",
  },
};

export interface WalletConfig {
  [name: string]: {
    privateKey: string;
  };
}

/**
 * Load wallet configuration from disk
 * Creates default config if it doesn't exist
 */
export const loadWallets = async (): Promise<WalletConfig> => {
  try {
    const data = await readFile(WALLET_CONFIG_PATH, "utf-8");
    return JSON.parse(data);
  } catch (error) {
    // If file doesn't exist, create default
    await saveWallets(DEFAULT_WALLETS);
    return DEFAULT_WALLETS;
  }
};

/**
 * Save wallet configuration to disk
 */
export const saveWallets = async (wallets: WalletConfig): Promise<void> => {
  try {
    // Ensure directory exists
    await mkdir(CONFIG_DIR, { recursive: true });
    await writeFile(WALLET_CONFIG_PATH, JSON.stringify(wallets, null, 2));
  } catch (error) {
    console.error("Failed to save wallet configuration:", error);
    throw error;
  }
};

/**
 * Add a wallet to the configuration
 */
export const addWallet = async (
  name: string,
  privateKey: string
): Promise<void> => {
  const wallets = await loadWallets();
  wallets[name] = { privateKey };
  await saveWallets(wallets);
};

/**
 * Remove a wallet from the configuration
 */
export const removeWallet = async (name: string): Promise<void> => {
  const wallets = await loadWallets();
  delete wallets[name];
  await saveWallets(wallets);
};

/**
 * Get a wallet by name
 */
export const getWallet = async (
  name: string
): Promise<{ privateKey: string } | null> => {
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
