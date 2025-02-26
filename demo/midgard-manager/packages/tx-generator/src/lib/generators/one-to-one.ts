import { Writable } from "node:stream";

import {
  Emulator,
  EmulatorAccount,
  Lucid,
  LucidEvolution,
  Network,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from "@lucid-evolution/lucid";

import { SerializedMidgardTransaction } from "../client/types.js";
import { MidgardNodeClient } from "../client/node-client.js";
import {
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  waitWritable,
} from "../../utils/common.js";

/**
 * Configuration for generating one-to-one transactions.
 * These transactions simulate simple transfers between addresses.
 * @interface OneToOneTransactionConfig
 * @property {Network} network - The network configuration (testnet/mainnet)
 * @property {UTxO} initialUTxO - The initial UTxO to use for transaction generation
 * @property {number} txsCount - Number of transactions to generate
 * @property {string} walletSeedOrPrivateKey - Wallet seed phrase or private key for signing
 * @property {Writable} [writable] - Optional writable stream for test output
 * @property {MidgardNodeClient} [nodeClient] - Optional node client for submitting transactions
 */
interface OneToOneTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  txsCount: number;
  walletSeedOrPrivateKey: string;
  writable?: Writable;
  nodeClient?: MidgardNodeClient;
}

// Constants for transaction generation
const GC_PAUSE_INTERVAL = 1000; // Number of transactions before GC pause
const MIN_LOVELACE_OUTPUT = 1_000_000n; // Minimum lovelace per output

/**
 * Validates the configuration parameters
 * @param config - Transaction generation configuration
 * @throws Error if configuration is invalid
 */
const validateConfig = (config: OneToOneTransactionConfig): void => {
  const { initialUTxO, walletSeedOrPrivateKey } = config;

  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);
  const publicKeyHash = getPublicKeyHashFromPrivateKey(privateKey);
  const initialUTxOAddressPubKeyHash = paymentCredentialOf(
    initialUTxO.address
  ).hash;

  if (publicKeyHash !== initialUTxOAddressPubKeyHash) {
    throw new Error("Payment Key is not valid to spend Initial UTxO");
  }

  if (initialUTxO.assets.lovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error("Initial UTxO must have at least 1 ADA");
  }
};

/**
 * Initializes the Lucid instance with test configuration
 * @param emulator - Emulator instance
 * @param network - Network configuration
 * @returns Configured Lucid instance
 */
const initializeLucid = async (
  emulator: Emulator,
  network: Network
): Promise<LucidEvolution> => {
  return await Lucid(emulator, network, {
    presetProtocolParameters: {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      minFeeA: 0,
      minFeeB: 0,
      priceMem: 0,
      priceStep: 0,
      coinsPerUtxoByte: 0n,
    },
  });
};

/**
 * Generate simple one-to-one transactions for testing.
 * Each transaction has one input and one output with the same value.
 */
const generateOneToOneTransactions = async (
  config: OneToOneTransactionConfig
): Promise<SerializedMidgardTransaction[]> => {
  const { network, initialUTxO, txsCount, writable, nodeClient } = config;

  // Validate configuration
  validateConfig(config);
  const privateKey = parseUnknownKeytoBech32PrivateKey(
    config.walletSeedOrPrivateKey
  );

  // Setup emulator environment
  const account: EmulatorAccount = {
    seedPhrase: "",
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey,
  };

  const emulator = new Emulator([account]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

  const lucid = await initializeLucid(emulator, network);
  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // Generate mock transactions
  const transactions: SerializedMidgardTransaction[] = [];

  try {
    for (let i = 0; i < txsCount; i++) {
      const txBuilder = lucid.newTx();

      const [newWalletUTxOs, , txSignBuilder] = await txBuilder.pay
        .ToAddress(initialUTxO.address, initialUTxO.assets)
        .chain();

      const txSigned = await txSignBuilder.sign
        .withPrivateKey(privateKey)
        .complete();

      // Create serialized transaction in Midgard format
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: "One-to-One Self Transfer",
        txId: txHash,
        type: "Midgard L2 User Transaction",
      };

      // Submit to node if client provided
      if (nodeClient) {
        await nodeClient.submitTransaction(tx.cborHex, tx.description);
      }

      // Add to transactions array
      transactions.push(tx);

      // Write to test output if writable provided
      if (writable) {
        await waitWritable(writable);
        writable.write(JSON.stringify(tx, null, 2) + "\n");
      }

      // Update wallet state for next transaction
      lucid.overrideUTxOs(newWalletUTxOs);

      // Cleanup resources
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause to prevent memory pressure
      if (i % GC_PAUSE_INTERVAL === 0) {
        await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
      }
    }
  } catch (error) {
    console.error("Error generating transactions:", error);
    throw error;
  }

  return transactions;
};

export { generateOneToOneTransactions };
export type { OneToOneTransactionConfig };
