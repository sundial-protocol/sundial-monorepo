import { Writable } from "node:stream";

import {
  Emulator,
  EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  LucidEvolution,
  Network,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from "@lucid-evolution/lucid";
import pLimit from "p-limit";

import { SerializedMidgardTransaction } from "../client/types.js";
import {
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  waitWritable,
} from "../../utils/common.js";
import { MidgardNodeClient } from "../client/node-client.js";

// Constants
const TOTAL_ACCOUNT_COUNT = 100;
const OUTPUT_UTXOS_CHUNK = 20;
const GC_PAUSE_INTERVAL = 250; // number of transactions before GC pause
const ACCOUNT_GENERATION_CONCURRENCY = 10;
const MIN_LOVELACE_OUTPUT = 1_000_000n; // Minimum lovelace per output

interface MultiOutputTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  utxosCount: number; // number of UTxOs to generate
  finalUtxosCount?: number; // number of UTxOs to consolidate into at the end (default: 1)
  walletSeedOrPrivateKey: string;
  writable?: Writable;
  nodeClient?: MidgardNodeClient;
}

const validateConfig = (config: MultiOutputTransactionConfig): void => {
  const {
    initialUTxO,
    utxosCount,
    finalUtxosCount = 1,
    walletSeedOrPrivateKey,
  } = config;

  if (finalUtxosCount > utxosCount / OUTPUT_UTXOS_CHUNK) {
    throw new Error(
      `Final UTxO Count can be ${Math.floor(
        utxosCount / OUTPUT_UTXOS_CHUNK
      )} at maximum`
    );
  }

  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);
  const publicKeyHash = getPublicKeyHashFromPrivateKey(privateKey);
  const initialUTxOAddressPubKeyHash = paymentCredentialOf(
    initialUTxO.address
  ).hash;

  if (publicKeyHash !== initialUTxOAddressPubKeyHash) {
    throw new Error("Payment Key is not valid to spend Initial UTxO");
  }

  const outputLovelace =
    (initialUTxO.assets.lovelace - MIN_LOVELACE_OUTPUT) / BigInt(utxosCount);
  if (outputLovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error("Not enough Lovelace to distribute");
  }
};

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

const generateTestAccounts = async (
  count: number
): Promise<EmulatorAccount[]> => {
  const limit = pLimit(ACCOUNT_GENERATION_CONCURRENCY);
  return Promise.all(
    Array.from({ length: count }, () =>
      limit(() => generateEmulatorAccountFromPrivateKey({}))
    )
  );
};

const calculateOutputLovelace = (
  totalLovelace: bigint,
  utxosCount: number
): bigint => {
  return (totalLovelace - MIN_LOVELACE_OUTPUT) / BigInt(utxosCount);
};

/**
 * Generate complex multi-output transactions for testing.
 * First splits UTxOs into multiple outputs, then merges them back.
 */
const generateMultiOutputTransactions = async (
  config: MultiOutputTransactionConfig
): Promise<SerializedMidgardTransaction[]> => {
  const {
    network,
    initialUTxO,
    utxosCount,
    finalUtxosCount = 1,
    writable,
    nodeClient,
  } = config;

  validateConfig(config);
  const privateKey = parseUnknownKeytoBech32PrivateKey(
    config.walletSeedOrPrivateKey
  );

  // Setup emulator env
  const mainAccount: EmulatorAccount = {
    seedPhrase: "",
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey,
  };

  const emulator = new Emulator([mainAccount]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

  const lucid = await initializeLucid(emulator, network);
  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // Generate test accounts for distribution
  const accounts = await generateTestAccounts(TOTAL_ACCOUNT_COUNT);

  // State tracking
  let currentUtxosCount = 1;
  let currentTxsCount = 0;
  const rollBackers: { utxos: UTxO[]; privateKey: string }[] = [];
  const outputLovelace = calculateOutputLovelace(
    initialUTxO.assets.lovelace,
    utxosCount
  );

  // Store all generated transactions
  const transactions: SerializedMidgardTransaction[] = [];

  try {
    // Distribution phase: Generate transactions with multiple outputs
    while (currentUtxosCount < utxosCount) {
      const randomAccount =
        accounts[Math.floor(Math.random() * TOTAL_ACCOUNT_COUNT)];
      const txBuilder = lucid.newTx();

      // create multiple outputs in single transaction
      Array.from({ length: OUTPUT_UTXOS_CHUNK }).forEach(() =>
        txBuilder.pay.ToAddress(randomAccount.address, {
          lovelace: outputLovelace,
        })
      );

      const [newWalletUTxOs, derivedOutputs, txSignBuilder] =
        await txBuilder.chain();
      let txSigned = await txSignBuilder.sign
        .withPrivateKey(privateKey)
        .complete();

      // Create serialized transaction in Midgard format
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `Multi-Output Distribution (1-to-${OUTPUT_UTXOS_CHUNK})`,
        txId: txHash,
        type: "Midgard L2 User Transaction",
      };

      // Submit to node if client provided
      if (nodeClient) {
        await nodeClient.submitTransaction(tx.cborHex, tx.description);
      }

      transactions.push(tx);

      // Write to test output if writable provided
      if (writable) {
        await waitWritable(writable);
        writable.write(JSON.stringify(tx, null, 2) + "\n");
      }

      currentUtxosCount += OUTPUT_UTXOS_CHUNK;
      currentTxsCount += 1;

      // Update UTxO state
      lucid.overrideUTxOs(newWalletUTxOs);

      // Store outputs for collection phase
      derivedOutputs.pop(); // Remove own output
      rollBackers.unshift({
        utxos: derivedOutputs,
        privateKey: randomAccount.privateKey,
      });

      // Cleanup resource
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause
      if (currentTxsCount % GC_PAUSE_INTERVAL === 0) {
        await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
      }
    }

    // Collection phase: Gather UTxOs back
    for (let i = 0; i < rollBackers.length; i += finalUtxosCount) {
      const chunk = rollBackers.slice(i, i + finalUtxosCount);
      const txBuilder = lucid.newTx();

      // Add inputs from each rollbacker
      chunk.forEach(({ utxos }) =>
        utxos.forEach((utxo) => txBuilder.collectFrom([utxo]))
      );

      // Add single output back to main account
      txBuilder.pay.ToAddress(mainAccount.address, {
        lovelace: outputLovelace * BigInt(OUTPUT_UTXOS_CHUNK * chunk.length),
      });

      const [newWalletUTxOs, , txSignBuilder] = await txBuilder.chain();

      // Sign with main key and all rollbacker keys
      const txSigned = await txSignBuilder.sign
        .withPrivateKey(privateKey)
        .complete();

      // Create serialized transaction
      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `Multi-Output Collection (${OUTPUT_UTXOS_CHUNK}-to-1)`,
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

      // Update UTxO state
      lucid.overrideUTxOs(newWalletUTxOs);

      // Cleanup resources
      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      // Periodic GC pause
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

export { generateMultiOutputTransactions };
export type { MultiOutputTransactionConfig };
