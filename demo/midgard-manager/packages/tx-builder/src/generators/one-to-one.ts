import { Writable } from 'node:stream';

import {
  Data,
  Emulator,
  EmulatorAccount,
  Lucid,
  LucidEvolution,
  Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';

import { LucidPool } from '../lucid-pool.js';
import { SerializedMidgardTransaction } from '../types.js';
import { waitWritable } from '../utils.js';

export interface OneToOneTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  txsCount: number;
  walletSeedOrPrivateKey: string;
  writable?: Writable;
  random?: () => number;
  deterministicStartMs?: number;
  lucid?: LucidEvolution;
}

const GC_PAUSE_INTERVAL = 1000;
const GC_PAUSE_DURATION_MS = 10;
const MIN_LOVELACE_OUTPUT = 1_000_000n;

const generateUniqueHexDatum = (
  counter: number,
  random: () => number,
  deterministicStartMs: number
): string => {
  const timestamp = (deterministicStartMs + counter).toString(16).padStart(12, '0');
  const randomHex = Math.floor(random() * 16777215)
    .toString(16)
    .padStart(6, '0');
  const count = counter.toString(16).padStart(6, '0');
  return timestamp + randomHex + count;
};

const validateConfig = (config: OneToOneTransactionConfig): void => {
  const { initialUTxO, walletSeedOrPrivateKey } = config;
  if (!walletSeedOrPrivateKey.startsWith('ed25519_sk')) {
    throw new Error('Invalid private key format. Expected Lucid emulator account private key.');
  }
  if (initialUTxO.assets.lovelace < MIN_LOVELACE_OUTPUT) {
    throw new Error('Initial UTxO must have at least 1 ADA');
  }
  if (config.txsCount < 1) {
    throw new Error('Transaction count must be at least 1');
  }
};

const initializeLucid = async (emulator: Emulator, network: Network): Promise<LucidEvolution> => {
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

export const generateOneToOneTransactions = async (
  config: OneToOneTransactionConfig
): Promise<SerializedMidgardTransaction[]> => {
  const {
    network,
    initialUTxO,
    txsCount,
    writable,
    walletSeedOrPrivateKey,
    random = Math.random,
    deterministicStartMs = Date.now(),
    lucid: pooledLucid,
  } = config;

  validateConfig(config);

  let lucid: LucidEvolution;
  if (pooledLucid !== undefined) {
    lucid = pooledLucid;
  } else {
    const account: EmulatorAccount = {
      seedPhrase: '',
      address: initialUTxO.address,
      assets: initialUTxO.assets,
      privateKey: walletSeedOrPrivateKey,
    };
    const emulator = new Emulator([account]);
    emulator.ledger = {
      [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
        utxo: initialUTxO,
        spent: false,
      },
    };
    lucid = await initializeLucid(emulator, network);
  }

  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  const transactions: SerializedMidgardTransaction[] = [];

  try {
    if (txsCount > 1) {
      const initialTxBuilder = lucid.newTx();
      const [, , initialTxSignBuilder] = await initialTxBuilder.pay
        .ToAddress(initialUTxO.address, initialUTxO.assets)
        .chain();
      const initialTxSigned = await initialTxSignBuilder.sign
        .withPrivateKey(walletSeedOrPrivateKey)
        .complete();
      const firstUtxo = {
        txHash: initialTxSigned.toHash(),
        outputIndex: initialUTxO.outputIndex,
        address: initialUTxO.address,
        assets: initialUTxO.assets,
      };
      lucid.selectWallet.fromAddress(firstUtxo.address, [firstUtxo]);
      initialTxBuilder.rawConfig().txBuilder.free();
      initialTxSignBuilder.toTransaction().free();
      initialTxSigned.toTransaction().free();
    }

    for (let i = 0; i < txsCount; i++) {
      const txBuilder = lucid.newTx();
      const [newWalletUTxOs, , txSignBuilder] = await txBuilder.pay
        .ToAddressWithData(
          initialUTxO.address,
          {
            kind: 'inline',
            value: Data.to(generateUniqueHexDatum(i, random, deterministicStartMs)),
          },
          initialUTxO.assets
        )
        .chain();
      const txSigned = await txSignBuilder.sign.withPrivateKey(walletSeedOrPrivateKey).complete();

      const txHash = txSigned.toHash();
      const tx: SerializedMidgardTransaction = {
        cborHex: txSigned.toCBOR(),
        description: `One-to-One Self Transfer ()`,
        txId: txHash,
        type: 'Midgard L2 User Transaction',
      };

      transactions.push(tx);

      if (writable) {
        await waitWritable(writable);
        writable.write(JSON.stringify([tx], null, 2) + '\n');
      }

      lucid.overrideUTxOs(newWalletUTxOs);

      txBuilder.rawConfig().txBuilder.free();
      txSignBuilder.toTransaction().free();
      txSigned.toTransaction().free();

      if (i > 0 && i % GC_PAUSE_INTERVAL === 0) {
        await new Promise<void>((resolve) => setTimeout(() => resolve(), GC_PAUSE_DURATION_MS));
      }
    }
  } catch (error) {
    console.error('Error generating transactions:', error);
    throw error;
  }

  return transactions;
};

export { LucidPool };
