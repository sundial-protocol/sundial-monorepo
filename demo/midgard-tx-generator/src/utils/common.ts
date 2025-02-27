import { Writable } from 'node:stream';

import { Assets, CML, Data, walletFromSeed } from '@lucid-evolution/lucid';

/**
 * Core utility functions for Midgard transaction generation
 */

type SerializedAssets = Record<string, string>;

/**
 * Serializes assets to string format
 */
export const serializeAssets = (assets: Assets): SerializedAssets => {
  return Object.fromEntries(
    Object.entries(assets).map(([asset, value]) => [asset, value.toString()])
  );
};

/**
 * Parses a key into bech32 private key format
 */
export const parseUnknownKeytoBech32PrivateKey = (
  unknownKey: unknown
): string => {
  if (typeof unknownKey !== 'string')
    throw new Error('Expected a string value for the private key');

  if (unknownKey.trim().includes(' ')) {
    const wallet = walletFromSeed(unknownKey.trim(), {
      accountIndex: 0,
      addressType: 'Base',
    });
    return wallet.paymentKey;
  } else {
    try {
      const paymentKey = CML.PrivateKey.from_normal_bytes(
        Buffer.from(unknownKey.substring(4), 'hex')
      );
      return paymentKey.to_bech32();
    } catch {
      const paymentKey = CML.PrivateKey.from_bech32(unknownKey.trim());
      return paymentKey.to_bech32();
    }
  }
};

/**
 * Gets public key hash from private key
 */
export const getPublicKeyHashFromPrivateKey = (privateKey: string): string => {
  return CML.PrivateKey.from_bech32(privateKey).to_public().hash().to_hex();
};

/**
 * Gets CBOR hex representation of private key
 */
export const getPrivateKeyCborHex = (privateKey: string): string => {
  return Data.to(
    Buffer.from(CML.PrivateKey.from_bech32(privateKey).to_raw_bytes()).toString(
      'hex'
    )
  );
};

/**
 * Ensures the writable stream is ready before writing
 * @param writable - The writable stream to check
 * @returns Promise that resolves when the stream is writable
 */
const waitWritable = (writable: Writable): Promise<void> => {
  return new Promise((resolve) => {
    setInterval(() => {
      if (writable.writable) resolve();
    }, 10);
  });
};

export { waitWritable };
