import { CML, generateEmulatorAccountFromPrivateKey, walletFromSeed } from '@lucid-evolution/lucid';

export const parseUnknownKeytoBech32PrivateKey = (unknownKey: unknown): string => {
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

export const getPublicKeyHashFromPrivateKey = (privateKey: string): string => {
  return CML.PrivateKey.from_bech32(privateKey).to_public().hash().to_hex();
};

export const waitWritable = (writable: NodeJS.WritableStream): Promise<void> => {
  return new Promise((resolve) => {
    if (writable.writable) {
      resolve();
    } else {
      writable.once('drain', resolve);
    }
  });
};

export async function generateTestWallet(): Promise<{ privateKey: string; address: string }> {
  const account = await generateEmulatorAccountFromPrivateKey({});
  return { privateKey: account.privateKey, address: account.address };
}

// Derives a Cardano enterprise address from a bech32 private key.
// networkId: 0 = testnet (Preview/Preprod), 1 = mainnet.
export function getAddressFromPrivateKey(privateKey: string, networkId: 0 | 1): string {
  const pubKeyHash = CML.PrivateKey.from_bech32(privateKey).to_public().hash();
  const credential = CML.Credential.new_pub_key(pubKeyHash);
  return CML.EnterpriseAddress.new(networkId, credential).to_address().to_bech32(undefined);
}
