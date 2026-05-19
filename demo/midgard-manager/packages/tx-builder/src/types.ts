export type TransactionType = 'one-to-one' | 'multi-output' | 'mixed';

export interface SerializedMidgardTransaction {
  type: 'Midgard L2 User Transaction';
  description: string;
  cborHex: string;
  txId: string;
}
