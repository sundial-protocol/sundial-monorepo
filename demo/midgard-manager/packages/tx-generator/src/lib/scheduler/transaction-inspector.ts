import { Network } from '@lucid-evolution/lucid';

import { inspectTransactionCbor } from '../../../../../../midgard-ts/src/validation/inspect.js';
import { SerializedMidgardTransaction } from '../types.js';

export type InspectedGeneratedTransaction = {
  transaction: SerializedMidgardTransaction;
  inspection: ReturnType<typeof inspectTransactionCbor>;
};

const resolveNetworkId = (network: Network): number => (network === 'Mainnet' ? 1 : 0);

export const inspectGeneratedTransaction = (
  transaction: SerializedMidgardTransaction,
  network: Network
): InspectedGeneratedTransaction => {
  const expectedNetworkId = resolveNetworkId(network);

  return {
    transaction,
    inspection: inspectTransactionCbor({
      cborHex: transaction.cborHex,
      expectedTxIdHex: transaction.txId,
      phaseAConfig: {
        expectedNetworkId,
        cardanoNetwork: expectedNetworkId,
        minFeeA: 0n,
        minFeeB: 0n,
      },
    }),
  };
};
