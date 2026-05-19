import { Network } from '@lucid-evolution/lucid';
import * as midgardTs from 'midgard-ts';

import { SerializedMidgardTransaction } from '../types.js';

type InspectTxConfig = {
  cborHex: string;
  expectedTxIdHex?: string;
  phaseAConfig: {
    expectedNetworkId: number;
    cardanoNetwork: number;
    minFeeA: bigint;
    minFeeB: bigint;
  };
};

type InspectTxResult = {
  computedTxIdHex: string | null;
  cborByteSize: number;
  midgardByteSize: number | null;
  validation:
    | { status: 'accepted' }
    | {
        status: 'rejected';
        rejectCode: string;
        detail: string | null;
      };
  shape: unknown | null;
};

type InspectFn = (config: InspectTxConfig) => InspectTxResult;

export type InspectedGeneratedTransaction = {
  transaction: SerializedMidgardTransaction;
  inspection: ReturnType<InspectFn>;
};

const resolveNetworkId = (network: Network): number => (network === 'Mainnet' ? 1 : 0);

const resolveInspectTransactionCbor = (): InspectFn => {
  const maybeFromNamespace = (midgardTs as { inspectTransactionCbor?: InspectFn })
    .inspectTransactionCbor;
  if (typeof maybeFromNamespace === 'function') {
    return maybeFromNamespace;
  }

  const maybeFromDefault = (
    midgardTs as {
      default?: { inspectTransactionCbor?: InspectFn };
    }
  ).default?.inspectTransactionCbor;
  if (typeof maybeFromDefault === 'function') {
    return maybeFromDefault;
  }

  throw new Error(
    'inspectTransactionCbor export is not available from midgard-ts/src/validation/inspect.js'
  );
};

const inspectTransactionCbor = resolveInspectTransactionCbor();

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
