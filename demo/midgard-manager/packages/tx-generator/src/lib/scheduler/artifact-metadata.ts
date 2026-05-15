import { TransactionGeneratorConfig } from '../types.js';
import { InspectedGeneratedTransaction } from './transaction-inspector.js';

export type TransactionProfile =
  | 'one-to-one'
  | 'multi-output-distribution'
  | 'multi-output-collection'
  | 'replay'
  | 'unknown';

export type SubmissionStatus =
  | 'SUBMITTED'
  | 'ERROR'
  | 'NODE_UNAVAILABLE'
  | 'NOT_ATTEMPTED'
  | 'VALIDATION_REJECTED';

export type TransactionEvidenceEntry = {
  txId: string;
  computedTxId: string | null;
  cborByteSize: number;
  midgardByteSize: number | null;
  profile: TransactionProfile;
  validation: {
    status: 'accepted' | 'rejected';
    rejectCode?: string;
    detail?: string | null;
  };
  submission: {
    status: SubmissionStatus;
    error?: string;
  };
};

export type GeneratorManifest = {
  mode: 'generated' | 'replay';
  generationSeed: string;
  replayCorpusPath: string | undefined;
  profile: {
    transactionType: TransactionGeneratorConfig['transactionType'];
    oneToOneRatio: number | undefined;
    batchSize: number;
    concurrency: number;
    intervalSeconds: number;
  };
  generatedAt: string;
  transactionCount: number;
  transactions: TransactionEvidenceEntry[];
};

const includes = (value: string, searchValue: string): boolean =>
  value.toLowerCase().includes(searchValue.toLowerCase());

export const classifyTransactionProfile = (
  description: string,
  mode: 'generated' | 'replay'
): TransactionProfile => {
  if (mode === 'replay') {
    return 'replay';
  }
  if (includes(description, 'one-to-one')) {
    return 'one-to-one';
  }
  if (includes(description, 'distribution')) {
    return 'multi-output-distribution';
  }
  if (includes(description, 'collection')) {
    return 'multi-output-collection';
  }
  return 'unknown';
};

export const toEvidenceEntry = (
  inspectedTransaction: InspectedGeneratedTransaction,
  mode: 'generated' | 'replay'
): TransactionEvidenceEntry => {
  const { transaction, inspection } = inspectedTransaction;
  const profile = classifyTransactionProfile(transaction.description, mode);

  if (inspection.validation.status === 'rejected') {
    return {
      txId: transaction.txId,
      computedTxId: inspection.computedTxIdHex,
      cborByteSize: inspection.cborByteSize,
      midgardByteSize: inspection.midgardByteSize,
      profile,
      validation: {
        status: 'rejected',
        rejectCode: inspection.validation.rejectCode,
        detail: inspection.validation.detail,
      },
      submission: {
        status: 'VALIDATION_REJECTED',
      },
    };
  }

  return {
    txId: transaction.txId,
    computedTxId: inspection.computedTxIdHex,
    cborByteSize: inspection.cborByteSize,
    midgardByteSize: inspection.midgardByteSize,
    profile,
    validation: {
      status: 'accepted',
    },
    submission: {
      status: 'NOT_ATTEMPTED',
    },
  };
};
