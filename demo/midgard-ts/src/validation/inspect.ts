import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";

import { cmlToMidgard } from "../cardano";
import { encodeTransaction, type Transaction } from "../types/transaction";
import { runPhaseAValidation } from "./phase-a";
import {
  PhaseAConfig,
  RejectCodes,
  type RejectCode,
  type QueuedTx,
} from "./types";

export type TransactionValidationStatus = "accepted" | "rejected";

export type TransactionValidationResult =
  | { status: "accepted" }
  | {
      status: "rejected";
      rejectCode: RejectCode;
      detail: string | null;
    };

export type TransactionShapeMetadata = {
  inputCount: number;
  outputCount: number;
  referenceInputCount: number;
  requiredSignerCount: number;
  requiredObserverCount: number;
  vkeyWitnessCount: number;
  nativeScriptWitnessCount: number;
  hasMint: boolean;
  hasInlineDatum: boolean;
  hasScriptReference: boolean;
  hasRedeemers: boolean;
  hasPlutusV3Scripts: boolean;
  hasAuxiliaryDataHash: boolean;
  hasScriptDataHash: boolean;
  hasNetworkId: boolean;
  isValidFlag: boolean;
};

export type InspectTransactionCborConfig = {
  cborHex: string;
  expectedTxIdHex?: string;
  phaseAConfig: PhaseAConfig;
};

export type InspectTransactionCborResult = {
  computedTxIdHex: string | null;
  cborByteSize: number;
  midgardByteSize: number | null;
  validation: TransactionValidationResult;
  shape: TransactionShapeMetadata | null;
};

const HEX_REGEX = /^[0-9a-fA-F]+$/;

const isHex = (value: string): boolean =>
  value.length > 0 && value.length % 2 === 0 && HEX_REGEX.test(value);

const normalizeHex = (value: string): string => value.toLowerCase();

const buildShapeMetadata = (tx: Transaction): TransactionShapeMetadata => ({
  inputCount: tx.body.inputs.length,
  outputCount: tx.body.outputs.length,
  referenceInputCount: tx.body.reference_inputs?.length ?? 0,
  requiredSignerCount: tx.body.required_signers?.length ?? 0,
  requiredObserverCount: tx.body.required_observers?.length ?? 0,
  vkeyWitnessCount: tx.witness_set.vkey_witnesses?.length ?? 0,
  nativeScriptWitnessCount: tx.witness_set.native_scripts?.length ?? 0,
  hasMint: tx.body.mint !== undefined && tx.body.mint.length > 0,
  hasInlineDatum: tx.body.outputs.some((output) => output.datum !== undefined),
  hasScriptReference: tx.body.outputs.some(
    (output) => output.script_ref !== undefined,
  ),
  hasRedeemers: tx.witness_set.redeemers !== undefined,
  hasPlutusV3Scripts:
    tx.witness_set.plutus_v3_scripts !== undefined &&
    tx.witness_set.plutus_v3_scripts.length > 0,
  hasAuxiliaryDataHash: tx.body.auxiliary_data_hash !== undefined,
  hasScriptDataHash: tx.body.script_data_hash !== undefined,
  hasNetworkId: tx.body.network_id !== undefined,
  isValidFlag: tx.is_valid,
});

const rejected = (
  computedTxIdHex: string | null,
  cborByteSize: number,
  rejectCode: RejectCode,
  detail: string | null,
): InspectTransactionCborResult => ({
  computedTxIdHex,
  cborByteSize,
  midgardByteSize: null,
  validation: { status: "rejected", rejectCode, detail },
  shape: null,
});

export function inspectTransactionCbor(
  config: InspectTransactionCborConfig,
): InspectTransactionCborResult {
  const cborHex = config.cborHex.trim();
  const expectedTxIdHex = config.expectedTxIdHex?.trim().toLowerCase();

  if (!isHex(cborHex)) {
    return rejected(
      null,
      0,
      RejectCodes.CborDeserialization,
      "cborHex must be a non-empty even-length hexadecimal string",
    );
  }

  const cborByteSize = cborHex.length / 2;

  let cmlTx: CML.Transaction;
  try {
    cmlTx = CML.Transaction.from_cbor_hex(cborHex);
  } catch (error) {
    return rejected(
      null,
      cborByteSize,
      RejectCodes.CborDeserialization,
      String(error),
    );
  }

  const computedTxIdHex = CML.hash_transaction(cmlTx.body()).to_hex();

  if (expectedTxIdHex !== undefined) {
    if (!isHex(expectedTxIdHex) || expectedTxIdHex.length !== 64) {
      return rejected(
        computedTxIdHex,
        cborByteSize,
        RejectCodes.TxHashMismatch,
        "expectedTxIdHex must be a 32-byte hexadecimal hash",
      );
    }

    if (normalizeHex(computedTxIdHex) !== normalizeHex(expectedTxIdHex)) {
      return rejected(
        computedTxIdHex,
        cborByteSize,
        RejectCodes.TxHashMismatch,
        `provided ${expectedTxIdHex} != computed ${computedTxIdHex}`,
      );
    }
  }

  let midgardTx: Transaction;
  try {
    midgardTx = cmlToMidgard(cmlTx);
  } catch (error) {
    return rejected(
      computedTxIdHex,
      cborByteSize,
      RejectCodes.UnsupportedFieldNonEmpty,
      String(error),
    );
  }

  const midgardByteSize = encodeTransaction(midgardTx).length;
  const shape = buildShapeMetadata(midgardTx);

  const queuedTx: QueuedTx = {
    txId: CML.hash_transaction(cmlTx.body()).to_raw_bytes(),
    tx: midgardTx,
    arrivalSeq: 0n,
  };
  const phaseA = runPhaseAValidation([queuedTx], config.phaseAConfig);

  if (phaseA.rejected.length > 0) {
    const rejection = phaseA.rejected[0];
    return {
      computedTxIdHex,
      cborByteSize,
      midgardByteSize,
      validation: {
        status: "rejected",
        rejectCode: rejection.code,
        detail: rejection.detail,
      },
      shape,
    };
  }

  return {
    computedTxIdHex,
    cborByteSize,
    midgardByteSize,
    validation: { status: "accepted" },
    shape,
  };
}
