import * as Ledger from "@/database/utils/ledger.js";
import type { ProcessedTx } from "@/utils.js";

export const TxParseWorkerMessageType = {
  ParseTx: "ParseTx",
  ParseTxResult: "ParseTxResult",
} as const;

export type SerializedLedgerEntry = {
  txIdHex: string;
  outRefHex: string;
  outputHex: string;
  address: string;
};

export type SerializedProcessedTx = {
  txIdHex: string;
  txCborHex: string;
  spentHex: string[];
  produced: SerializedLedgerEntry[];
};

export type ParseTxRequestMessage = {
  type: typeof TxParseWorkerMessageType.ParseTx;
  requestId: number;
  txCborHex: string;
};

export type ParseTxSuccessMessage = {
  type: typeof TxParseWorkerMessageType.ParseTxResult;
  requestId: number;
  outcome: "success";
  parsed: SerializedProcessedTx;
};

export type ParseTxFailureMessage = {
  type: typeof TxParseWorkerMessageType.ParseTxResult;
  requestId: number;
  outcome: "failure";
  error: string;
};

export type ParseTxResponseMessage =
  | ParseTxSuccessMessage
  | ParseTxFailureMessage;

export const serializeProcessedTx = (
  processedTx: ProcessedTx,
): SerializedProcessedTx => ({
  txIdHex: processedTx.txId.toString("hex"),
  txCborHex: processedTx.txCbor.toString("hex"),
  spentHex: processedTx.spent.map((outRef) => outRef.toString("hex")),
  produced: processedTx.produced.map((entry) => ({
    txIdHex: entry[Ledger.Columns.TX_ID].toString("hex"),
    outRefHex: entry[Ledger.Columns.OUTREF].toString("hex"),
    outputHex: entry[Ledger.Columns.OUTPUT].toString("hex"),
    address: entry[Ledger.Columns.ADDRESS],
  })),
});

export const deserializeProcessedTx = (
  serialized: SerializedProcessedTx,
): ProcessedTx => ({
  txId: Buffer.from(serialized.txIdHex, "hex"),
  txCbor: Buffer.from(serialized.txCborHex, "hex"),
  spent: serialized.spentHex.map((outRefHex) => Buffer.from(outRefHex, "hex")),
  produced: serialized.produced.map((entry) => ({
    [Ledger.Columns.TX_ID]: Buffer.from(entry.txIdHex, "hex"),
    [Ledger.Columns.OUTREF]: Buffer.from(entry.outRefHex, "hex"),
    [Ledger.Columns.OUTPUT]: Buffer.from(entry.outputHex, "hex"),
    [Ledger.Columns.ADDRESS]: entry.address,
  })),
});
