import { parentPort, workerData } from "worker_threads";
import { CML } from "@lucid-evolution/lucid";
import * as chalk_ from "chalk";
import { Data, Effect, pipe } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import * as SDK from "@al-ft/midgard-sdk";

export type ProcessedTx = {
  txId: Buffer;
  txCbor: Buffer;
  spent: Buffer[];
  produced: Ledger.Entry[];
};

// For some reason importing these directly into the new confirmation worker
// failed. This is probably a temporary workaround.
export const reexportedParentPort = parentPort;
export const reexportedWorkerData = workerData;

export const chalk = new chalk_.Chalk();

export type ProviderName = "Blockfrost" | "Koios" | "Kupmios" | "Maestro";

export const logSuccess = (msg: string) => {
  Effect.runSync(Effect.logInfo(`🎉 ${msg}`));
};

export const logWarning = (msg: string) => {
  Effect.runSync(Effect.logWarning(`⚠️  ${msg}`));
};

export const logAbort = (msg: string) => {
  Effect.runSync(Effect.logError(msg));
};

export const logInfo = (msg: string) => {
  Effect.runSync(Effect.logInfo(`ℹ️  ${msg}`));
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  // Odd-length input can't decode to whole bytes; fromHex() throws
  // synchronously on it, so reject it here before it reaches any decoder.
  return str.length % 2 === 0 && hexRegex.test(str);
};

// Midgard only supports Conway-format (post-Alonzo) transaction outputs, and
// the mempool acceptance validator decodes the ledger pre-state with that
// invariant. `CML.TransactionOutput.new` builds the legacy (pre-Conway)
// format, so any code constructing an output that will be persisted to the
// ledger must go through this constructor instead.
export const newConwayFormatTxOutput = (
  address: CML.Address,
  value: CML.Value,
  datumOption?: CML.DatumOption,
): CML.TransactionOutput => {
  const conway = CML.ConwayFormatTxOut.new(address, value);
  if (datumOption !== undefined) {
    conway.set_datum_option(datumOption);
  }
  return CML.TransactionOutput.new_conway_format_tx_out(conway);
};

// Same invariant as `newConwayFormatTxOutput`, but for re-encoding an output
// that may already be in either format (e.g. one produced by Lucid's
// `utxoToCore`, which emits legacy-format outputs for simple address+value
// UTxOs) rather than one being constructed from scratch.
export const toConwayFormatOutputCbor = (
  output: CML.TransactionOutput,
): Uint8Array => {
  if (output.kind() === CML.TransactionOutputKind.ConwayFormatTxOut) {
    return output.to_cbor_bytes();
  }
  const conway = CML.ConwayFormatTxOut.new(output.address(), output.amount());
  const datum = output.datum();
  if (datum !== undefined) {
    conway.set_datum_option(datum);
  }
  const scriptRef = output.script_ref();
  if (scriptRef !== undefined) {
    conway.set_script_reference(scriptRef);
  }
  return CML.TransactionOutput.new_conway_format_tx_out(conway).to_cbor_bytes();
};

export const breakDownTxMinimally = (
  txCBOR: Buffer,
  txHash?: Buffer,
): Effect.Effect<
  { spent: Buffer[]; produced: Ledger.MinimalEntry[] },
  SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const spent: Buffer[] = [];
    const produced: Ledger.MinimalEntry[] = [];
    const tx = CML.Transaction.from_cbor_bytes(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const outputs = txBody.outputs();
    const inputsCount = inputs.len();
    const outputsCount = outputs.len();
    for (let i = 0; i < inputsCount; i++) {
      yield* Effect.try({
        try: () => spent.push(Buffer.from(inputs.get(i).to_cbor_bytes())),
        catch: (e) =>
          new SDK.CmlUnexpectedError({
            message: `An error occurred on input CBOR serialization`,
            cause: e,
          }),
      });
    }
    const finalTxHash =
      txHash === undefined
        ? CML.hash_transaction(txBody).to_raw_bytes()
        : txHash;
    for (let i = 0; i < outputsCount; i++) {
      produced.push({
        [Ledger.Columns.OUTREF]: Buffer.from(finalTxHash),
        [Ledger.Columns.OUTPUT]: Buffer.from(outputs.get(i).to_cbor_bytes()),
      });
    }
    return { spent, produced };
  });

/**
 * Given a transaction CBOR bytes, this function breaks it down into its spent
 * outrefs and produced outputs (as ledger entries).
 *
 * @param txCbor - Uint8Array of the CBOR encoded transaction itself (expected to be deserializable to a CML.Transaction).
 * @returns An effect that can be reduced to a `ProcessedTx`.
 */
export const breakDownTx = (
  txCbor: Uint8Array,
): Effect.Effect<ProcessedTx, SDK.CmlDeserializationError> =>
  Effect.gen(function* () {
    const deserializedTx = yield* Effect.try({
      try: () => CML.Transaction.from_cbor_bytes(txCbor),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to deserialize transaction: ${txCbor}`,
          cause: e,
        }),
    });
    const txBody = deserializedTx.body();
    const txHash = CML.hash_transaction(txBody);
    const txHashBytes = Buffer.from(txHash.to_raw_bytes());
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    const spent: Buffer[] = [];
    for (let i = 0; i < inputsCount; i++) {
      spent.push(Buffer.from(inputs.get(i).to_cbor_bytes()));
    }
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced: Ledger.Entry[] = [];
    for (let i = 0; i < outputsCount; i++) {
      const output = outputs.get(i);
      produced.push({
        [Ledger.Columns.TX_ID]: txHashBytes,
        [Ledger.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(toConwayFormatOutputCbor(output)),
        [Ledger.Columns.ADDRESS]: output.address().to_bech32(),
      });
    }
    return {
      txId: txHashBytes,
      txCbor: Buffer.from(txCbor),
      spent: spent,
      produced: produced,
    };
  });

/**
 * Given a batch size and a total count, the required continuation will be
 * provided with start and end indices.
 *
 * @param batchSize - Size of each batch
 * @param totalCount - Total count of the iterable meant to be batched
 * @param opName - A name to make logs more readable (doesn't affect the logic)
 * @param effectMaker - A continuation that is provided with starting and ending indices for each batch.
 */
export const batchProgram = <A, E, C>(
  batchSize: number,
  totalCount: number,
  opName: string,
  effectMaker: (startIndex: number, endIndex: number) => Effect.Effect<A, E, C>,
  concurrencyOverride?: number,
) => {
  const batchIndices = Array.from(
    { length: Math.ceil(totalCount / batchSize) },
    (_, i) => i * batchSize,
  );
  return Effect.forEach(
    batchIndices,
    (startIndex) => {
      const endIndex = startIndex + batchSize;
      return pipe(
        effectMaker(startIndex, endIndex),
        Effect.withSpan(`batch-${opName}-${startIndex}-${endIndex}`),
      );
    },
    { concurrency: concurrencyOverride ?? "unbounded" },
  );
};

export const trivialTransactionFromCMLUnspentOutput = (
  transactionUnspentOutput: CML.TransactionUnspentOutput,
): Effect.Effect<CML.Transaction, never, never> =>
  Effect.gen(function* () {
    const inputs = CML.TransactionInputList.new();
    const outputs = CML.TransactionOutputList.new();
    outputs.add(transactionUnspentOutput.output());
    const fee = 0n;

    const transactionBody = CML.TransactionBody.new(inputs, outputs, fee);
    const witnessSet = CML.TransactionWitnessSet.new();

    const transaction = CML.Transaction.new(transactionBody, witnessSet, true);
    return transaction;
  });

export const ENV_VARS_GUIDE = `
Make sure you first have set the environment variable for your seed phrase:

\u0009${chalk.bold("SEED_PHRASE")}\u0009 Your wallet's seed phrase

Depending on which provider you'll be using, other environment variables may also be needed:

Blockfrost or Maestro:
\u0009${chalk.bold("API_KEY")}    \u0009 Your provider's API key

Kupmios:
\u0009${chalk.bold("KUPO_URL")}   \u0009 URL of your Kupo instance
\u0009${chalk.bold("OGMIOS_URL")} \u0009 URL of your Ogmios instance
`;

export class FileSystemError extends Data.TaggedError(
  "FileSystemError",
)<SDK.GenericErrorFields> {}
