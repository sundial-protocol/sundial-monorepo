import { CML, Data, OutRef } from "@lucid-evolution/lucid";
import { Either } from "effect";
import { cmlInputToOutRef } from "./cml.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";

export const findSpentAndProducedUTxOs = (
  txCBOR: string
): Either.Either<{ spent: OutRef[]; produced: OutRef[] }, string> => {
  try {
    const tx = CML.Transaction.from_cbor_hex(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    const spent = [];
    for (let i = 0; i < inputsCount; i++) {
      const input = inputs.get(i);
      spent.push(cmlInputToOutRef(input));
    }
    const txHash = CML.hash_transaction(txBody).to_hex();
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced = [];
    for (let i = 0; i < outputsCount; i++) {
      produced.push({ txHash, outputIndex: i });
    }
    return Either.right({ spent, produced });
  } catch (_e) {
    return Either.left("Something went wrong decoding the transaction");
  }
};

export const hashHeader = (header: Header): Either.Either<string, string> =>
  hashHexWithBlake2b224(Data.to(header, Header));
