import { CML, Data, OutRef, UTxO, valueToAssets } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { cmlInputToOutRef } from "./cml.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";

export const findSpentAndProducedUTxOs = (
  txCBOR: string
): { spent: OutRef[]; produced: UTxO[] } => {
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
      const output = outputs.get(i);
      produced.push({
        address: output.address().to_bech32(),
        assets: valueToAssets(output.amount()),
        datumHash: output.datum_hash()?.to_hex(),
        datum: output.datum()?.to_cbor_hex(),
        scriptRef: output.script_ref()?.to_cbor_hex(),
        txHash,
        outputIndex: i,
      } as UTxO);
    }
    return ({ spent, produced });
  } catch (_e) {
      throw Error("Something went wrong decoding the transaction")
  }
};

export const hashHeader = (header: Header): Effect.Effect<string, Error> =>
  hashHexWithBlake2b224(Data.to(header, Header));
