import * as CMLReal from "../../../../node_modules/.pnpm/node_modules/@anastasia-labs/cardano-multiplatform-lib-nodejs/cardano_multiplatform_lib.js";

// Integration stub for @lucid-evolution/lucid.
//
// Unlike the unit stub (which throws on every call), this stub returns
// deterministic in-memory fixtures so that integration tests can exercise the
// node's parsing, repository, and MPT layers without contacting a live Cardano
// node.
//
// Tests that need to verify HTTP request/response decoding should swap this
// stub for a localhost Blockfrost/Kupmios HTTP server as described in the test
// plan §External Dependency Replacement Matrix.
//
// Deterministic fixture bytes used across the integration suite:
//   txId   : 32 × 0xaa
//   outRef : CML input CBOR of (txId#0)
//   output : 16 × 0xcc (minimal CBOR output placeholder)

export type UTxO = {
  txHash: string;
  outputIndex: number;
  address: string;
  assets: Record<string, bigint>;
  datum?: string;
  datumHash?: string;
  scriptRef?: unknown;
};

export const toHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const fromHex = (hex: string): Uint8Array => Buffer.from(hex, "hex");

// Converts a UTxO to a real CML.TransactionUnspentOutput, mirroring real
// Lucid's `utxoToCore`: the output is built via the legacy (pre-Conway)
// constructor, since that's what real Lucid emits for simple address+value
// UTxOs. Callers that persist this to the ledger are expected to re-encode it
// to Conway format themselves (see `toConwayFormatOutputCbor`). Returning a
// real TransactionUnspentOutput (rather than a plain object) matches the real
// library's shape: it natively supports `.input()`, `.output()`, and
// `.to_cbor_bytes()`, all of which callers in `src/` rely on.
export const utxoToCore = (utxo: UTxO): CMLReal.TransactionUnspentOutput => {
  const input = CMLReal.TransactionInput.new(
    CMLReal.TransactionHash.from_hex(utxo.txHash),
    BigInt(utxo.outputIndex),
  );
  const address = CMLReal.Address.from_bech32(utxo.address);
  const lovelace = utxo.assets.lovelace ?? 0n;
  const output = CMLReal.TransactionOutput.new(
    address,
    CMLReal.Value.from_coin(lovelace),
  );
  return CMLReal.TransactionUnspentOutput.new(input, output);
};

export const coreToUtxo = (cml: CMLReal.TransactionUnspentOutput): UTxO => {
  const input = cml.input();
  const output = cml.output();
  return {
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
    address: output.address().to_bech32(),
    assets: { lovelace: output.amount().coin() },
  };
};

export const CML = CMLReal;
