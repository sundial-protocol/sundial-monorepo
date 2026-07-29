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

// Converts a UTxO to a CML-compatible object whose to_cbor_bytes() serialises
// to a stable deterministic buffer.  Sufficient for ledger MPT put operations.
export const utxoToCore = (
  utxo: UTxO,
): {
  input: () => { to_cbor_bytes: () => Uint8Array };
  output: () => { to_cbor_bytes: () => Uint8Array };
} => ({
  input: () => ({
    to_cbor_bytes: () =>
      Buffer.from(
        JSON.stringify({ txHash: utxo.txHash, outputIndex: utxo.outputIndex }),
      ),
  }),
  output: () => ({
    to_cbor_bytes: () => Buffer.alloc(16, 0xcc),
  }),
});

export const coreToUtxo = (cml: { to_cbor_bytes: () => Uint8Array }): UTxO => {
  const bytes = Buffer.from(cml.to_cbor_bytes()).toString("utf8");
  try {
    const data = JSON.parse(bytes);
    return {
      txHash: data.txHash ?? "aa".repeat(32),
      outputIndex: data.outputIndex ?? 0,
      address: "",
      assets: {},
    };
  } catch {
    return { txHash: "aa".repeat(32), outputIndex: 0, address: "", assets: {} };
  }
};

export const CML = CMLReal;
