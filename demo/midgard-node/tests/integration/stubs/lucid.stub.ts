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

// CML stubs — provide enough surface for transaction parsing paths.
const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
const outputCborBytes = Buffer.alloc(16, 0xcc);
const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]);

const mockTxHash = {
  to_raw_bytes: () => Buffer.alloc(32, 0xaa),
  to_hex: () => "aa".repeat(32),
};

const mockInputList = {
  len: () => 1,
  get: (_i: number) => ({ to_cbor_bytes: () => inputCborBytes }),
};

const mockOutputList = {
  len: () => 1,
  get: (_i: number) => ({
    to_cbor_bytes: () => outputCborBytes,
    address: () => ({
      to_bech32: () =>
        "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
    }),
  }),
};

const mockBody = {
  inputs: () => mockInputList,
  outputs: () => mockOutputList,
};

export const CML = {
  Transaction: {
    from_cbor_bytes: (_bytes: Uint8Array) => ({ body: () => mockBody }),
    new: (body: unknown, _witness: unknown, _bool: boolean) => ({
      body: () => body,
    }),
  },
  TransactionBody: {
    new: (inputs: unknown, outputs: unknown, fee: bigint) => ({
      inputs: () => inputs,
      outputs: () => outputs,
      fee: () => fee,
    }),
  },
  TransactionWitnessSet: {
    new: () => ({}),
  },
  TransactionInputList: {
    new: () => {
      const items: unknown[] = [];
      return {
        add: (item: unknown) => items.push(item),
        len: () => items.length,
        get: (i: number) => items[i],
      };
    },
  },
  TransactionOutputList: {
    new: () => {
      const items: unknown[] = [];
      return {
        add: (item: unknown) => items.push(item),
        len: () => items.length,
        get: (i: number) => items[i],
      };
    },
  },
  TransactionInput: {
    new: (_hash: unknown, _idx: bigint) => ({
      to_cbor_bytes: () => outrefCborBytes,
    }),
    from_cbor_bytes: (bytes: Buffer) => ({
      transaction_id: () => ({
        to_hex: () => Buffer.from(bytes).toString("hex").substring(0, 64),
      }),
      index: () => BigInt(0),
    }),
  },
  TransactionUnspentOutput: {
    from_cbor_bytes: (bytes: Buffer) => ({
      to_cbor_bytes: () => bytes,
    }),
  },
  hash_transaction: () => mockTxHash,
};
