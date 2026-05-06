import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

// Deterministic fixtures
const txIdABytes = Buffer.alloc(32, 0xaa);
const txCborA = Buffer.alloc(64, 0xcc);
const inputCborBytes = Buffer.from([0x82, 0x01, 0x02]);
const outputCborBytes = Buffer.from([0x82, 0x03, 0x04]);
const outrefCborBytes = Buffer.from([0x82, 0xab, 0xcd]);
const testAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const utxoA = {
  txHash: "aa".repeat(32),
  outputIndex: 0,
  address: testAddress,
  assets: {},
};
const utxoB = {
  txHash: "bb".repeat(32),
  outputIndex: 1,
  address: testAddress,
  assets: {},
};

vi.mock("@lucid-evolution/lucid", () => {
  const mockInputList = {
    len: () => 1,
    get: (_i: number) => ({ to_cbor_bytes: () => inputCborBytes }),
  };
  const mockOutputList = {
    len: () => 1,
    get: (_i: number) => ({
      to_cbor_bytes: () => outputCborBytes,
      address: () => ({ to_bech32: () => testAddress }),
    }),
  };
  const mockBody = {
    inputs: () => mockInputList,
    outputs: () => mockOutputList,
  };
  const mockTxHash = {
    to_raw_bytes: () => Buffer.alloc(32, 0xaa),
    to_hex: () => "aa".repeat(32),
  };

  return {
    CML: {
      Transaction: {
        from_cbor_bytes: vi.fn((_bytes: Uint8Array) => ({
          body: () => mockBody,
        })),
        new: vi.fn((body: any, _witness: any, _bool: boolean) => ({
          body: () => body,
        })),
      },
      TransactionBody: {
        new: vi.fn((inputs: any, outputs: any, fee: bigint) => ({
          inputs: () => inputs,
          outputs: () => outputs,
          fee: () => fee,
        })),
      },
      TransactionWitnessSet: {
        new: vi.fn(() => ({})),
      },
      TransactionInputList: {
        new: vi.fn(() => {
          const items: any[] = [];
          return {
            add: (item: any) => items.push(item),
            len: () => items.length,
            get: (i: number) => items[i],
          };
        }),
      },
      TransactionOutputList: {
        new: vi.fn(() => {
          const items: any[] = [];
          return {
            add: (item: any) => items.push(item),
            len: () => items.length,
            get: (i: number) => items[i],
          };
        }),
      },
      TransactionInput: {
        new: vi.fn((_hash: any, _idx: bigint) => ({
          to_cbor_bytes: () => outrefCborBytes,
        })),
        from_cbor_bytes: vi.fn((bytes: Buffer) => ({
          transaction_id: () => ({
            to_hex: () => Buffer.from(bytes).toString("hex").substring(0, 64),
          }),
          index: () => BigInt(0),
        })),
      },
      hash_transaction: vi.fn(() => mockTxHash),
      TransactionUnspentOutput: {
        from_cbor_bytes: vi.fn((bytes: Buffer) => ({
          to_cbor_bytes: () => bytes,
        })),
      },
    },
    utxoToCore: vi.fn((utxo: any) => ({
      to_cbor_bytes: () =>
        Buffer.from(
          JSON.stringify({
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          }),
        ),
    })),
    coreToUtxo: vi.fn((cml: any) => {
      const data = JSON.parse(
        Buffer.from(cml.to_cbor_bytes()).toString("utf8"),
      );
      return {
        txHash: data.txHash,
        outputIndex: data.outputIndex,
        address: "",
        assets: {},
      };
    }),
  };
});

import {
  trivialTransactionFromCMLUnspentOutput,
  breakDownTx,
  breakDownTxMinimally,
} from "@/utils.js";
import {
  serializeUTxOsForStorage,
  deserializeUTxOsFromStorage,
} from "@/database/utils/common.js";

const mockUnspentOutput = {
  output: () => ({ to_cbor_bytes: () => outputCborBytes }),
};

describe("Convert trivial UTxO to transaction", () => {
  it.effect("Convert trivial UTxO to transaction", () =>
    Effect.gen(function* () {
      const tx = yield* trivialTransactionFromCMLUnspentOutput(
        mockUnspentOutput as any,
      );
      const body = tx.body();
      expect(body.inputs().len()).toBe(0);
      expect(body.outputs().len()).toBe(1);
      expect(body.fee()).toBe(0n);
    }),
  );
});

describe("Break down valid transaction", () => {
  it.effect("Break down valid transaction", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTx(txCborA);
      expect(result.txId).toBeInstanceOf(Buffer);
      expect(result.txId.length).toBe(32);
      expect(result.txCbor).toEqual(txCborA);
      expect(result.spent.length).toBe(1);
      expect(result.produced.length).toBe(1);
    }),
  );
});

describe("Produced ledger entries include output addresses", () => {
  it.effect("Produced ledger entries include output addresses", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTx(txCborA);
      const entry = result.produced[0] as any;
      expect(entry).toHaveProperty("tx_id");
      expect(entry).toHaveProperty("outref");
      expect(entry).toHaveProperty("output");
      expect(entry).toHaveProperty("address");
      expect(typeof entry.address).toBe("string");
    }),
  );
});

describe("Minimal transaction breakdown uses supplied hash", () => {
  it.effect("Minimal transaction breakdown uses supplied hash", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTxMinimally(txCborA, txIdABytes);
      expect(result.produced.length).toBe(1);
      const produced = result.produced[0] as any;
      expect(produced.outref).toBeInstanceOf(Buffer);
    }),
  );
});

describe("Serialize UTxOs for storage", () => {
  it.effect("Serialize UTxOs for storage", () =>
    Effect.gen(function* () {
      const buf = yield* serializeUTxOsForStorage([utxoA as any, utxoB as any]);
      expect(buf).toBeInstanceOf(Buffer);
      expect(buf.length).toBeGreaterThan(0);
    }),
  );
});

describe("Deserialize UTxOs from storage", () => {
  it.effect("Deserialize UTxOs from storage", () =>
    Effect.gen(function* () {
      const serialized = yield* serializeUTxOsForStorage([
        utxoA as any,
        utxoB as any,
      ]);
      const deserialized = yield* deserializeUTxOsFromStorage(serialized);
      expect(deserialized.length).toBe(2);
      const hashes = deserialized.map((u) => u.txHash);
      expect(hashes).toContain(utxoA.txHash);
      expect(hashes).toContain(utxoB.txHash);
    }),
  );
});
