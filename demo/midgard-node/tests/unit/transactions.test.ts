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
import { CML } from "@lucid-evolution/lucid";

const mockUnspentOutput = {
  output: () => ({ to_cbor_bytes: () => outputCborBytes }),
};

it.effect("trivialTransactionFromCMLUnspentOutput wraps output in body", () =>
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

describe("breakDownTx", () => {
  it.effect("extracts txId, spent, and produced from valid CBOR", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTx(txCborA);
      expect(result.txId).toBeInstanceOf(Buffer);
      expect(result.txId.length).toBe(32);
      expect(result.txCbor).toEqual(txCborA);
      expect(result.spent.length).toBe(1);
      expect(result.produced.length).toBe(1);
    }),
  );

  it.effect("produced ledger entries include output address", () =>
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

  it.effect("fails on deserialization error", () => {
    vi.mocked(CML.Transaction.from_cbor_bytes).mockImplementationOnce(() => {
      throw new Error("bad cbor bytes");
    });
    return breakDownTx(txCborA).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(exit._tag).toBe("Failure");
      }),
    );
  });
});

describe("breakDownTxMinimally", () => {
  it.effect("uses the supplied hash for produced outputs", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTxMinimally(txCborA, txIdABytes);
      expect(result.produced.length).toBe(1);
      const produced = result.produced[0] as any;
      expect(produced.outref).toBeInstanceOf(Buffer);
    }),
  );

  it.effect("falls back to CML.hash_transaction when no hash supplied", () =>
    Effect.gen(function* () {
      const result = yield* breakDownTxMinimally(txCborA);
      expect(result.spent.length).toBe(1);
      expect(result.produced.length).toBe(1);
      const produced = result.produced[0] as any;
      expect(produced.outref).toBeInstanceOf(Buffer);
    }),
  );

  it.effect("fails when input CBOR serialization throws", () => {
    vi.mocked(CML.Transaction.from_cbor_bytes).mockReturnValueOnce({
      body: () => ({
        inputs: () => ({
          len: () => 1,
          get: () => ({
            to_cbor_bytes: () => {
              throw new Error("bad input cbor");
            },
          }),
        }),
        outputs: () => ({ len: () => 0 }),
      }),
    } as any);
    return breakDownTxMinimally(txCborA, txIdABytes).pipe(
      Effect.exit,
      Effect.map((exit) => {
        expect(exit._tag).toBe("Failure");
      }),
    );
  });
});

describe("UTxO storage serialization", () => {
  it.effect("round trips a list of UTxOs through Buffer storage", () =>
    Effect.gen(function* () {
      const buf = yield* serializeUTxOsForStorage([utxoA as any, utxoB as any]);
      expect(buf).toBeInstanceOf(Buffer);
      expect(buf.length).toBeGreaterThan(0);
      const deserialized = yield* deserializeUTxOsFromStorage(buf);
      expect(deserialized.length).toBe(2);
      const hashes = deserialized.map((u) => u.txHash);
      expect(hashes).toContain(utxoA.txHash);
      expect(hashes).toContain(utxoB.txHash);
    }),
  );
});
