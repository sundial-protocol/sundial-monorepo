import { Writer, Reader, writeU64 } from "../../src/codec";
import {
  writeVKeyWitness,
  readVKeyWitness,
  writeMintStatic,
  writeMintDynamic,
  readMintStatic,
  readMintDynamic,
  encodeTransactionOutput,
  decodeTransactionOutput,
  encodeTransactionOutputCompact,
  decodeTransactionOutputCompact,
  type TransactionOutput,
  type TransactionOutputCompact,
} from "../../src/types/output";

import { bytesSeq } from "./helpers/cardano-fixtures.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const hash28A = new Uint8Array(28).fill(0xaa);
const hash32A = new Uint8Array(32).fill(0x11);
const hash32B = new Uint8Array(32).fill(0x22);
const hash32C = new Uint8Array(32).fill(0x33);
const vkeyA = new Uint8Array(32).fill(0x44);
const signatureA = new Uint8Array(64).fill(0x55);
const addressA = bytesSeq(29);
const datumA = bytesSeq(13);
const scriptRefA = bytesSeq(16);
const assetNameA = bytesSeq(5);

const coinOutputA: TransactionOutput = {
  address: addressA,
  value: { type: "Coin", coin: 2_000_000n },
  datum: undefined,
  script_ref: undefined,
};

const multiassetOutputA: TransactionOutput = {
  address: addressA,
  value: {
    type: "MultiAsset",
    coin: 1_000_000n,
    assets: [[hash28A, [[assetNameA, 500n]]]],
  },
  datum: undefined,
  script_ref: undefined,
};

it("VKey witness codec round trips", () => {
    const w = new Writer();
    writeVKeyWitness(w, { vkey: vkeyA, signature: signatureA });
    const encoded = w.toBytes();
    expect(encoded.length).toBe(96); // 32 (vkey) + 64 (signature) = 96 bytes
    const r = new Reader(encoded);
    const decoded = readVKeyWitness(r);
    expect(decoded.vkey).toEqual(vkeyA);
    expect(decoded.signature).toEqual(signatureA);
});

it("Coin transaction output round trips", () => {
    const encoded = encodeTransactionOutput(coinOutputA);
    const decoded = decodeTransactionOutput(encoded);
    expect(decoded.address).toEqual(addressA);
    expect(decoded.value.type).toBe("Coin");
    if (decoded.value.type === "Coin") expect(decoded.value.coin).toBe(2_000_000n);
    expect(decoded.datum).toBeUndefined();
    expect(decoded.script_ref).toBeUndefined();
});

it("Coin transaction output with datum and script reference round trips", () => {
    const input: TransactionOutput = {
      ...coinOutputA,
      datum: datumA,
      script_ref: scriptRefA,
    };
    const encoded = encodeTransactionOutput(input);
    const decoded = decodeTransactionOutput(encoded);
    expect(decoded.datum).toEqual(datumA);
    expect(decoded.script_ref).toEqual(scriptRefA);
});

it("Multiasset transaction output round trips", () => {
    const encoded = encodeTransactionOutput(multiassetOutputA);
    const decoded = decodeTransactionOutput(encoded);
    expect(decoded.value.type).toBe("MultiAsset");
    if (decoded.value.type === "MultiAsset") {
      expect(decoded.value.coin).toBe(1_000_000n);
      expect(decoded.value.assets.length).toBe(1);
      expect(decoded.value.assets[0][0]).toEqual(hash28A);
      const [[decodedName, decodedQty]] = decoded.value.assets[0][1];
      expect(decodedName).toEqual(assetNameA);
      expect(decodedQty).toBe(500n);
    }
});

it("Mint static and dynamic sections round trip", () => {
    const mint: [Uint8Array, [Uint8Array, bigint][]][] = [
      [hash28A, [[assetNameA, 10n]]],
      [new Uint8Array(28).fill(0xbb), [[bytesSeq(4), -3n]]],
    ];

    const staticW = new Writer();
    writeMintStatic(staticW, mint);
    const dynW = new Writer();
    writeMintDynamic(dynW, mint);

    const a = staticW.toBytes();
    const b = dynW.toBytes();
    const buf = new Uint8Array(a.length + b.length);
    buf.set(a);
    buf.set(b, a.length);

    const r = new Reader(buf);
    const partial = readMintStatic(r);
    const decoded = readMintDynamic(r, partial);
    expect(decoded.length).toBe(2);
    expect(decoded[0][1][0][1]).toBe(10n);   // minting quantity
    expect(decoded[1][1][0][1]).toBe(-3n);   // burning quantity
});

it("Compact coin output round trips", () => {
    const compact: TransactionOutputCompact = {
      address: addressA,
      value: { type: "Coin", coin: 1_000_000n },
      datum_hash: undefined,
      script_ref_hash: undefined,
    };
    const encoded = encodeTransactionOutputCompact(compact);
    const decoded = decodeTransactionOutputCompact(encoded);
    expect(decoded.value.type).toBe("Coin");
    if (decoded.value.type === "Coin") expect(decoded.value.coin).toBe(1_000_000n);
    expect(decoded.datum_hash).toBeUndefined();
    expect(decoded.script_ref_hash).toBeUndefined();
});

it("Compact multiasset output with hashes round trips", () => {
    const compact: TransactionOutputCompact = {
      address: addressA,
      value: { type: "MultiAsset", coin: 500_000n, hash: hash32A },
      datum_hash: hash32B,
      script_ref_hash: hash32C,
    };
    const encoded = encodeTransactionOutputCompact(compact);
    const decoded = decodeTransactionOutputCompact(encoded);
    expect(decoded.value.type).toBe("MultiAsset");
    if (decoded.value.type === "MultiAsset") {
      expect(decoded.value.coin).toBe(500_000n);
      expect(decoded.value.hash).toEqual(hash32A);
    }
    expect(decoded.datum_hash).toEqual(hash32B);
    expect(decoded.script_ref_hash).toEqual(hash32C);
});

// ---------------------------------------------------------------------------
// H-12: Bounds checking — length fields must not exceed remaining bytes
// ---------------------------------------------------------------------------

it("readMintStatic rejects outer count exceeding remaining bytes", () => {
    // 8 bytes claiming 1,000,000 outer policy entries — no actual entry data
    const w = new Writer();
    writeU64(w, 1_000_000);
    const r = new Reader(w.toBytes());
    expect(() => readMintStatic(r)).toThrow(/UnboundedLength/);
});

it("readMintStatic rejects inner asset count exceeding remaining bytes", () => {
    // outerLen=1 passes the outer check (40 bytes available for 1 entry),
    // but innerLen=1,000,000 fires the inner check with 0 bytes remaining
    const w = new Writer();
    writeU64(w, 1); // outerLen = 1
    w.write(new Uint8Array(28)); // pid raw bytes
    w.writeZeros(4); // alignment padding → 32 bytes total for pid
    writeU64(w, 1_000_000); // innerLen = huge
    // Total: 8 + 32 + 8 = 48 bytes; after reading outerLen+pid+innerLen, 0 remain
    const r = new Reader(w.toBytes());
    expect(() => readMintStatic(r)).toThrow(/UnboundedLength/);
});

// ---------------------------------------------------------------------------
// Discriminant-decode errors must report the bad value and its byte offset
// ---------------------------------------------------------------------------

it("decodeTransactionOutput reports the bad Value discriminant and its byte offset", () => {
    const encoded = encodeTransactionOutput(coinOutputA);
    // address len (8 bytes) precedes the value discriminant, which starts at offset 8
    encoded[15] = 9; // corrupt the low byte of the discriminant u64
    expect(() => decodeTransactionOutput(encoded)).toThrow(
      /UnknownDiscriminant for Value: got 9 at byte offset 8/,
    );
});

it("decodeTransactionOutputCompact reports the bad ValueCompact discriminant and its byte offset", () => {
    const compact: TransactionOutputCompact = {
      address: addressA,
      value: { type: "Coin", coin: 1_000_000n },
      datum_hash: undefined,
      script_ref_hash: undefined,
    };
    const encoded = encodeTransactionOutputCompact(compact);
    // address len (8 bytes) precedes the value discriminant, which starts at offset 8
    encoded[15] = 9; // corrupt the low byte of the discriminant u64
    expect(() => decodeTransactionOutputCompact(encoded)).toThrow(
      /UnknownDiscriminant for ValueCompact: got 9 at byte offset 8/,
    );
});
