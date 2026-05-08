import {
  ALIGN,
  alignmentBytes,
  alignedSize,
  Writer,
  Reader,
  writeU64,
  readU64,
  writeBigU64,
  readBigU64,
  writeBigI64,
  readBigI64,
  writeU16,
  readU16,
  writeU8,
  readU8,
  writeBool,
  readBool,
  writeFixedBytes,
  readFixedBytes,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
} from "../../src/codec";

import { bytesSeq } from "./helpers/cardano-fixtures.js";

// ---------------------------------------------------------------------------
// Test fixtures
// ---------------------------------------------------------------------------

function bytes(fill: number, len: number): Uint8Array {
  return new Uint8Array(len).fill(fill);
}

const hash28A = bytes(0xaa, 28);

it("Alignment constants and helpers match 8-byte boundary", () => {
    expect(ALIGN).toBe(8);
    for (const n of [0, 1, 7, 8, 9, 15, 16]) {
      expect(alignedSize(n) % ALIGN).toBe(0);
    }
    expect(alignmentBytes(0)).toBe(0);
    expect(alignmentBytes(1)).toBe(7);
    expect(alignmentBytes(7)).toBe(1);
    expect(alignmentBytes(8)).toBe(0);
    expect(alignmentBytes(9)).toBe(7);
});

it("Writer preserves chunk order", () => {
    const w = new Writer();
    w.write(new Uint8Array([0x01, 0x02]));
    w.pushByte(0x03);
    w.write(new Uint8Array([0x04]));
    const result = w.toBytes();
    expect(result).toEqual(new Uint8Array([1, 2, 3, 4]));
    expect(result.length).toBe(4);
});

it("Writer skips empty chunks without changing output", () => {
    const w = new Writer();
    w.write(new Uint8Array(0));
    w.write(new Uint8Array([0xaa]));
    w.write(new Uint8Array(0));
    const result = w.toBytes();
    expect(result).toEqual(new Uint8Array([0xaa]));
    expect(result.length).toBe(1);
});

it("Reader reads sequential slices", () => {
    const r = new Reader(bytesSeq(4));
    const first = r.read(2);
    expect(first).toEqual(new Uint8Array([0, 1]));
    const second = r.read(2);
    expect(second).toEqual(new Uint8Array([2, 3]));
    expect(r.remaining()).toBe(0);
});

it("Reader skip advances position", () => {
    const r = new Reader(bytesSeq(4));
    r.skip(2);
    const slice = r.read(2);
    expect(slice).toEqual(new Uint8Array([2, 3]));
    expect(r.remaining()).toBe(0);
});

it("u64 round trips", () => {
    const input = 4_294_967_297; // 2^32 + 1, above 32-bit range
    const w = new Writer();
    writeU64(w, input);
    expect(w.toBytes().length).toBe(8);
    const r = new Reader(w.toBytes());
    const decoded = readU64(r);
    expect(decoded).toBe(input);
});

it("bigint u64 round trips", () => {
    const input = 9_000_000_000n;
    const w = new Writer();
    writeBigU64(w, input);
    expect(w.toBytes().length).toBe(8);
    const r = new Reader(w.toBytes());
    const decoded = readBigU64(r);
    expect(decoded).toBe(input);
});

it("bigint i64 round trips positive and negative amounts", () => {
    for (const value of [123n, -123n]) {
      const w = new Writer();
      writeBigI64(w, value);
      const r = new Reader(w.toBytes());
      expect(readBigI64(r)).toBe(value);
    }
});

it("u16 round trip uses padded static encoding", () => {
    const input = 513;
    const w = new Writer();
    writeU16(w, input);
    expect(w.toBytes().length).toBe(8); // 6 zero bytes + 2 data bytes
    const r = new Reader(w.toBytes());
    expect(readU16(r)).toBe(input);
});

it("u8 and bool helpers round trip", () => {
    const w = new Writer();
    writeU8(w, 7);
    expect(w.toBytes().length).toBe(8); // 7 zero bytes + 1 data byte
    expect(readU8(new Reader(w.toBytes()))).toBe(7);

    const wt = new Writer();
    writeBool(wt, true);
    expect(readBool(new Reader(wt.toBytes()))).toBe(true);

    const wf = new Writer();
    writeBool(wf, false);
    expect(readBool(new Reader(wf.toBytes()))).toBe(false);
});

it("Fixed bytes round trip with padding", () => {
    const w = new Writer();
    writeFixedBytes(w, hash28A);
    expect(w.toBytes().length).toBe(32); // 28 bytes + 4 bytes padding to align to 8
    const r = new Reader(w.toBytes());
    const decoded = readFixedBytes(r, 28);
    expect(decoded).toEqual(hash28A);
    expect(r.remaining()).toBe(0); // padding was consumed
});

it("Variable bytes static and dynamic sections round trip", () => {
    const data = bytesSeq(13);

    const staticW = new Writer();
    writeVarBytesStatic(staticW, data); // writes length 13 as u64 (8 bytes)

    const dynW = new Writer();
    writeVarBytesDynamic(dynW, data); // writes 13 bytes + 3 bytes pad = 16 bytes

    const a = staticW.toBytes();
    const b = dynW.toBytes();
    const buf = new Uint8Array(a.length + b.length);
    buf.set(a);
    buf.set(b, a.length);

    const r = new Reader(buf);
    const len = readVarBytesLen(r);
    expect(len).toBe(13);
    const decoded = readVarBytesDynamic(r, len);
    expect(decoded).toEqual(data);
    expect(r.remaining()).toBe(0);
});
