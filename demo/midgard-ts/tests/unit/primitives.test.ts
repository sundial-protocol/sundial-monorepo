import { Writer, Reader } from "../../src/codec";
import {
  writeHash28Static,
  readHash28Static,
  writeHash32Static,
  readHash32Static,
  encodeOutputReference,
  decodeOutputReference,
  encodeCredential,
  decodeCredential,
} from "../../src/types/primitives";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const hash28A = new Uint8Array(28).fill(0xaa);
const hash28B = new Uint8Array(28).fill(0xbb);
const hash32A = new Uint8Array(32).fill(0x11);

it("Hash28 static codec round trips", () => {
    const w = new Writer();
    writeHash28Static(w, hash28A);
    const encoded = w.toBytes();
    expect(encoded.length).toBe(32); // 28 bytes + 4 bytes alignment padding
    const r = new Reader(encoded);
    const decoded = readHash28Static(r);
    expect(decoded).toEqual(hash28A);
    expect(r.remaining()).toBe(0); // padding consumed
});

it("Hash32 static codec round trips", () => {
    const w = new Writer();
    writeHash32Static(w, hash32A);
    const encoded = w.toBytes();
    expect(encoded.length).toBe(32); // 32 bytes, already aligned — no padding
    const r = new Reader(encoded);
    const decoded = readHash32Static(r);
    expect(decoded).toEqual(hash32A);
    expect(r.remaining()).toBe(0);
});

it("Output reference codec round trips", () => {
    const outRefA = { tx_id: hash32A, index: 0 };
    const encoded = encodeOutputReference(outRefA);
    expect(encoded.length).toBe(40); // 32 (Hash32) + 8 (u16 padded) = 40
    const decoded = decodeOutputReference(encoded);
    expect(decoded.tx_id).toEqual(hash32A);
    expect(decoded.index).toBe(0);

    // Non-zero index
    const hash32B = new Uint8Array(32).fill(0x22);
    const outRefB = { tx_id: hash32B, index: 1 };
    const encoded2 = encodeOutputReference(outRefB);
    const decoded2 = decodeOutputReference(encoded2);
    expect(decoded2.tx_id).toEqual(hash32B);
    expect(decoded2.index).toBe(1);
});

it("PubKey credential codec round trips", () => {
    const cred = { type: "PubKey" as const, hash: hash28A };
    const encoded = encodeCredential(cred);
    expect(encoded.length).toBe(40); // 8 (u64 discriminant) + 32 (Hash28 + pad)
    const decoded = decodeCredential(encoded);
    expect(decoded.type).toBe("PubKey");
    expect(decoded.hash).toEqual(hash28A);
});

it("Script credential codec round trips", () => {
    const cred = { type: "Script" as const, hash: hash28B };
    const encoded = encodeCredential(cred);
    expect(encoded.length).toBe(40);
    const decoded = decodeCredential(encoded);
    expect(decoded.type).toBe("Script");
    expect(decoded.hash).toEqual(hash28B);
});
