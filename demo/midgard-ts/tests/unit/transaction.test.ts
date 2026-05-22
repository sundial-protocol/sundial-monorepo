import { Writer, writeU64 } from "../../src/codec";

import {
  encodeTransactionWitnessSetCompact,
  decodeTransactionWitnessSetCompact,
  encodeTransactionBody,
  decodeTransactionBody,
  encodeTransactionBodyCompact,
  decodeTransactionBodyCompact,
  encodeTransaction,
  decodeTransaction,
  encodeTransactionCompact,
  decodeTransactionCompact,
  type TransactionBody,
  type Transaction,
  type TransactionBodyCompact,
  type TransactionWitnessSetCompact,
  type TransactionCompact,
} from "../../src/types/transaction";

import { bytesSeq } from "./helpers/cardano-fixtures.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const hash32A = new Uint8Array(32).fill(0x11);
const hash32B = new Uint8Array(32).fill(0x22);
const hash32C = new Uint8Array(32).fill(0x33);
const hash32D = new Uint8Array(32).fill(0x44);
const hash28A = new Uint8Array(28).fill(0xaa);
const addressA = bytesSeq(29);

const minimalWitnessSet = {
  vkey_witnesses: undefined,
  native_scripts: undefined,
  redeemers: undefined,
  plutus_v3_scripts: undefined,
};

const minimalTransactionBody: TransactionBody = {
  inputs: [{ tx_id: hash32A, index: 0 }],
  outputs: [
    {
      address: addressA,
      value: { type: "Coin", coin: 2_000_000n },
      datum: undefined,
      script_ref: undefined,
    },
  ],
  fee: 170_000n,
  ttl: undefined,
  auxiliary_data_hash: undefined,
  validity_interval_start: undefined,
  mint: undefined,
  script_data_hash: undefined,
  required_signers: undefined,
  network_id: undefined,
  reference_inputs: undefined,
  required_observers: undefined,
};

const minimalTransaction: Transaction = {
  body: minimalTransactionBody,
  witness_set: minimalWitnessSet,
  is_valid: true,
};

it("Witness set compact with all fields absent round trips", () => {
    const compact: TransactionWitnessSetCompact = {
      vkey_witnesses_hash: undefined,
      native_scripts_hash: undefined,
      redeemers_hash: undefined,
      plutus_v3_scripts_hash: undefined,
    };
    const encoded = encodeTransactionWitnessSetCompact(compact);
    const decoded = decodeTransactionWitnessSetCompact(encoded);
    expect(decoded.vkey_witnesses_hash).toBeUndefined();
    expect(decoded.native_scripts_hash).toBeUndefined();
    expect(decoded.redeemers_hash).toBeUndefined();
    expect(decoded.plutus_v3_scripts_hash).toBeUndefined();
});

it("Witness set compact with all fields present round trips", () => {
    const compact: TransactionWitnessSetCompact = {
      vkey_witnesses_hash: hash32A,
      native_scripts_hash: hash32B,
      redeemers_hash: hash32C,
      plutus_v3_scripts_hash: hash32D,
    };
    const encoded = encodeTransactionWitnessSetCompact(compact);
    const decoded = decodeTransactionWitnessSetCompact(encoded);
    expect(decoded.vkey_witnesses_hash).toEqual(hash32A);
    expect(decoded.native_scripts_hash).toEqual(hash32B);
    expect(decoded.redeemers_hash).toEqual(hash32C);
    expect(decoded.plutus_v3_scripts_hash).toEqual(hash32D);
});

it("Minimal transaction body round trips", () => {
    const encoded = encodeTransactionBody(minimalTransactionBody);
    const decoded = decodeTransactionBody(encoded);
    expect(decoded.inputs.length).toBe(1);
    expect(decoded.inputs[0].tx_id).toEqual(hash32A);
    expect(decoded.inputs[0].index).toBe(0);
    expect(decoded.outputs.length).toBe(1);
    expect(decoded.fee).toBe(170_000n);
    expect(decoded.ttl).toBeUndefined();
    expect(decoded.mint).toBeUndefined();
    expect(decoded.required_signers).toBeUndefined();
});

it("Transaction body with optional fields round trips", () => {
    const body: TransactionBody = {
      ...minimalTransactionBody,
      ttl: 9_999,
      validity_interval_start: 1_000,
      required_signers: [hash28A],
      network_id: 1,
      reference_inputs: [{ tx_id: hash32B, index: 0 }],
      required_observers: [hash28A],
    };
    const encoded = encodeTransactionBody(body);
    const decoded = decodeTransactionBody(encoded);
    expect(decoded.ttl).toBe(9_999);
    expect(decoded.validity_interval_start).toBe(1_000);
    expect(decoded.required_signers?.length).toBe(1);
    expect(decoded.network_id).toBe(1);
    expect(decoded.reference_inputs?.length).toBe(1);
    expect(decoded.required_observers?.length).toBe(1);
});

it("Transaction body compact with no optional hashes round trips", () => {
    const compact: TransactionBodyCompact = {
      inputs_hash: hash32A,
      outputs_hash: hash32B,
      fee: 170_000n,
      ttl: undefined,
      auxiliary_data_hash: undefined,
      validity_interval_start: undefined,
      mint_hash: undefined,
      script_data_hash: undefined,
      required_signers_hash: undefined,
      network_id: undefined,
      reference_inputs_hash: undefined,
      required_observers_hash: undefined,
    };
    const encoded = encodeTransactionBodyCompact(compact);
    const decoded = decodeTransactionBodyCompact(encoded);
    expect(decoded.inputs_hash).toEqual(hash32A);
    expect(decoded.outputs_hash).toEqual(hash32B);
    expect(decoded.fee).toBe(170_000n);
    expect(decoded.ttl).toBeUndefined();
    expect(decoded.mint_hash).toBeUndefined();
});

it("Transaction body compact with optional hashes round trips", () => {
    const compact: TransactionBodyCompact = {
      inputs_hash: hash32A,
      outputs_hash: hash32B,
      fee: 1_000_000n,
      ttl: 500,
      auxiliary_data_hash: hash32C,
      validity_interval_start: 100,
      mint_hash: new Uint8Array(32).fill(0xb),
      script_data_hash: new Uint8Array(32).fill(0xc),
      required_signers_hash: new Uint8Array(32).fill(0xd),
      network_id: 1,
      reference_inputs_hash: new Uint8Array(32).fill(0xe),
      required_observers_hash: new Uint8Array(32).fill(0xf),
    };
    const encoded = encodeTransactionBodyCompact(compact);
    const decoded = decodeTransactionBodyCompact(encoded);
    expect(decoded.ttl).toBe(500);
    expect(decoded.validity_interval_start).toBe(100);
    expect(decoded.network_id).toBe(1);
    expect(decoded.mint_hash).toEqual(new Uint8Array(32).fill(0xb));
    expect(decoded.required_observers_hash).toEqual(
      new Uint8Array(32).fill(0xf),
    );
});

it("Minimal full transaction round trips", () => {
    const encoded = encodeTransaction(minimalTransaction);
    const decoded = decodeTransaction(encoded);
    expect(decoded.is_valid).toBe(true);
    expect(decoded.body.inputs.length).toBe(1);
    expect(decoded.body.fee).toBe(170_000n);
    expect(decoded.witness_set.vkey_witnesses).toBeUndefined();
    expect(decoded.witness_set.redeemers).toBeUndefined();

    // Also test is_valid: false
    const txFalse = { ...minimalTransaction, is_valid: false };
    const decoded2 = decodeTransaction(encodeTransaction(txFalse));
    expect(decoded2.is_valid).toBe(false);
});

it("Compact transaction round trips valid flag", () => {
    const compact: TransactionCompact = {
      transaction_body_hash: hash32A,
      transaction_witness_set_hash: hash32B,
      is_valid: true,
    };
    const encoded = encodeTransactionCompact(compact);
    expect(encoded.length).toBe(72); // 32 + 32 + 8 = 72 bytes
    const decoded = decodeTransactionCompact(encoded);
    expect(decoded.transaction_body_hash).toEqual(hash32A);
    expect(decoded.transaction_witness_set_hash).toEqual(hash32B);
    expect(decoded.is_valid).toBe(true);

    // Also test is_valid: false
    const compactFalse = { ...compact, is_valid: false };
    expect(
      decodeTransactionCompact(encodeTransactionCompact(compactFalse)).is_valid,
    ).toBe(false);
});

// ---------------------------------------------------------------------------
// H-12: Bounds checking — length fields must not exceed remaining bytes
// ---------------------------------------------------------------------------

it("decodeTransactionBody rejects inputs count exceeding remaining bytes", () => {
    // First 8 bytes claim 1,000,000 inputs — no actual input data follows
    const w = new Writer();
    writeU64(w, 1_000_000);
    expect(() => decodeTransactionBody(w.toBytes())).toThrow(/UnboundedLength/);
});

it("decodeTransactionBody rejects outputs count exceeding remaining bytes", () => {
    // inputsLen=0 is valid, then outputsLen=1,000,000 with no output data
    const w = new Writer();
    writeU64(w, 0); // inputsLen = 0
    writeU64(w, 1_000_000); // outputsLen = huge
    expect(() => decodeTransactionBody(w.toBytes())).toThrow(/UnboundedLength/);
});

it("decodeTransactionBody rejects required_signers count exceeding remaining bytes", () => {
    // Encode a minimal valid static body with bit 5 (required_signers) set in mask,
    // then in the dynamic section claim 1,000,000 signers with no actual data
    const body: TransactionBody = {
      ...minimalTransactionBody,
      required_signers: [hash28A], // forces bit 5 in opts_mask
    };
    const encoded = encodeTransactionBody(body);
    // Tamper: locate the required_signers length in the dynamic section.
    // The static section ends just before the dynamic data.  The dynamic section
    // starts with the output dynamic bytes, then optional fields in bit order.
    // For a body with only required_signers set (bit 5), the dynamic section is:
    //   output dynamics (addrLen-aligned address bytes for the one output)
    //   required_signers count (8 bytes) followed by signers
    // We build a truncated buffer: valid static + output dynamic + huge count (8 bytes).
    const validBody = encodeTransactionBody(minimalTransactionBody);
    // Build static section with bit 5 set but no signers data in dynamic
    const w = new Writer();
    writeU64(w, 1); // inputsLen
    w.write(encoded.slice(8, 48)); // copy the one input's bytes (40 bytes)
    writeU64(w, 1); // outputsLen
    // We can't easily tamper inline without reimplementing the encoder.
    // Instead, use a simpler approach: valid static section + 0 inputs/outputs + huge signers
    const w2 = new Writer();
    writeU64(w2, 0); // inputsLen = 0
    writeU64(w2, 0); // outputsLen = 0
    // fee as BigU64
    const feeBuf = new Uint8Array(8);
    new DataView(feeBuf.buffer).setUint32(4, 170_000, false);
    w2.write(feeBuf);
    writeU64(w2, 1 << 5); // opts_mask: bit 5 = required_signers
    // Dynamic section: required_signers count = 1,000,000
    writeU64(w2, 1_000_000);
    expect(() => decodeTransactionBody(w2.toBytes())).toThrow(/UnboundedLength/);
});
