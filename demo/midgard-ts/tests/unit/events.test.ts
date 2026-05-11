import {
  encodeDepositInfo,
  decodeDepositInfo,
  encodeDepositInfoCompact,
  decodeDepositInfoCompact,
  encodeWithdrawalInfo,
  decodeWithdrawalInfo,
  encodeWithdrawalInfoCompact,
  decodeWithdrawalInfoCompact,
} from "../../src/types/events";

import { bytesSeq } from "./helpers/cardano-fixtures.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const addressA = bytesSeq(29);
const addressB = Uint8Array.from({ length: 29 }, (_, i) => (i + 50) & 0xff);
const datumA = bytesSeq(13);
const hash32A = new Uint8Array(32).fill(0x11);
const hash32B = new Uint8Array(32).fill(0x22);
const outRefA = { tx_id: hash32A, index: 0 };
const outRefB = { tx_id: hash32B, index: 1 };

it("Deposit info without datum round trips", () => {
    const input = { l2_address: addressA, l2_datum: undefined };
    const encoded = encodeDepositInfo(input);
    const decoded = decodeDepositInfo(encoded);
    expect(decoded.l2_address).toEqual(addressA);
    expect(decoded.l2_datum).toBeUndefined();
});

it("Deposit info with datum round trips", () => {
    const input = { l2_address: addressA, l2_datum: datumA };
    const encoded = encodeDepositInfo(input);
    const decoded = decodeDepositInfo(encoded);
    expect(decoded.l2_address).toEqual(addressA);
    expect(decoded.l2_datum).toEqual(datumA);
});

it("Deposit compact info with datum hash round trips", () => {
    const input = { l2_address: addressA, l2_datum: hash32A };
    const encoded = encodeDepositInfoCompact(input);
    const decoded = decodeDepositInfoCompact(encoded);
    expect(decoded.l2_address).toEqual(addressA);
    expect(decoded.l2_datum).toEqual(hash32A);

    // Also test without datum
    const nodatum = { l2_address: addressA, l2_datum: undefined };
    const decoded2 = decodeDepositInfoCompact(encodeDepositInfoCompact(nodatum));
    expect(decoded2.l2_datum).toBeUndefined();
});

it("Withdrawal info without datum round trips", () => {
    const input = {
      l2_outref: outRefA,
      l1_address: addressB,
      l1_datum: undefined,
    };
    const encoded = encodeWithdrawalInfo(input);
    const decoded = decodeWithdrawalInfo(encoded);
    expect(decoded.l2_outref.tx_id).toEqual(outRefA.tx_id);
    expect(decoded.l2_outref.index).toBe(outRefA.index);
    expect(decoded.l1_address).toEqual(addressB);
    expect(decoded.l1_datum).toBeUndefined();
});

it("Withdrawal info with datum round trips", () => {
    const input = {
      l2_outref: outRefA,
      l1_address: addressB,
      l1_datum: datumA,
    };
    const encoded = encodeWithdrawalInfo(input);
    const decoded = decodeWithdrawalInfo(encoded);
    expect(decoded.l2_outref.tx_id).toEqual(outRefA.tx_id);
    expect(decoded.l2_outref.index).toBe(outRefA.index);
    expect(decoded.l1_address).toEqual(addressB);
    expect(decoded.l1_datum).toEqual(datumA);
});

it("Withdrawal compact info with datum hash round trips", () => {
    const input = {
      l2_outref: outRefB,
      l1_address: addressB,
      l1_datum: hash32B,
    };
    const encoded = encodeWithdrawalInfoCompact(input);
    const decoded = decodeWithdrawalInfoCompact(encoded);
    expect(decoded.l2_outref.tx_id).toEqual(outRefB.tx_id);
    expect(decoded.l2_outref.index).toBe(outRefB.index);
    expect(decoded.l1_address).toEqual(addressB);
    expect(decoded.l1_datum).toEqual(hash32B);
});
