/**
 * TS-UNIT-001 – TS-UNIT-004: Public index exports
 *
 * Area: API
 * Layer: unit
 *
 * These tests confirm that the public surface of `src/index.ts` exports the
 * symbols that downstream consumers depend on.  They are pure import checks —
 * no encoding/decoding is performed here.
 *
 * No mocking required.
 * No extra tools required beyond vitest (already present in demo workspace).
 */

import {
  // Codec helpers (COD)
  ALIGN,
  alignmentBytes,
  alignedSize,
  Writer,
  Reader,
  // Primitive codecs (PRM)
  encodeOutputReference,
  decodeOutputReference,
  encodeCredential,
  decodeCredential,
  // Event codecs (EVT)
  encodeDepositInfo,
  decodeDepositInfo,
  encodeWithdrawalInfo,
  decodeWithdrawalInfo,
  // Transaction codecs (TXN)
  encodeTransaction,
  decodeTransaction,
  // Block codecs (BLK)
  encodeBlock,
  decodeBlock,
} from "../../src/index";

// ---------------------------------------------------------------------------
// TS-UNIT-001 Public index exports codec helpers
// ---------------------------------------------------------------------------
describe("Public index exports codec helpers", () => {
  it("Public index exports codec helpers", () => {
    /**
     * IMPLEMENTATION PLAN
     * -------------------
     * Imports: ALIGN, alignmentBytes, alignedSize, Writer, Reader from src/index
     *
     * Steps:
     *   1. Assert typeof ALIGN === "number"
     *   2. Assert typeof alignmentBytes === "function"
     *   3. Assert typeof alignedSize === "function"
     *   4. Assert Writer is a constructor (typeof Writer === "function")
     *   5. Assert Reader is a constructor (typeof Reader === "function")
     *   6. Optionally: new Writer() should not throw; new Reader(new Uint8Array(0)) should not throw
     *
     * Expected result: all symbols are defined and are the right types
     *
     * No mocks needed.
     */

    // Stub — replace with real assertions during implementation
    expect(1).toBe(1);
  });
});

// ---------------------------------------------------------------------------
// TS-UNIT-002 Public index exports primitive codecs
// ---------------------------------------------------------------------------
describe("Public index exports primitive codecs", () => {
  it("Public index exports primitive codecs", () => {
    /**
     * IMPLEMENTATION PLAN
     * -------------------
     * Imports: encodeOutputReference, decodeOutputReference,
     *          encodeCredential, decodeCredential from src/index
     *
     * Steps:
     *   1. Assert typeof encodeOutputReference === "function"
     *   2. Assert typeof decodeOutputReference === "function"
     *   3. Assert typeof encodeCredential === "function"
     *   4. Assert typeof decodeCredential === "function"
     *   5. Optionally call each with a minimal valid fixture to confirm callability:
     *      encodeOutputReference({ tx_id: new Uint8Array(32).fill(1), index: 0 })
     *
     * Expected result: each symbol is defined and callable
     *
     * No mocks needed.
     */

    // Stub — replace with real assertions during implementation
    expect(1).toBe(1);
  });
});

// ---------------------------------------------------------------------------
// TS-UNIT-003 Public index exports event codecs
// ---------------------------------------------------------------------------
describe("Public index exports event codecs", () => {
  it("Public index exports event codecs", () => {
    /**
     * IMPLEMENTATION PLAN
     * -------------------
     * Imports: encodeDepositInfo, decodeDepositInfo,
     *          encodeWithdrawalInfo, decodeWithdrawalInfo from src/index
     *
     * Steps:
     *   1. Assert typeof encodeDepositInfo === "function"
     *   2. Assert typeof decodeDepositInfo === "function"
     *   3. Assert typeof encodeWithdrawalInfo === "function"
     *   4. Assert typeof decodeWithdrawalInfo === "function"
     *   5. Optionally call encodeDepositInfo({ l2_address: new Uint8Array(0), l2_datum: undefined })
     *      to confirm it doesn't throw
     *
     * Expected result: each symbol is defined and callable
     *
     * No mocks needed.
     */

    // Stub — replace with real assertions during implementation
    expect(1).toBe(1);
  });
});

// ---------------------------------------------------------------------------
// TS-UNIT-004 Public index exports transaction and block codecs
// ---------------------------------------------------------------------------
describe("Public index exports transaction and block codecs", () => {
  it("Public index exports transaction and block codecs", () => {
    /**
     * IMPLEMENTATION PLAN
     * -------------------
     * Imports: encodeTransaction, decodeTransaction,
     *          encodeBlock, decodeBlock from src/index
     *
     * Steps:
     *   1. Assert typeof encodeTransaction === "function"
     *   2. Assert typeof decodeTransaction === "function"
     *   3. Assert typeof encodeBlock === "function"
     *   4. Assert typeof decodeBlock === "function"
     *
     * Expected result: each symbol is defined and callable
     *
     * No mocks needed.
     */

    // Stub — replace with real assertions during implementation
    expect(1).toBe(1);
  });
});
