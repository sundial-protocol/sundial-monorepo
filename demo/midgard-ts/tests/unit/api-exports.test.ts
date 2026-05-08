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

describe("Public index exports codec helpers", () => {
  it("Public index exports codec helpers", () => {
    expect(typeof ALIGN).toBe("number");
    expect(typeof alignmentBytes).toBe("function");
    expect(typeof alignedSize).toBe("function");
    expect(typeof Writer).toBe("function");
    expect(typeof Reader).toBe("function");
    expect(() => new Writer()).not.toThrow();
    expect(() => new Reader(new Uint8Array(0))).not.toThrow();
  });
});

describe("Public index exports primitive codecs", () => {
  it("Public index exports primitive codecs", () => {
    expect(typeof encodeOutputReference).toBe("function");
    expect(typeof decodeOutputReference).toBe("function");
    expect(typeof encodeCredential).toBe("function");
    expect(typeof decodeCredential).toBe("function");
    expect(() =>
      encodeOutputReference({ tx_id: new Uint8Array(32).fill(1), index: 0 }),
    ).not.toThrow();
  });
});

describe("Public index exports event codecs", () => {
  it("Public index exports event codecs", () => {
    expect(typeof encodeDepositInfo).toBe("function");
    expect(typeof decodeDepositInfo).toBe("function");
    expect(typeof encodeWithdrawalInfo).toBe("function");
    expect(typeof decodeWithdrawalInfo).toBe("function");
    expect(() =>
      encodeDepositInfo({ l2_address: new Uint8Array(0), l2_datum: undefined }),
    ).not.toThrow();
  });
});

describe("Public index exports transaction and block codecs", () => {
  it("Public index exports transaction and block codecs", () => {
    expect(typeof encodeTransaction).toBe("function");
    expect(typeof decodeTransaction).toBe("function");
    expect(typeof encodeBlock).toBe("function");
    expect(typeof decodeBlock).toBe("function");
  });
});
