import { inspectTransactionCbor, RejectCodes } from "../../src/index";
import { buildSimpleAdaTransferFixture } from "./helpers/cardano-fixtures";

const phaseAConfig = {
  expectedNetworkId: 1,
  cardanoNetwork: 1,
  minFeeA: 0n,
  minFeeB: 0n,
};

it("inspects valid CBOR and returns accepted validation metadata", () => {
  const { cmlTx, txId } = buildSimpleAdaTransferFixture();
  const expectedTxIdHex = Buffer.from(txId).toString("hex");

  const result = inspectTransactionCbor({
    cborHex: cmlTx.to_cbor_hex(),
    expectedTxIdHex,
    phaseAConfig,
  });

  expect(result.computedTxIdHex).toBe(expectedTxIdHex);
  expect(result.validation.status).toBe("accepted");
  expect(result.cborByteSize).toBeGreaterThan(0);
  expect(result.midgardByteSize).toBeGreaterThan(0);
  expect(result.shape?.inputCount).toBe(1);
  expect(result.shape?.outputCount).toBe(1);
  expect(result.shape?.hasInlineDatum).toBe(false);
});

it("rejects when provided tx id does not match the CBOR body hash", () => {
  const { cmlTx } = buildSimpleAdaTransferFixture();

  const result = inspectTransactionCbor({
    cborHex: cmlTx.to_cbor_hex(),
    expectedTxIdHex: "11".repeat(32),
    phaseAConfig,
  });

  expect(result.validation.status).toBe("rejected");
  if (result.validation.status === "rejected") {
    expect(result.validation.rejectCode).toBe(RejectCodes.TxHashMismatch);
    expect(result.validation.detail).toContain("provided");
  }
});
