import { cmlToMidgard, midgardToCml } from "../../src/cardano";
import { encodeTransaction, decodeTransaction } from "../../src/index";
import { buildSimpleAdaTransferFixture } from "./helpers/cardano-fixtures";

it("converts a simple ADA transfer between CML and Midgard without information loss", () => {
  const { cmlTx } = buildSimpleAdaTransferFixture();

  // Round-trip: CML -> Midgard -> codec -> Midgard -> CML
  const originalCbor = cmlTx.to_cbor_hex();
  const midgard = cmlToMidgard(cmlTx);
  const encoded = encodeTransaction(midgard);
  const midgard2 = decodeTransaction(encoded);
  const restoredCml = midgardToCml(midgard2);
  const restoredCbor = restoredCml.to_cbor_hex();

  expect(restoredCbor).toBe(originalCbor);
});
