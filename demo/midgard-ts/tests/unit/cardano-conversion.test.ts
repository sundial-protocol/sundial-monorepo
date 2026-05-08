import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { cmlToMidgard, midgardToCml } from "../../src/cardano";
import { encodeTransaction, decodeTransaction } from "../../src/index";

describe("Cardano simple ADA transfer converts to Midgard and back", () => {
  it("Cardano simple ADA transfer converts to Midgard and back", () => {
    function b(n: number, fill: number): Uint8Array {
      return new Uint8Array(n).fill(fill);
    }

    // Build a valid Conway-format CML transaction locally:
    //   - one spend input: hash filled with 0x11, index 0
    //   - one coin output: enterprise address (mainnet, pubkey hash 0xab), 2_000_000n lovelace
    //   - fee: 170_000n
    //   - no witnesses (empty witness set)
    //   - is_valid: true
    const txHash = CML.TransactionHash.from_raw_bytes(b(32, 0x11));
    const cred = CML.Credential.new_pub_key(
      CML.Ed25519KeyHash.from_raw_bytes(b(28, 0xab)),
    );
    const addr = CML.EnterpriseAddress.new(1, cred).to_address();
    const input = CML.TransactionInput.new(txHash, 0n);
    const inputs = CML.TransactionInputList.new();
    inputs.add(input);
    const conwayOut = CML.ConwayFormatTxOut.new(
      addr,
      CML.Value.from_coin(2_000_000n),
    );
    const output = CML.TransactionOutput.new_conway_format_tx_out(conwayOut);
    const outputs = CML.TransactionOutputList.new();
    outputs.add(output);
    const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
    const ws = CML.TransactionWitnessSet.new();
    const cmlTx = CML.Transaction.new(body, ws, true);

    // Round-trip: CML → Midgard → codec → Midgard → CML
    const originalCbor = cmlTx.to_cbor_hex();
    const midgard = cmlToMidgard(cmlTx);
    const encoded = encodeTransaction(midgard);
    const midgard2 = decodeTransaction(encoded);
    const restoredCml = midgardToCml(midgard2);
    const restoredCbor = restoredCml.to_cbor_hex();

    expect(restoredCbor).toBe(originalCbor);
  });
});
