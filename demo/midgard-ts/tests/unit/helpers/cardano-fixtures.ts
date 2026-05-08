import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";

export function filledBytes(length: number, fill: number): Uint8Array {
  return new Uint8Array(length).fill(fill);
}

export type SimpleAdaTransferFixture = {
  cmlTx: CML.Transaction;
  txId: Uint8Array;
  addressBytes: Uint8Array;
  paymentKeyHashBytes: Uint8Array;
};

export function buildSimpleAdaTransferFixture(): SimpleAdaTransferFixture {
  const spendInputHash = CML.TransactionHash.from_raw_bytes(filledBytes(32, 0x11));
  const paymentKeyHashBytes = filledBytes(28, 0xab);
  const paymentCredential = CML.Credential.new_pub_key(
    CML.Ed25519KeyHash.from_raw_bytes(paymentKeyHashBytes),
  );

  const address = CML.EnterpriseAddress.new(1, paymentCredential).to_address();
  const inputs = CML.TransactionInputList.new();
  inputs.add(CML.TransactionInput.new(spendInputHash, 0n));

  const outputs = CML.TransactionOutputList.new();
  const conwayOutput = CML.ConwayFormatTxOut.new(
    address,
    CML.Value.from_coin(2_000_000n),
  );
  outputs.add(CML.TransactionOutput.new_conway_format_tx_out(conwayOutput));

  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const witnessSet = CML.TransactionWitnessSet.new();
  const cmlTx = CML.Transaction.new(body, witnessSet, true);

  return {
    cmlTx,
    txId: CML.hash_transaction(cmlTx.body()).to_raw_bytes(),
    addressBytes: address.to_raw_bytes(),
    paymentKeyHashBytes,
  };
}
