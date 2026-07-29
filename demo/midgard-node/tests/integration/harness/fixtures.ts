import { CML } from "@lucid-evolution/lucid";
import * as Ledger from "@/database/utils/ledger.js";

const producedSigningKey = CML.PrivateKey.from_normal_bytes(
  new Uint8Array(32).fill(0x21),
);
const producedPublicKey = producedSigningKey.to_public();
const producedCredential = CML.Credential.new_pub_key(producedPublicKey.hash());
const producedAddress = CML.EnterpriseAddress.new(
  0,
  producedCredential,
).to_address();

const spentSigningKey = CML.PrivateKey.from_normal_bytes(
  new Uint8Array(32).fill(0x42),
);
const spentPublicKey = spentSigningKey.to_public();
const spentCredential = CML.Credential.new_pub_key(spentPublicKey.hash());
const seedAddress = CML.EnterpriseAddress.new(0, spentCredential).to_address();

const spendInputHash = CML.TransactionHash.from_raw_bytes(
  new Uint8Array(32).fill(0x11),
);
const spendInput = CML.TransactionInput.new(spendInputHash, 0n);

const outputs = CML.TransactionOutputList.new();
outputs.add(
  CML.TransactionOutput.new_conway_format_tx_out(
    CML.ConwayFormatTxOut.new(producedAddress, CML.Value.from_coin(1_830_000n)),
  ),
);

const inputs = CML.TransactionInputList.new();
inputs.add(spendInput);

const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
body.set_network_id(CML.NetworkId.new(0n));

const bodyHash = CML.hash_transaction(body);
const witnessSet = CML.TransactionWitnessSet.new();
const vkeyWitnesses = CML.VkeywitnessList.new();
vkeyWitnesses.add(
  CML.Vkeywitness.new(
    spentPublicKey,
    spentSigningKey.sign(bodyHash.to_raw_bytes()),
  ),
);
witnessSet.set_vkeywitnesses(vkeyWitnesses);

const signedTx = CML.Transaction.new(body, witnessSet, true);

export const txCborA = Buffer.from(signedTx.to_cbor_bytes());
export const txIdA = Buffer.from(bodyHash.to_raw_bytes());
export const inputCborBytes = Buffer.from(spendInput.to_cbor_bytes());
export const outrefCborBytes = Buffer.from(
  CML.TransactionInput.new(bodyHash, 0n).to_cbor_bytes(),
);
export const outputCborBytes = Buffer.from(
  CML.TransactionOutput.new_conway_format_tx_out(
    CML.ConwayFormatTxOut.new(seedAddress, CML.Value.from_coin(2_000_000n)),
  ).to_cbor_bytes(),
);
export const testAddress = producedAddress.to_bech32();
export const spentAddress = seedAddress.to_bech32();

export const makeSeedLedgerEntry = (): Ledger.Entry => ({
  [Ledger.Columns.TX_ID]: txIdA,
  [Ledger.Columns.OUTREF]: inputCborBytes,
  [Ledger.Columns.OUTPUT]: outputCborBytes,
  [Ledger.Columns.ADDRESS]: spentAddress,
});
