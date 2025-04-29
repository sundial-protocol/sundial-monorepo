use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}

pub fn hash_out_ref(nonce_out_ref: OutputReference) -> AssetName {
  cbor.serialise(nonce_out_ref) |> blake2b_256
}
