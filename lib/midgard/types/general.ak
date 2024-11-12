use aiken/crypto.{Blake2b_224, Hash, VerificationKey}
use cardano/assets.{AssetName, PolicyId, Value}

/// A number of milliseconds since 00:00:00 UTC on 1 January 1970.
pub type POSIXTime =
  Int

pub type AssetClass {
  policy_id: PolicyId,
  asset_name: AssetName,
}

pub type PubKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type NodeKey {
  Key { key: ByteArray }
  Empty
}

pub type NodeDatum<app_data> {
  key: NodeKey,
  link: NodeKey,
  data: app_data,
}

pub type Common<datum_type> {
  own_cs: PolicyId,
  mint: Value,
  node_inputs: List<NodeDatum<datum_type>>,
  node_outputs: List<NodeDatum<datum_type>>,
}
