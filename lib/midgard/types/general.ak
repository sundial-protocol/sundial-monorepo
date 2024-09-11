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
  Key { key: PubKeyHash }
  Empty
}

pub type SetNode {
  key: NodeKey,
  next: NodeKey,
}

pub type LinkedListDatum<datum_type> {
  node: SetNode,
  extra_data: datum_type,
}

pub type NodePair<datum_type> {
  val: Value,
  data: LinkedListDatum<datum_type>,
}

pub type Common<datum_type> {
  own_cs: PolicyId,
  mint: Value,
  node_inputs: List<NodePair<datum_type>>,
  node_outputs: List<NodePair<datum_type>>,
}
