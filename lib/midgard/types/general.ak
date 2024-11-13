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

pub type LinkedListDatum<datum_type> {
  key: NodeKey,
  next: NodeKey,
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

pub type ProofStep {
  Branch {
    skip: Int,
    neighbors: ByteArray 
  }
  Fork {
    skip: Int,
    direction: Direction  // This would replace the Neighbor type
  }
  Leaf {
    skip: Int,
    key: ByteArray,
    value: ByteArray
  }
}

pub type Direction {
  Left
  Right
}

pub type Proof {
  steps: List<ProofStep>
}