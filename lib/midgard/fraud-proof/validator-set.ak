use midgard/common/types.{PubKeyHash}

pub type Config {
  init_utxo: OutputReference,
}

pub type Redeemer {
  Init
  Deinit
  Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  Remove { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}
