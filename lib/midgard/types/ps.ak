use cardano/address.{Address}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{OutputReference}
use midgard/types/general.{POSIXTime, PubKeyHash, SetNode}

pub type PublisherSetConfig {
  init_utxo: OutputReference,
  deadline: POSIXTime,
  penalty_address: Address,
  midgard_treasury_hash: ByteArray,
  refund_period: Int,
  min_lovelace_bond: Int,
}

pub type Common {
  own_cs: PolicyId,
  mint: Value,
  node_inputs: List<NodePair>,
  node_outputs: List<NodePair>,
}

pub type PublisherDatum {
  insert_time: POSIXTime,
  refund_process_init_time: POSIXTime,
  node: SetNode,
}

pub type NodePair {
  val: Value,
  publisher_data: PublisherDatum,
}

pub type PublisherSetRedeemer {
  PS_Init
  PS_Deinit
  PS_Insert { key_to_insert: PubKeyHash, covering_node: SetNode }
  PS_Slash {
    key_to_remove: PubKeyHash,
    covering_node: SetNode,
    remove_set_node: SetNode,
  }
  PS_InitRefund { key_to_mark: PubKeyHash }
  PS_CompleteRefund { key_to_remove: PubKeyHash, covering_node: SetNode }
}
