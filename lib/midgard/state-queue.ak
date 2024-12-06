use cardano/transaction.{OutputReference}
use midgard/common/types.{POSIXTime, PubKeyHash}

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type Datum {
  state_time: POSIXTime,
  publisher_key: ByteArray,
  block_hash: ByteArray,
}

pub type Redeemer {
  Init
  Deinit
  Append {
    prev_node_key: PubKeyHash,
    publisher_key: PubKeyHash,
    publisher_ref_input_index: Int,
  }
  Merge { root_node_key: PubKeyHash, mergeable_node_key: PubKeyHash }
  RemoveFraudChild {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
    node_input_index: Int,
  }
  RemoveFraudNode {
    prev_valid_node_key: ByteArray,
    remove_node_key: ByteArray,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_input_index: Int,
    node_input_index: Int,
  }
}
