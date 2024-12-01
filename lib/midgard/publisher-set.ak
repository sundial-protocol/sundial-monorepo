use cardano/address.{Address}
use cardano/transaction.{OutputReference}
use midgard/common/types.{POSIXTime, PubKeyHash}

pub type Config {
  init_utxo: OutputReference,
  midgard_treasury: Address,
  refund_waiting_period: Int,
  min_lovelace_bond: Int,
}

pub type Datum {
  insert_time: POSIXTime,
  refund_process_init_time: POSIXTime,
}

pub type Redeemer {
  Init
  Deinit
  Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  Slash {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
  }
  InitRefund { node_to_mark_key: PubKeyHash }
  CompleteRefund { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}
