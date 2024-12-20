use aiken/crypto.{Blake2b_256, Hash}
use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/common/types.{POSIXTime, PubKeyHash, RootHash}

/// TODO?
/// > [!NOTE]
/// > Keep in mind that validators may depend on the lexicographic ordering
/// > between `confirmed_token_name` and `block_token_name`.
pub const confirmed_asset_name: AssetName = "CONFIRMED_STATE"

pub const block_asset_name: AssetName = #""

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type HeaderHash =
  Hash<Blake2b_256, Header>

pub type Header {
  prev_utxos_root: RootHash,
  utxos_root: RootHash,
  transactions_root: RootHash,
  deposits_root: RootHash,
  withdrawals_root: RootHash,
  start_time: POSIXTime,
  end_time: POSIXTime,
  prev_header_hash: HeaderHash,
  operator_vkey: PubKeyHash,
  protocol_version: Int,
}

pub type Block {
  header_hash: HeaderHash,
  header: Header,
  block_body: BlockBody,
}

pub type BlockBody {
  utxos: RootHash,
  transactions: RootHash,
  deposits: RootHash,
  withdrawals: RootHash,
}

pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: RootHash,
  start_time: POSIXTime,
  end_time: POSIXTime,
  protocol_version: Int,
}

pub type Datum {
  StateQueue { node_datum: NodeDatum }
  Canonical { confirmed_state: ConfirmedState }
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
