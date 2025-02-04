use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/registered_operators.{WitnessStatus}

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

pub type Datum {
  operator: ByteArray,
  start_time: Int,
  end_time: Int,
}

pub type MintRedeemer {
  Init
  Deinit
  CommitBlockHeader {
    operator: ByteArray,
    scheduler_asset_name: ByteArray,
    hub_oracle_ref_input_index: Int,
    scheduler_state_ref_input_index: Int,
    header_node_output_index: Int,
    previous_header_node_output_index: Int,
    active_operators_redeemer_index: Int,
  }
  MergeToConfirmedState {
    header_node_input_index: Int,
    confirmed_state_node_input_index: Int,
    confirmed_state_node_output_index: Int,
  }
  RemoveFraudulentBlockHeader {
    fraud_proof_red_input_index: Int,
    fraud_proof_asset_name: ByteArray,
    fraudulent_operator: ByteArray,
    operator_status: WitnessStatus,
  }
}
