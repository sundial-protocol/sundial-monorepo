use midgard/common/types.{POSIXTime}

pub type Datum {
  commitment_time: Option<POSIXTime>,
}

pub type MintRedeemer {
  Init { init_utxo_index: Int }
  Deinit
  Retire_Operator {
    new_retire_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_removed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    retired_operator_appended_node_output_index: Int,
    retired_operator_anchor_node_output_index: Int,
  }
  Recover_Operator_Bond {
    retired_operator_key: ByteArray,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
  }
  Remove_Operator_Slash_Bond {
    slashed_retired_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_slashed_node_input_index: Int,
    retired_operator_anchor_node_input_index: Int,
    state_queue_redeemer_index: Int,
    state_queue_removed_node_input_index: Int,
    state_queue_anchor_node_input_index: Int,
  }
}
