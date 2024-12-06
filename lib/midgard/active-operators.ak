use midgard/common/types.{POSIXTime}

pub type Datum {
  commitment_time: Option<POSIXTime>,
}

pub type SpendRedeemer {
  List_State_Transition
  Update_Commitment_Time {
    active_node_input_index: Int,
    hub_oracle_ref_input_index: Int,
    state_queue_redeemer_index: Int,
    state_queue_appended_node_output_index: Int,
    state_queue_anchor_node_output_index: Int,
  }
}

pub type MintRedeemer {
  Init { init_utxo_index: Int }
  Deinit
  Activate_Operator {
    new_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    registered_operator_removed_node_input_index: Int,
    registered_operator_anchor_node_input_index: Int,
    active_operator_appended_node_output_index: Int,
    active_operator_anchor_node_output_index: Int,
  }
  Remove_Operator_Slash_Bond {
    slashed_active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_slashed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    state_queue_redeemer_index: Int,
    state_queue_removed_node_input_index: Int,
    state_queue_anchor_node_input_index: Int,
  }
  Retire_Operator {
    active_operator_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_removed_node_input_index: Int,
    active_operator_anchor_node_input_index: Int,
    retired_operator_inserted_node_output_index: Int,
    retired_operators_anchor_node_output_index: Int,
  }
}
