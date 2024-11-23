use midgard/types/general.{PubKeyHash}

pub type PublisherSetRedeemer {
  PS_Init
  PS_Deinit
  PS_Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  PS_Slash {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
  }
  PS_InitRefund { node_to_mark_key: PubKeyHash }
  PS_CompleteRefund { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}

pub type FraudProofValidatorSetRedeemer {
  FPVS_Init
  FPVS_Deinit
  FPVS_Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  FPVS_Remove { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}

pub type StateQueueRedeemer {
  SQ_Init
  SQ_Deinit
  SQ_Append {
    prev_node_key: PubKeyHash,
    publisher_key: PubKeyHash,
    publisher_ref_input_index: Int,
  }
  SQ_Merge { root_node_key: PubKeyHash, mergeable_node_key: PubKeyHash }
  SQ_Remove_Fraud_Child {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
    node_input_index: Int,
  }
  SQ_Remove_Fraud_Node {
    prev_valid_node_key: ByteArray,
    remove_node_key: ByteArray,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_input_index: Int,
    node_input_index: Int,
  }
}

pub type ComputationThreadRedeemer {
  CT_Mint {
    fraud_proof_set_node_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    output_to_init_step_index: Int,
    fraud_prover_hash: ByteArray,
    fraud_proof_catalogue_asset_name: ByteArray,
    hub_oracle_asset_name: ByteArray,
    state_queue_asset_name: ByteArray,
  }
  CT_Success { token_to_burn_asset_name: ByteArray }
  CT_Cancel { token_to_burn_asset_name: ByteArray }
}

pub type FraudProofMintRedeemer {
  FP_Mint {
    hub_oracle_ref_input_index: Int,
    fraud_proof_last_step_input_index: Int,
    computation_thread_token_asset_name: ByteArray,
    output_with_fraud_proof_index: Int,
    hub_oracle_asset_name: ByteArray,
  }
  FP_Burn {
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    fraud_proof_token_asset_name: ByteArray,
    hub_oracle_asset_name: ByteArray,
    fraud_proof_spend_input_index: Int,
  }
}

pub type FraudProofSpendRedeemer {
  fraud_proof_token_asset_name: ByteArray,
}

pub type RegisteredOperatorsMintRedeemer {
  ROM_Init { init_utxo_index: Int }
  ROM_Deinit
  ROM_Register_Operator {
    key_to_prepend: ByteArray,
    hub_oracle_ref_input_index: Int,
    active_operator_ref_input_index: Int,
    active_operator_asset_name: ByteArray,
    retired_operator_ref_input_index: Int,
    retired_operator_asset_name: ByteArray,
    prepended_node_output_index: Int,
    anchor_node_output_index: Int,
  }
  ROM_Activate_Operator {
    node_to_activate_key: ByteArray,
    hub_oracle_ref_input_index: Int,
    retired_operator_ref_input_index: Int,
    removed_node_input_index: Int,
    anchor_node_input_index: Int,
    active_operators_inserted_node_output_index: Int,
    active_operators_anchor_node_output_index: Int,
  }
  ROM_Deregister_Operator
  ROM_Remove_Duplicate_slash_Bond
}
