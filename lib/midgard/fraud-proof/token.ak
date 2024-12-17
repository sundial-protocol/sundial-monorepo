use midgard/common/types.{POSIXTime}

pub type Datum {
  fraud_prover: ByteArray,
}

pub type ValidatorDatum {
  insert_time: POSIXTime,
  init_step_script_hash: ByteArray,
}

pub type SpendRedeemer {
  fraud_proof_token_asset_name: ByteArray,
}

pub type MintRedeemer {
  Mint {
    hub_oracle_ref_input_index: Int,
    fraud_proof_last_step_input_index: Int,
    computation_thread_token_asset_name: ByteArray,
    output_with_fraud_proof_index: Int,
    hub_oracle_asset_name: ByteArray,
  }
  Burn {
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    fraud_proof_token_asset_name: ByteArray,
    hub_oracle_asset_name: ByteArray,
    fraud_proof_spend_input_index: Int,
  }
}
