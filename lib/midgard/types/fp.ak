pub type FraudProofMintRedeemer {
  Mint {
    hub_oracle_ref_input_index: Int,
    computation_thread_token_asset_name: ByteArray,
    output_with_fraud_proof_index: Int,
  }
  Burn {
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    fraud_proof_token_asset_name: ByteArray,
  }
}

pub type FraudProofSpendRedeemer {
  fraud_proof_token_asset_name: ByteArray,
}
