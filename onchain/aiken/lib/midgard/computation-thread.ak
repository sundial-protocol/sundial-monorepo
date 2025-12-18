use aiken/crypto.{VerificationKeyHash}

pub type StepDatum<a> {
  fraud_prover: VerificationKeyHash,
  data: Option<a>,
}

pub type MintRedeemer {
  Init {
    fraud_proof_set_node_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    output_to_init_step_index: Int,
    fraud_prover_hash: VerificationKeyHash,
    fraud_proof_catalogue_asset_name: ByteArray,
    state_queue_asset_name: ByteArray,
  }
  Success { token_to_burn_asset_name: ByteArray }
  Cancel { token_to_burn_asset_name: ByteArray }
}
