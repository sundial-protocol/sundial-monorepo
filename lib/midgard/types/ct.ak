pub type ComputationThreadRedeemer {
  fraud_proof_set_node_ref_input_index: Int,
  hub_oracle_ref_input_index: Int,
  frauded_state_queue_node_ref_input_index: Int,
  output_to_init_step_index: Int,
  fraud_prover_hash: ByteArray,
}

pub type StepDatum {
  fraud_prover: ByteArray,
  data: Data,
}
