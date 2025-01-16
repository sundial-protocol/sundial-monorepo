use midgard/fraud_proof/common/types.{Proof}

pub type Step1Redeemer {
  Cancel { index_output: Int }
  Continue {
    frauded_state_queue_node_ref_input_index: Int,
    frauded_tx1_hash: ByteArray,
    frauded_tx1_info_hash: ByteArray,
    tx1_to_root_proof_path: Proof,
    index_output: Int,
  }
}

pub type Step1State {
  verified_tx1_hash: ByteArray,
}
