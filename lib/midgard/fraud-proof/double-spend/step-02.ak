use midgard/fraud_proof/common/types.{Proof}

pub type Step2Redeemer {
  Cancel { index_output: Int }
  Continue {
    frauded_state_queue_node_ref_input_index: Int,
    frauded_tx2_hash: ByteArray,
    frauded_tx2_info_hash: ByteArray,
    tx2_to_root_proof_path: Proof,
    index_output: Int,
  }
}

pub type Step2State {
  verified_tx1_hash: ByteArray,
  verified_tx2_hash: ByteArray,
}
