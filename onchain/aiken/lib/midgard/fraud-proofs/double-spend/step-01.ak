use cardano/transaction.{TransactionId}
use aiken/merkle_patricia_forestry as mpf

// TODO: tx1_info_hash should have a more specific type
// We can fix this when we start using the Plutarch MPT library
pub type StepRedeemer {
  Cancel { input_index: Int, output_index: Int }
  Continue {
    input_index: Int,
    output_index: Int,
    hub_ref_input_index: Int,
    bad_state_queue_node_ref_input_index: Int,
    bad_tx1_hash: TransactionId,
    bad_tx1_info_hash: ByteArray,
    bad_tx1_to_root_proof_path: mpf.Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
}

pub type StepState {
  verified_tx1_hash: TransactionId,
}
