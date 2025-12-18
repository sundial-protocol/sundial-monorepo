use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{TransactionId}
use midgard/common/types.{H32}
use midgard/computation_thread as ct

// TODO: Use `MPT<Key, Value>` instead of linear hashes.
pub type State {
  verified_tx1_inputs_root: H32<List<OutputReference>>,
  verified_tx2_inputs_root: H32<List<OutputReference>>,
}

pub type Datum =
  ct.StepDatum<State>

pub type StepRedeemer {
  Cancel { input_index: Int, computation_thread_mint_redeemer_index: Int }
  Continue {
    input_index: Int,
    output_index: Int,
    hub_ref_input_index: Int,
    bad_state_queue_node_ref_input_index: Int,
    bad_tx2_hash: TransactionId,
    bad_tx2_info_hash: ByteArray,
    bad_tx2_membership_proof: mpf.Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
}
