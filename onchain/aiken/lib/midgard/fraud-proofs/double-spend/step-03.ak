use aiken/merkle_patricia_forestry as mpf
use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct

pub type State {
  verified_tx2_inputs_root: MerkleRoot,
  double_spent_input_key: Data,
  double_spent_input_value: Data,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer {
  Cancel { input_index: Int, computation_thread_mint_redeemer_index: Int }
  Continue {
    input_index: Int,
    output_index: Int,
    input_membership_proof: mpf.Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
}
