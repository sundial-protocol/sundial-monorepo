use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct

pub type State {
  verified_tx2_inputs_root: MerkleRoot,
  double_spent_input_key: Data,
  double_spent_input_value: Data,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
