use cardano/transaction.{TransactionId}
use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact}

pub type State {
  verified_tx1_id: TransactionId,
  verified_tx1_inputs_root: MerkleRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  bad_state_queue_node_ref_input_index: Int,
  bad_tx_id: TransactionId,
  bad_tx_body: MidgardTxBodyCompact,
  bad_tx: MidgardTxBodyCompact,
  bad_tx_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
