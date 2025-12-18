use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{TransactionId}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxCompact}

pub type State {
  verified_tx1: MidgardTxCompact,
  verified_tx2: MidgardTxCompact,
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
