use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{TransactionId}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact}

pub type State {
  verified_tx1_body: MidgardTxBodyCompact,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer {
  Cancel { input_index: Int, computation_thread_mint_redeemer_index: Int }
  Continue {
    input_index: Int,
    output_index: Int,
    hub_ref_input_index: Int,
    bad_state_queue_node_ref_input_index: Int,
    bad_tx_id: TransactionId,
    bad_tx_body: MidgardTxBodyCompact,
    bad_tx: MidgardTxBodyCompact,
    bad_tx_membership_proof: mpf.Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
}
