use aiken/merkle_patricia_forestry as mpf
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.WithdrawalEvent>

pub type SpendPurpose {
  Finalize { membership_proof: mpf.Proof, reserve_redeemer_index: Int }
  Refund { refund_approach: user_events.RefundApproach }
}

pub type SpendRedeemer =
  user_events.SpendRedeemer<SpendPurpose>
