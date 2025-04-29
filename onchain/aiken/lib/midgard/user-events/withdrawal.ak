use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.WithdrawalEvent>

pub type SpendPurpose {
  FinalizeWithdrawal
  Refund
}

pub type SpendRedeemer {
  mpf_utxo: OutputReference,
  membership_proof: mpf.Proof,
  purpose: SpendPurpose,
}
