use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/transaction.{OutputReference}
use midgard/common/types.{PosixTime}
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

pub type MintRedeemer {
  AuthenticateWithdrawalOrder {
    nonce_input_index: Int,
    withdrawal_order_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnWithdrawNFT
}
