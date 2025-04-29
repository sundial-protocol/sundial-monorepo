use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{AssetName}
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.DepositEvent>

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  mint_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub type SpendPurpose {
  TransferToReserve { membership_proof: mpf.Proof }
  Refund { refund_approach: RefundApproach }
}

pub type RefundApproach {
  SkippedDeposit { non_membership_proof: mpf.Proof }
  DepositInSettlementGap { settlement_link_ref_input_index: Int }
}

pub type MintRedeemer {
  AuthenticateDeposit {
    nonce_input_index: Int,
    deposit_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnDepositNFT {
    l2_id: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}
