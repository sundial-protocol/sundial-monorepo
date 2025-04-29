use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{AssetName}
use cardano/transaction.{Datum as CardanoDatum, Publish, Redeemer}
use midgard/common/types.{PosixTime}
use midgard/ledger_state

pub type Datum {
  event: ledger_state.DepositEvent,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: CardanoDatum,
}

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
