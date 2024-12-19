use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/transaction.{OutputReference}
use midgard/common/types.{POSIXTime}

pub type Event {
  id: OutputReference,
  l2_address: Address,
  l2_datum: Option<Data>,
}

pub type Datum {
  event: Event,
  inclusion_time: POSIXTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: Option<Data>,
}

pub type SpendPurpose {
  FundWithdrawal
  TransferToReserve
  Refund
}

pub type SpendRedeemer {
  mpf_utxo: OutputReference,
  membership_proof: mpf.Proof,
  purpose: SpendPurpose,
}

pub type MintRedeemer {
  AuthenticateDeposit { nonce: OutputReference }
  BurnDepositNFT
}

pub type ObserverRedeemer {
  ObserverRedeemer
}
