use cardano/address.{Address}
use cardano/transaction.{OutputReference}

pub type PublisherSetConfig {
  init_utxo: OutputReference,
  midgard_treasury: Address,
  refund_waiting_period: Int,
  min_lovelace_bond: Int,
}

pub type StateQueueConfig {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type FraudProofValidatorSetConfig {
  init_utxo: OutputReference,
}
