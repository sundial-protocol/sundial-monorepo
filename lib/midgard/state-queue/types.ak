use cardano/address.{Address}
use cardano/transaction.{OutputReference}

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

