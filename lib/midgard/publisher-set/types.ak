use cardano/address.{Address}
use cardano/transaction.{OutputReference}

pub type Config {
  init_utxo: OutputReference,
  midgard_treasury: Address,
  refund_waiting_period: Int,
  min_lovelace_bond: Int,
}

