use cardano/assets.{quantity_of}
use cardano/transaction.{OutputReference, Transaction}
use midgard/types/redeemer.{FraudProofSpendRedeemer}
use midgard/utils

validator publisher_set_spend(publisher_set_mint_script_hash: ByteArray) {
  spend(
    _datumOpt: Option<Data>,
    redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    todo
  }

  else(_) {
    fail
  }
}
