use aiken/crypto.{ScriptHash}
use cardano/transaction.{OutputReference, Transaction}
use midgard/hub_oracle as hub
use midgard/settlement/deposit.{Datum, Redeemer}

validator main(hub_oracle: ScriptHash, mpf_validator: ScriptHash) {
  spend(
    m_datum: Option<Datum>,
    redeemer: Redeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(datum) = m_datum
    let Datum { mpf_root, processed, publisher } = datum
    todo
  }

  else(_) {
    fail
  }
}
