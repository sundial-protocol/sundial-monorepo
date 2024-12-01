use cardano/assets.{quantity_of}
use cardano/transaction.{OutputReference, Transaction}
use midgard/fraud_proof/catalogue.{SpendRedeemer}
use midgard/utils

validator spend(fraud_proof_script_hash: ByteArray) {
  spend(
    _datumOpt: Option<Data>,
    redeemer: SpendRedeemer,
    _input: OutputReference,
    self: Transaction,
  ) {
    and {
      quantity_of(
        self.mint,
        fraud_proof_script_hash,
        redeemer.fraud_proof_token_asset_name,
      ) == -1,
      utils.quantity_of_policy_id(self.mint, fraud_proof_script_hash) == 0,
    }
  }

  else(_) {
    fail
  }
}
