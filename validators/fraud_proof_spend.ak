use cardano/assets.{quantity_of}
use cardano/transaction.{OutputReference, Transaction}
use midgard/types/fp.{FraudProofSpendRedeemer}
use midgard/utils

validator fraud_proof_spend(fraud_proof_script_hash: ByteArray) {
  spend(
    _datumOpt: Option<Data>,
    redeemer: FraudProofSpendRedeemer,
    _input: OutputReference,
    self: Transaction,
  ) {
    and {
      quantity_of(
        self.mint,
        fraud_proof_script_hash,
        redeemer.fraud_proof_token_asset_name,
      ) == -1,
      utils.quantity_of_policy_id(self.mint, fraud_proof_script_hash) == 1,
    }
  }

  else(_) {
    fail
  }
}
