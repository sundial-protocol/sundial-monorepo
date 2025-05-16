use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Transaction}
use midgard/common/parameter_validation
use midgard/user_events/witness.{
  PublishRedeemer, MintOrBurn, RegisterToProveRegistered, RegisterToProveNotRegistered,
  UnregisterToProveRegistered, UnregisterToProveNotRegistered,
}

validator main(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: PublishRedeemer, certificate: Certificate, tx: Transaction) {
    parameter_validation.prehashed_param_no_datum_wrapper(
      validator_function: fn(
        nonce: Hash<Blake2b_256, ByteArray>,
        publish_redeemer: PublishRedeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        when publish_redeemer is {
          MintOrBurn { target_policy } -> {
            let mint_qty =
              tx.mint |> assets.quantity_of(target_policy, nonce)

            when cert is {
              RegisterCredential { .. } -> mint_qty == 1
              UnregisterCredential { .. } -> mint_qty == -1
              _ -> False
            }
          }
          RegisterToProveNotRegistered -> todo
          UnregisterToProveNotRegistered -> todo
          UnregisterToProveRegistered -> todo
          RegisterToProveRegistered -> todo
        }
      },
      param: nonce,
      redeemer: redeemer,
      variable_arg: certificate,
      tx: tx,
    )
  }

  else(_) {
    fail
  }
}
