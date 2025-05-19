use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Transaction}
use midgard/common/parameter_validation
use midgard/user_events/witness.{
  MintOrBurn, PublishRedeemer, RegisterToProveNotRegistered,
  RegisterToProveRegistered, UnregisterToProveNotRegistered,
  UnregisterToProveRegistered,
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
            let mint_qty = tx.mint |> assets.quantity_of(target_policy, nonce)

            when cert is {
              RegisterCredential { .. } -> mint_qty == 1
              UnregisterCredential { .. } -> mint_qty == -1
              _ -> False
            }
          }
          RegisterToProveNotRegistered { registration_redeemer_index } -> {
            expect [registration_pair,
              unregistration_pair, ..] =
              tx.redeemers |> list.drop(registration_redeemer_index)

            expect Pair(Publish {
              certificate: RegisterCredential {
                credential: Script(reg_script_hash),
                ..
              },
              ..
            },
            reg_redeemer_data) = registration_pair
            expect RegisterToProveNotRegistered = reg_redeemer_data

            expect Pair(Publish {
              certificate: UnregisterCredential {
                credential: Script(unreg_script_hash),
                ..
              },
              ..
            },
            unreg_redeemer_data) = unregistration_pair.1st
            expect UnregisterToProveNotRegistered = reg_redeemer_data
          }
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
