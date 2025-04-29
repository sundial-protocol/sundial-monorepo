use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash, ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Transaction,
}
use env
use midgard/common/parameter_validation
use midgard/common/utils.{plutarch_pexcludes, plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events/withdrawal.{WitnessRedeemer}

validator witness(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: WitnessRedeemer, certificate: Certificate, tx: Transaction) {
    parameter_validation.prehashed_param_no_datum_wrapper(
      validator_function: fn(
        l1_nonce: Hash<Blake2b_256, ByteArray>,
        withdrawal_minting_policy: WitnessRedeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        let mint_qty =
          tx.mint |> assets.quantity_of(withdrawal_minting_policy, l1_nonce)

        when cert is {
          RegisterCredential { .. } -> mint_qty == 1
          UnregisterCredential { .. } -> mint_qty == -1
          _ -> False
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
