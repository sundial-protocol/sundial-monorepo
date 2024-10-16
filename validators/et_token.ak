use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, OutputReference, Redeemer, Transaction}
use midgard/types/et.{EvaluationThreadSetConfig}
use midgard/utils

validator et_token(cfg: EvaluationThreadSetConfig) {
  mint(redeemer: Redeemer, policy_id: PolicyId, self: Transaction) {
    todo
    //Put 1 minted token in the first carrier
    //Must have asset name == frauded block number
    //With datum: proverPubKeyHash
    //Must have a ref input to a Fraud Proof Set where inside you have the first carrier validator address
    //There must be a reference input of the frauded state queue node

    //Can burn only during Abort or FinishProof
  }

  else(_) {
    fail
  }
}
