use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, Transaction,
}
use midgard/types/et.{EvaluationThreadConfig, EvaluationThreadTokenRedeemer}
use midgard/types/fpvs.{FraudProofValidatorDatum}
use midgard/utils

validator et_token(cfg: EvaluationThreadConfig) {
  mint(
    redeemer: EvaluationThreadTokenRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    expect Some(fraud_proof_set_node_input) =
      list.at(
        self.reference_inputs,
        redeemer.fraud_proof_set_node_ref_input_index,
      )
    expect Some(frauded_state_queue_node_input) =
      list.at(
        self.reference_inputs,
        redeemer.frauded_state_queue_node_ref_input_index,
      )
    let frauded_block_hash =
      "TODO frauded_state_queue_node_input.datum.frauded_block_hash"
    expect InlineDatum(fraud_proof_set_node_datum) =
      fraud_proof_set_node_input.output.datum
    expect parsed_fraud_proof_set_node_datum: FraudProofValidatorDatum =
      fraud_proof_set_node_datum
    or {
      quantity_of(self.mint, policy_id, frauded_block_hash) == -1,
      and {
        validate_fraud_proof_set_node_input(),
        validate_frauded_state_queue_node_input(),
        quantity_of(self.mint, policy_id, frauded_block_hash) == 1,
        correct_output_to_first_carrier_validator(
          parsed_fraud_proof_set_node_datum.first_carrier_validator_script_hash,
        ),
      },
    }
  }

  else(_) {
    fail
  }
}

fn validate_fraud_proof_set_node_input() {
  todo
}

fn validate_frauded_state_queue_node_input() {
  todo
}

fn correct_output_to_first_carrier_validator(
  first_carrier_validator_script_hash: ByteArray,
) {
  todo
  //Put 1 minted token in the first carrier
  //Must have asset name == frauded block number
  //With datum: proverPubKeyHash
}
