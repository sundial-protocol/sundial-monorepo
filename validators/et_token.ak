use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, OutputReference, Redeemer, Transaction}
use midgard/types/et.{EvaluationThreadConfig}
use midgard/utils

validator et_token(cfg: EvaluationThreadConfig) {
  mint(redeemer: EtTokenRedeemer, policy_id: PolicyId, self: Transaction) {
    expect Some(fraud_proof_set_node_input) =
      list.at(self.reference_inputs, fraud_proof_set_node_ref_input_index)
    expect Some(frauded_state_queue_node_input) =
      list.at(self.reference_inputs, frauded_state_queue_node_ref_input_index)
    or {
      quantity_of(self.mint, own_script_hash, frauded_block_hash) == -1,
      and {
        validate_fraud_proof_set_node_input(),
        validate_frauded_state_queue_node_input(),
        quantity_of(self.mint, own_script_hash, frauded_block_hash) == 1,
        correct_output_to_first_carrier_validator(
          fraud_proof_set_node_input_datum.first_carrier_validator_script_hash,
        ),
      },
    }
  }

  else(_) {
    fail
  }
}

fn correct_output_to_first_carrier_validator(
  first_carrier_validator_script_hash: ByteArray,
) {
  todo
  //Put 1 minted token in the first carrier
  //Must have asset name == frauded block number
  //With datum: proverPubKeyHash
}
