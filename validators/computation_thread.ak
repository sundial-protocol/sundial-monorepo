use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, OutputReference, Redeemer, Transaction,
}
use midgard/types/ct.{ComputationThreadRedeemer}
use midgard/types/fpvs.{FraudProofValidatorDatum}
use midgard/types/general.{Key, LinkedListDatum}
use midgard/types/sq.{StateDatum}
use midgard/utils

validator computation_thread(
  fraud_proof_catalogue_script_hash: ByteArray,
  hub_oracle_policy_id: PolicyId,
) {
  mint(
    redeemer: ComputationThreadRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    expect Some(fraud_proof_catalogue_node_input) =
      list.at(
        self.reference_inputs,
        redeemer.fraud_proof_set_node_ref_input_index,
      )
    expect Some(hub_oracle_input) =
      list.at(self.reference_inputs, redeemer.hub_oracle_ref_input_index)
    let state_queue_policy_id = "TODO field of the Midgard hub oracle"
    expect Some(frauded_state_queue_node_input) =
      list.at(
        self.reference_inputs,
        redeemer.frauded_state_queue_node_ref_input_index,
      )

    expect InlineDatum(fraud_proof_catalogue_node_datum) =
      fraud_proof_catalogue_node_input.output.datum
    expect parsed_fraud_proof_catalogue_node_datum: LinkedListDatum<
      FraudProofValidatorDatum,
    > = fraud_proof_catalogue_node_datum
    expect Key(parsed_fraud_proof_catalogue_node_datum_key) =
      parsed_fraud_proof_catalogue_node_datum.key
    expect InlineDatum(frauded_state_queue_node_datum) =
      frauded_state_queue_node_input.output.datum
    expect parsed_frauded_state_queue_node_datum: LinkedListDatum<StateDatum> =
      frauded_state_queue_node_datum

    let token_to_mint_asset_name =
      bytearray.concat(
        parsed_fraud_proof_catalogue_node_datum_key,
        parsed_frauded_state_queue_node_datum.extra_data.block_hash,
      )
    and {
      utils.quantity_of_policy_id(self.mint, policy_id) == 1,
      or {
        quantity_of(self.mint, policy_id, token_to_mint_asset_name) == -1,
        //TODO Do we need to check that the tx is signed by the fraud prover pub-key hash of output state?
        and {
          quantity_of(self.mint, policy_id, token_to_mint_asset_name) == 1,
          validate_fraud_proof_catalogue_node_input(
            fraud_proof_catalogue_node_input,
          ),
          validate_hub_oracle_input(hub_oracle_input, hub_oracle_policy_id),
          validate_frauded_state_queue_node_input(
            frauded_state_queue_node_input,
          ),
          validate_output_to_initial_fraud_proof_step(
            parsed_fraud_proof_catalogue_node_datum.extra_data.init_step_script_hash,
          ),
        },
      },
    }
  }

  else(_) {
    fail
  }
}

fn validate_fraud_proof_catalogue_node_input(fraud_proof_catalogue_node_input) {
  todo
}

fn validate_hub_oracle_input(hub_oracle_input, hub_oracle_policy_id) {
  todo
}

fn validate_frauded_state_queue_node_input(frauded_state_queue_node_input) {
  todo
}

fn validate_output_to_initial_fraud_proof_step(init_step_script_hash) {
  todo
}
