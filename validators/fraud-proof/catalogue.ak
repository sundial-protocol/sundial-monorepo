use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/fraud_proof/catalogue.{Burn, Datum, Mint, SpendRedeemer, StepDatum}
use midgard/hub_oracle

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

validator mint(
  computation_thread_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: Datum, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Mint(
        hub_oracle_ref_input_index,
        fraud_proof_last_step_input_index,
        computation_thread_token_asset_name,
        output_with_fraud_proof_index,
        hub_oracle_asset_name,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect Some(fraud_proof_last_step_input) =
          list.at(self.inputs, fraud_proof_last_step_input_index)
        expect Some(output_with_fraud_proof) =
          list.at(self.outputs, output_with_fraud_proof_index)

        expect InlineDatum(hub_oracle_datum) = hub_oracle_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect InlineDatum(fraud_proof_last_step_datum) =
          fraud_proof_last_step_input.output.datum
        expect parsed_fraud_proof_last_step_datum: StepDatum =
          fraud_proof_last_step_datum
        let fraud_proof_spend_addr = parsed_hub_oracle_datum.fraud_proof_addr

        and {
          validate_hub_oracle_input(
            hub_oracle_input,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ),
          validate_fraud_proof_last_step_input(
            fraud_proof_last_step_input,
            computation_thread_script_hash,
            computation_thread_token_asset_name,
          ),
          quantity_of(
            self.mint,
            computation_thread_script_hash,
            computation_thread_token_asset_name,
          ) == -1,
          quantity_of(self.mint, policy_id, computation_thread_token_asset_name) == 1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          validate_output_to_fraud_proof_addr(
            output_with_fraud_proof,
            fraud_proof_spend_addr,
            parsed_fraud_proof_last_step_datum.fraud_prover,
            policy_id,
            computation_thread_token_asset_name,
          ),
        }
      }
      Burn(
        hub_oracle_ref_input_index,
        frauded_state_queue_node_ref_input_index,
        fraud_proof_token_asset_name,
        hub_oracle_asset_name,
        fraud_proof_spend_input_index,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect Some(fraud_proof_spend_input) =
          list.at(self.inputs, fraud_proof_spend_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        let fraud_proof_spend_addr = parsed_hub_oracle_datum.fraud_proof_addr
        let state_queue_policy_id = parsed_hub_oracle_datum.state_queue

        //Last 28 bytes contain the block_hash
        let block_hash = bytearray.drop(fraud_proof_token_asset_name, 4)

        expect InlineDatum(fraud_proof_spend_datum) =
          fraud_proof_spend_input.output.datum
        expect parsed_fraud_proof_spend_datum: Datum =
          fraud_proof_spend_datum

        and {
          validate_hub_oracle_input(
            hub_oracle_input,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ),
          validate_fraud_proof_spend_input(
            fraud_proof_spend_input,
            fraud_proof_spend_addr,
            policy_id,
            fraud_proof_token_asset_name,
          ),
          validate_non_member_state_queue_input(
            frauded_state_queue_node_input,
            state_queue_policy_id,
            block_hash,
          ),
          quantity_of(self.mint, policy_id, fraud_proof_token_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          is_signed_by_prover(self, parsed_fraud_proof_spend_datum.fraud_prover),
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn validate_hub_oracle_input(
  hub_oracle_input: Input,
  hub_oracle_script_hash: ByteArray,
  hub_oracle_asset_name: ByteArray,
) {
  quantity_of(
    hub_oracle_input.output.value,
    hub_oracle_script_hash,
    hub_oracle_asset_name,
  ) == 1
}

fn validate_fraud_proof_last_step_input(
  fraud_proof_last_step_input: Input,
  computation_thread_script_hash: ByteArray,
  computation_thread_token_asset_name: ByteArray,
) {
  quantity_of(
    fraud_proof_last_step_input.output.value,
    computation_thread_script_hash,
    computation_thread_token_asset_name,
  ) == 1
}

fn validate_non_member_state_queue_input(
  frauded_state_queue_node_input,
  state_queue_policy_id,
  block_hash,
) {
  todo
}

fn validate_fraud_proof_spend_input(
  fraud_proof_spend_input: Input,
  fraud_proof_spend_addr: Address,
  policy_id: ByteArray,
  fraud_proof_token_asset_name: ByteArray,
) {
  and {
    fraud_proof_spend_input.output.address == fraud_proof_spend_addr,
    quantity_of(
      fraud_proof_spend_input.output.value,
      policy_id,
      fraud_proof_token_asset_name,
    ) == 1,
  }
}

fn validate_output_to_fraud_proof_addr(
  output_with_fraud_proof: Output,
  fraud_proof_spend_addr: Address,
  fraud_prover_hash: ByteArray,
  own_hash: ByteArray,
  token_asset_name: ByteArray,
) {
  expect Script(script_hash) = fraud_proof_spend_addr.payment_credential

  expect InlineDatum(outputDatum) = output_with_fraud_proof.datum
  expect parsedDatum: Datum = outputDatum
  let newDatum = Datum { fraud_prover: fraud_prover_hash }

  let correctValue = and {
      quantity_of(output_with_fraud_proof.value, own_hash, token_asset_name) == 1,
      list.length(flatten(output_with_fraud_proof.value)) == 2,
    }
  and {
    utils.is_output_to_sc(output_with_fraud_proof, script_hash),
    parsedDatum == newDatum,
    correctValue,
  }
}

fn is_signed_by_prover(transaction: Transaction, fraud_prover: ByteArray) {
  list.has(transaction.extra_signatories, fraud_prover)
}
