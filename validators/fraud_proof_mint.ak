use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/types/datum.{HubOracleDatum}
use midgard/types/redeemer.{FP_Burn, FP_Mint, FraudProofMintRedeemer}
use midgard/utils

validator fraud_proof_mint(
  computation_thread_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: FraudProofMintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      FP_Mint(
        hub_oracle_ref_input_index,
        computation_thread_token_asset_name,
        output_with_fraud_proof_index,
        hub_oracle_asset_name,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect Some(output_with_fraud_proof) =
          list.at(self.outputs, output_with_fraud_proof_index)

        expect InlineDatum(hub_oracle_datum) = hub_oracle_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        let fraud_proof_spend_addr = parsed_hub_oracle_datum.fraud_proof_addr

        and {
          validate_hub_oracle_input(
            hub_oracle_input,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
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
            policy_id,
            computation_thread_token_asset_name,
          ),
        }
      }
      FP_Burn(
        hub_oracle_ref_input_index,
        frauded_state_queue_node_ref_input_index,
        fraud_proof_token_asset_name,
        hub_oracle_asset_name,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect InlineDatum(hub_oracle_datum) = hub_oracle_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        let state_queue_policy_id = parsed_hub_oracle_datum.state_queue

        //Last 28 bytes contain the block_hash
        let block_hash = bytearray.drop(fraud_proof_token_asset_name, 4)
        let fraud_prover = "TODO"

        and {
          validate_hub_oracle_input(
            hub_oracle_input,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ),
          validate_non_member_state_queue_input(
            frauded_state_queue_node_input,
            state_queue_policy_id,
            block_hash,
          ),
          quantity_of(self.mint, policy_id, fraud_proof_token_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          is_signed_by_prover(self, fraud_prover),
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

fn validate_non_member_state_queue_input(
  frauded_state_queue_node_input,
  state_queue_policy_id,
  block_hash,
) {
  todo
}

fn validate_output_to_fraud_proof_addr(
  output_with_fraud_proof: Output,
  fraud_proof_spend_addr: Address,
  own_hash: ByteArray,
  token_asset_name: ByteArray,
) {
  expect Script(script_hash) = fraud_proof_spend_addr.payment_credential
  //TODO No datum?
  let correctValue = and {
      quantity_of(output_with_fraud_proof.value, own_hash, token_asset_name) == 1,
      list.length(flatten(output_with_fraud_proof.value)) == 2,
    }
  and {
    utils.is_output_to_sc(output_with_fraud_proof, script_hash),
    correctValue,
  }
}

fn is_signed_by_prover(transaction: Transaction, fraud_prover: ByteArray) {
  True
}
//fn is_signed_by_prover(transaction: Transaction, fraud_prover: Address) {
//  expect VerificationKey(hash) = fraud_prover.payment_credential
//  list.has(transaction.extra_signatories, hash)
//}
