use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{Transaction}
use midgard/types/fp.{Burn, FraudProofMintRedeemer, Mint}
use midgard/utils

validator fraud_proof_mint(
  computation_thread_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: FraudProofMintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Mint(
        hub_oracle_ref_input_index,
        computation_thread_token_asset_name,
        output_with_fraud_proof_index,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        let fraud_proof_spend_addr = "TODO field of the Midgard hub oracle"
        expect Some(output_with_fraud_proof) =
          list.at(self.outputs, output_with_fraud_proof_index)

        and {
          validate_hub_oracle_input(hub_oracle_input, hub_oracle_script_hash),
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
          ),
        }
      }
      Burn(
        hub_oracle_ref_input_index,
        frauded_state_queue_node_ref_input_index,
        fraud_proof_token_asset_name,
      ) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        let state_queue_policy_id = "TODO field of the Midgard hub oracle"
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        //Last 28 bytes contain the block_hash
        let block_hash = bytearray.drop(fraud_proof_token_asset_name, 4)
        let fraud_prover = "TODO"

        and {
          validate_hub_oracle_input(hub_oracle_input, hub_oracle_script_hash),
          validate_non_member_state_queue_input(
            frauded_state_queue_node_input,
            state_queue_policy_id,
            block_hash,
          ),
          quantity_of(self.mint, policy_id, fraud_proof_token_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          is_signed_by_prover(fraud_prover),
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn validate_hub_oracle_input(hub_oracle_input, hub_oracle_script_hash) {
  todo
}

fn validate_output_to_fraud_proof_addr(
  output_with_fraud_proof,
  fraud_proof_addr,
) {
  todo
}

fn validate_non_member_state_queue_input(
  frauded_state_queue_node_input,
  state_queue_policy_id,
  block_hash,
) {
  todo
}

fn is_signed_by_prover(fraud_prover) {
  todo
}
