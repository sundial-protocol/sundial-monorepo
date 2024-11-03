use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, OutputReference, Redeemer, Transaction,
}
use midgard/types/fp.{
  Burn, FraudProofMintRedeemer, FraudProofSpendRedeemer, Mint,
}
use midgard/types/fpvs.{FraudProofValidatorDatum}
use midgard/types/general.{Key, LinkedListDatum}
use midgard/types/sq.{StateDatum}
use midgard/utils

validator fraud_proof(
  computation_thread_script_hash: ByteArray,
  hub_oracle_policy_id: PolicyId,
) {
  mint(redeemer: FraudProofMintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Mint(hub_oracle_ref_input_index, computation_thread_token_asset_name) -> {
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        let fraud_proof_addr = "TODO field of the Midgard hub oracle"

        and {
          validate_hub_oracle_input(hub_oracle_input, hub_oracle_policy_id),
          quantity_of(
            self.mint,
            computation_thread_script_hash,
            computation_thread_token_asset_name,
          ) == -1,
          quantity_of(self.mint, policy_id, computation_thread_token_asset_name) == 1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          validate_output_to_fraud_proof_addr(fraud_proof_addr),
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
        let block_hash = "TODO Last 28 bytes of fraud_proof_token_asset_name"

        and {
          validate_hub_oracle_input(hub_oracle_input, hub_oracle_policy_id),
          validate_non_member_state_queue_input(
            frauded_state_queue_node_input,
            block_hash,
          ),
          quantity_of(self.mint, policy_id, fraud_proof_token_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          validate_output_to_fraud_prover(),
        }
      }
    }
  }

  spend(
    datumOpt: Option<Data>,
    redeemer: FraudProofSpendRedeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    let ownScriptHash = utils.get_own_hash(input, self.inputs)
    and {
      quantity_of(
        self.mint,
        ownScriptHash,
        redeemer.fraud_proof_token_asset_name,
      ) == -1,
      utils.quantity_of_policy_id(self.mint, ownScriptHash) == 1,
    }
  }

  else(_) {
    fail
  }
}

fn validate_hub_oracle_input(hub_oracle_input, hub_oracle_policy_id) {
  todo
}

fn validate_output_to_fraud_proof_addr(fraud_proof_addr) {
  todo
}

fn validate_non_member_state_queue_input(
  frauded_state_queue_node_input,
  block_hash,
) {
  todo
}

fn validate_output_to_fraud_prover() {
  todo
}
