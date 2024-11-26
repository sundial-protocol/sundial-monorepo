// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/types/general.{LinkedListDatum}
use midgard/utils.{spend_custom}

// Invalid Input Reference fraud proof: A transaction tx in a proposed block contains an input referencing a nonexistent output.

//this step needs to check that the input we are specifying is part of the transaction body
pub type StepDatum {
  fraud_prover: ByteArray,
  data: Data,
}

//TODO This should be importend from a library, it's the state queue Datum
pub type StateDatum {
  block_hash: ByteArray,
  block_roots: BlockRoots,
}

pub type BlockRoots {
  block_withdraw_root: ByteArray,
  block_tx_root: ByteArray,
  block_deposit_root: ByteArray,
}

pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep1State {
  tx_hash: ByteArray,
  merkle_proof: ByteArray,
}

pub type Step2redeemer {
  Cancel { index_output: Int }
  Continue {
    frauded_state_queue_node_ref_input_index: Int,
    tx_body_tx_hash: ByteArray,
    tx_body_tx_index: Int,
    input_to_root_proof_path: ByteArray,
    index_output: Int,
  }
}

validator not_existing_input(
  ct_token_policy_id: PolicyId,
  next_step_spending_validator: ScriptHash,
  stake_validator: ScriptHash,
) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: Step2redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {
      Cancel { index_output } -> {
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        expect Some((_, _, lovelace)) =
          list.at(flatten(own_input.output.value), 0)

        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        and {
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          ),
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          ),
          quantity_of(output_to_fraud_prover.value, "", "") == lovelace,
        }
      }

      //  ------- Computation Step 2 - Membership Verification of input ------- 
      // Step - 1: Fraud Prover Input: 
      // Now I know that the transaction is part of the block, giving out a transaction and a body of it, first make sure the body is correct
      //second check if the input we want to prove is wrong is actually part of it 
      // --------------------------------------------------------------------
      Continue {
        frauded_state_queue_node_ref_input_index,
        frauded_state_queue_tx_hash,
        frauded_state_queue_tx_info_hash,
        tx_to_root_proof_path,
        index_output,
      } -> {
        // (1) Reference input with block_hash (tx_root) which carries the last 28 bytes of ct token name
        // --------------------------------------------------------------------
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect InlineDatum(frauded_state_queue_node_datum) =
          frauded_state_queue_node_input.output.datum

        expect parsed_frauded_state_queue_node_datum: LinkedListDatum<
          StateDatum,
        > = frauded_state_queue_node_datum
        let block_hash =
          parsed_frauded_state_queue_node_datum.extra_data.block_hash

        // TODO: (2.1) Extract tx_root from block_hash (implementation depends on block header format)
        let extracted_tx_root =
          parsed_frauded_state_queue_node_datum.extra_data.block_roots.block_tx_root
        //TODO this is not true, depends on header format
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        let merkleized_logic =
          spend_custom(
            stake_validator,
            fn(withdraw_redeemer: Data) {
              expect MembershipProofInputs {
                root,
                tx_hash,
                tx_info_hash,
                proof_path,
              } = withdraw_redeemer
              // Validate match: expected format <-> input_x  
              and {
                root == extracted_tx_root,
                tx_hash == frauded_state_queue_tx_hash,
                tx_info_hash == frauded_state_queue_tx_info_hash,
                proof_path == tx_to_root_proof_path,
              }
            },
            // Redeemer list of tx is accessed via self field 
            self,
          )

        //what's the output datum?
        expect Some(output_state) = list.at(self.outputs, index_output)
        //? is it fixed ASK
        expect InlineDatum(datum_output) = output_state.datum
        expect datum_parsed: StepDatum = datum_output

        // (4) Verify the state transition from the previous state to the new state, beside the address, what am I checking? the datum data what should contain?
        expect Script(hash) = output_state.address.payment_credential
        let step1_state =
          InvalidInputSpendStep1State {
            tx_hash: frauded_state_queue_tx_hash,
            merkle_proof: tx_to_root_proof_path,
          }
        let step_1_datum =
          StepDatum { fraud_prover: step_datum.fraud_prover, data: step1_state }
        and {
          drop(ct_token_asset_name, n: 28) == block_hash,
          step_1_datum == datum_parsed,
          quantity_of(
            output_state.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1,
          hash == next_step_spending_validator,
          merkleized_logic,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn ct_token_burnt(value: Value, policy: PolicyId, assetname: ByteArray) {
  quantity_of(value, policy, assetname) == -1
}

pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}
