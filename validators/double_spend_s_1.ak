// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}

use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}

use midgard/types/datum.{StateDatum, StepDatum}
use midgard/types/general.{LinkedListDatum}
use midgard/types/redeemer.{DS_Cancel, DS_Continue, DoubleSpendStep1Redeemer}

use aiken_design_patterns/merkelized_validator as merkelized_validator // Used to forward merkle inclusion proof logic to external script validator

// double_spend fraud proof: Proves membership Merkle proofs for 2 unique txs who spent the same input UTxO

// tx1 and tx2 refers to entire tx that contain:
// Input UTxOs it's trying to spend
// Output UTxOs it's creating

// Double spending occurs when the same UTxO appears as an input in two different txs
// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2):
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

// Common input: UTxO_A
// Verify i is an input of tx_1 and tx_2:
// Merkle proof of membership: i ∈ tx_1.inputs_root ∧ i ∈ tx_2.inputs_root

// -------- Variable Definition ---------------------------------------
// (i) a state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (ii) tx_root is merkle root of transactions in the state queue block
// (iii) tx_hash is the hash of the transaction that is being proven to be in tx_root
// (iv) proof_path is the minimum set of hashes needed to reconstruct the path from the tx to the root

pub type DoubleSpendStep1State {
  tx1_hash: ByteArray,
}

validator double_spend_s_1(
  double_spend_s_2_validator_script_hash: ByteArray,
  ct_token_policy_id: PolicyId,
  plutarch_merkle_inclusion_validator: ScriptHash,
) {
  spend(
    maybe_datum: Option<StepDatum>, // StepDatum { fraud_prover, data }
    redeemer: DoubleSpendStep1Redeemer, // DS_Cancel | DS_Continue
    input: OutputReference,  // UTxO spent from the validator with the StepDatum { fraud_prover, Void } (Step 0)
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {

      // --------- PURPOSE - Cancel Redeemer----------------------------------
      // (1) Burn the computation thread token 
      // (2) Return the ADA from the computation thread utxo to the fraud prover pub-key defined in the input datum
      // (3) Transaction must not mint or burn any other tokens

      DS_Cancel -> {
        // (1.1) Get ct token asset name
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        expect Some((_, _, lovelace)) =
          list.at(flatten(own_input.output.value), 0)
        // (2.1)
        expect Some(output_to_fraud_prover) = list.at(self.outputs, 0)
        and {
          self.mint == from_asset(
            // (1.2)
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          ),
          validate_output_to_fraud_prover(
            // (2.2)
            output_to_fraud_prover,
            step_datum.fraud_prover,
          ),
          quantity_of(output_to_fraud_prover.value, "", "") == lovelace,
          // TODO: (3)
        }
      }

      // --------- PURPOSE - Continue Redeemer-------------------------------
      // (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
      // (3) The fraud prover field must match between the output state and the input datum
      // (4) Verify the state transition from the previous state to the new state
      // (5) The computation thread token must be sent to the next step’s spending validator
      // (6) Transaction must not mint or burn any other tokens

      DS_Continue {
        frauded_state_queue_node_ref_input_index,
        frauded_state_queue_tx_root,
        frauded_state_queue_tx1_hash,
        frauded_state_queue_tx1_info_hash,
        frauded_state_queue_tx1_to_root_proof_path,
      } -> {
        // (1.1) Reference input with block_hash (tx_root) which carries the last 28 bytes of ct token name
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

        // (1.2) Get the ct token asset name
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        // (2) TODO: Delegate compute to merkelized validator - call out midgard-merkle endpoint to verify the inclusion of tx1 in the tx_root 
        expect [has_tx_hash] =
          [
            frauded_state_queue_tx_root, frauded_state_queue_tx1_hash,
            frauded_state_queue_tx1_info_hash,
            frauded_state_queue_tx1_to_root_proof_path,
          ] // (2.1)
            
          |> merkelized_validator.delegated_compute(
                plutarch_merkle_inclusion_validator, // parameterized in the spending validator
              
                // Input coercer - handles different types based on index
                fn(i, x) {
                  if i == 0 {
                    utils.coerce_tx_root(x)
                  } else if i == 3 {
                    utils.coerce_proof(x)
                  } else {
                    builtin.un_b_data(x)
                  }
                },

                // Output coercer
                fn(_i, x) {
                  expect bool_result: Bool = x
                  bool_result
                },
                tx.redeemers,
              )

        // (3) The fraud prover field must match between the output state and the input datum
        expect Some(output_to_step_2) = list.at(self.outputs, 0)
        expect InlineDatum(output_to_step_2_datum) = output_to_step_2.datum
        expect parsed_output_to_step_2_datum: StepDatum = output_to_step_2_datum
        // (5.1) Get the next step spending validator hash from the output
        expect Script(hash) = output_to_step_2.address.payment_credential

        // (4.1) Define the state transition        
        let step1_state =
          DoubleSpendStep1State { tx1_hash: frauded_state_queue_tx1_hash }
        // we now have verified tx1 is in tx_root, and should carry the verified txhash to step 2
        let step_1_datum =
          StepDatum {
            fraud_prover: parsed_output_to_step_2_datum.fraud_prover,
            data: step1_state,
          }

        and {
          drop(ct_token_asset_name, n: 28) == block_hash, // (1.3) tx_root matches block_hash
          //has_tx_hash, // (2) Merkelized validator returns true if tx1 is in tx_root
          parsed_output_to_step_2_datum.fraud_prover == step_datum.fraud_prover,  // (3) fraud prover field matches
          step_1_datum == parsed_output_to_step_2_datum, // (4.2) state transition verified
          hash == double_spend_s_2_validator_script_hash, // (5.2)
          quantity_of( // (6) no other mints or burns
            output_to_step_2.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1,
        }
      }
    }
  }

  else(_) {
    fail
  }
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
