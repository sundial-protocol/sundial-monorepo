// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/types/ct.{StepDatum}
use midgard/types/redeemer.{DS_Cancel,
  DS_Continue, DoubleSpendStep1Redeemer}
use midgard/utils.{ct_token_burnt, is_output_to_sc}

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
// Verify i is an input of tx:
// Merkle proof of membership: i ∈ tx.inputs root

// -------- Variable Definition ---------------------------------------
// (i) a state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (ii) tx_root is merkle root of transactions in the state queue block
// (iii) tx_hash is the hash of the transaction that is being proven to be in tx_root
// (iv) proof_path is the minimum set of hashes needed to reconstruct the path from the tx to the root

validator double_spend_s1(double_spend_s2_validator_script_hash: ByteArray, ct_token_policy_id: PolicyId, stake_validator: ScriptHash) {
  spend(
    step_datum: StepDatum,
    redeemer: DoubleSpendStep1Redeemer,
    input: OutputReference, // UTxO spent from the validator with the StepDatum { fraud_prover, Void }
    self: Transaction,
  ) {
    when redeemer is {
      // --------- PURPOSE - Cancel Redeemer----------------------------------
      // (1) Burn the computation thread token 
      // (2) Return the ADA from the computation thread utxo to the fraud prover pub-key defined in the input datum
      // (3) Transaction must not mint or burn any other tokens
      DS_Cancel  -> {
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        
        and {
          ct_token_burnt(
            own_input.output.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ),
          // (1) Burn CT Token, (2) Return ADA to fraud_prover 
          validate_output_to_fraud_prover(output_to_fraud_prover, fraud_prover_hash),
          
          // (3) TODO: No mint or burn of any other token
          // quantity_of_policy_id(
          //   self.mint,
          //   ct_token_policy_id,
          // ) == ,
        }
      }
      //  ------- Computation Step 1 - Membership Verification of tx1 ------- 
      // Step - 1: Fraud Prover Input: state_queue_node_ref_input_index, tx1_hash, proof_path 

      // --------- PURPOSE - Continue Redeemer-------------------------------
      // (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
      // (3) The fraud prover field must match between the output state and the input datum
      // (4) Verify the state transition from the previous state to the new state
      // (5) The computation thread token must be sent to the next step’s spending validator
      // (6) Transaction must not mint or burn any other tokens
      // --------------------------------------------------------------------

      DS_Continue {
        frauded_state_queue_node_ref_input_index,
        frauded_state_queue_tx1_hash,
        tx1_to_root_proof_path,
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

        //  ct token name 32 byte long - last 28 bytes of it should match the block_hash


        // (2) TODO: Delegate compute to merkelized validator - call out midgard-merkle endpoint to verify the inclusion of tx1 in the tx_root
        // Purpose: Prove inclusion, that the given transaction (tx1) exists in the frauded state commitment (tx_root) by delegating compute
        // --------------------------------------------------------------------
        
        expect [has_tx_hash] =
      [rdmr.tx_root, rdmr.tx_hash, rdmr.tx_info_hash, rdmr.proof]
        |> merkelized_validator.delegated_compute(
            plutarch_merkle_inclusion, // must come from parameters
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

      
        // (6) Transaction must not mint or burn any other tokens
        // --------------------------------------------------------------------
        // utils.quantity_of_policy_id(self.mint, policy_id_or_own_script_hash_of_step_1) == 
       
        and {
          todo,
          tx_root_matches_block_hash(tx_root, block_hash),
          has_tx_hash,
          // (3), (4), (5)
          validate_output_to_double_spend_step_2(
            output_to_init_step,
            fraud_prover_hash,
            double_spend_s2_validator_script_hash,
            policy_id,
            token_to_mint_asset_name,
          ),
          
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn tx_root_matches_block_hash(tx_root: ByteArray, block_hash: ByteArray) {
  todo,
}

// fn validate_output_to_fraud_prover(output: Output, fraud_prover_hash: ByteArray) {
//   when output.address.payment_credential is {
//     VerificationKey(hash) -> hash == fraud_prover_hash
//     _ -> False
//   }

//   and {
//     todo,
// }
// }

fn validate_output_to_double_spend_step_2(
  output_to_step_2: Output,
  fraud_prover_hash: ByteArray,
  double_spend_s2_validator_script_hash: ByteArray,
  own_hash: ByteArray,
  ct_token_asset_name: ByteArray,
) {
  expect InlineDatum(output_datum) = output_to_step_2.datum
  expect parsed_output_datum: StepDatum = output_datum

  let step1_state =
          DoubleSpendStep1State {
            tx1_hash: frauded_state_queue_tx1_hash,
            merkle_proof: tx1_to_root_proof_path,
          }

  let step_1_datum = StepDatum { fraud_prover: fraud_prover_hash, data: step1_state }

  let correctValue = and {
      quantity_of(output_to_step_2.value, own_hash, ct_token_asset_name) == 1,
      // list.length(flatten(output_to_step_2.value)) == ,
    }
  and {
    is_output_to_sc(output_to_step_2, double_spend_s2_validator_script_hash),
    parsed_output_datum == step_1_datum,
    correctValue,
  }
}