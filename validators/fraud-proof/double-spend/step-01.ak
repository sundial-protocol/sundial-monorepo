use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/unordered.{NodeDatum}
use aiken_design_patterns/merkelized_validator
use cardano/address.{Script}
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{validate_output_to_fraud_prover}
use midgard/fraud_proof/double_spend.{
  Cancel_1, Continue_1, DoubleSpendStep1State, Step01Redeemer,
}
use midgard/ledger_state
use midgard/state_queue

// -------- GOAL DEFINITION --------------------------------------
// double_spend fraud proof: Proves membership Merkle proofs for 2 unique txs which spends the same input UTxO
// Double spending occurs when the same UTxO appears as an input in two different txs

// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2):
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

// Common input: UTxO_A (UTxO_A ∈ tx_1.inputs_root ∧ UTxO_A ∈ tx_2.inputs_root

// -------- PARAMETERS DEFINITION ------------------------------
// (i) state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (ii) tx_root, merkle root of transactions in the state queue block
// (iii) tx_hash, hash of the transaction that is being proven to be in tx_root
// (iv) proof_path: minimum set of hashes needed to reconstruct the path from the tx to the root

validator double_spend_step_01(
  // Script hash of next step (backwards compilation order of step validators required)
  double_spend_step_02_validator_script_hash: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
) {
  spend(
    a_datum: Option<StepDatum>,
    redeemer: Step01Redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = a_datum
    when redeemer is {
      // -------------- REDEEMER: CANCEL -----------------
      // (1) Burn the computation thread token 
      // (2) Return the ADA from the computation thread utxo to the fraud prover pub-key defined in the input datum
      // (3) Transaction must not mint or burn any other tokens
      // --------------------------------------------------
      Cancel_1 { index_output } -> {
        // (1.1)
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        // Store output reference to avoid multiple field accesses
        let output = own_input.output
        // (1.2) Verify the computation thread token by asset name in the output
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(output.value), 1)
        // (1.3) Burn the computation thread token
        expect
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          )

        // (2.1) Lovelace amount in output to return to the fraud prover
        expect Some((_, _, lovelace)) = list.at(flatten(output.value), 0)
        // (2.2)
        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        // (2.3) Validate output to fraud prover (output to fraud prover must match the fraud prover pubkey)
        expect
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          )

        // (3.1) Lovelace amount in output to return to the fraud prover
        expect quantity_of(output_to_fraud_prover.value, "", "") == lovelace
        True
      }

      // -------------- REDEEMER: CONTINUE -----------------
      // (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
      // (3) The fraud prover field must match between the output state and the input datum
      // (4) Verify the state transition from the previous state to the new state
      // (5) The computation thread token must be sent to the next step’s spending validator
      // (6) Transaction must not mint or burn any other tokens
      // ---------------------------------------------------
      Continue_1 {
        frauded_state_queue_node_ref_input_index,
        frauded_tx1_hash,
        frauded_tx1_info_hash,
        tx1_to_root_proof_path,
        index_output,
      } -> {
        // (1.1) Reference input with block_hash 
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )

        // (1.2) Extract tx_root via the state queue block hash (supposedly contains tx_1)
        expect InlineDatum(frauded_state_queue_datum_data) =
          frauded_state_queue_node_input.output.datum
        expect frauded_state_queue_node_datum: state_queue.Datum =
          frauded_state_queue_datum_data
        expect parsed_state_queue_datum: ledger_state.Header =
          frauded_state_queue_node_datum.data
        let block_hash = parsed_state_queue_datum.utxos_root
        let extracted_tx_root = #""

        // (1.3) Verify the computation thread token by asset name in the output
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)
        // (1.4) tx_root provided by fraud prover in the redeemer matches block_hash from the state queue block from the reference input (1.1) (ct_token_asset_name: last 28 bytes is the block_hash)
        expect drop(ct_token_asset_name, n: 4) == block_hash

        // (2) Forward proof logic to a stake validator (plutarch_midgard_merkle_validator)
        // Delegate merkle proof verification logic to a stake validator - call out midgard-merkle endpoint to verify the inclusion of tx1 in tx_root 
        // The plutarch membership proof script will likely have multiple redeemer "endpoints" and we need to ensure that always a specific/intended redeemer is being executed 
        // This function expects the script hash of the membership proof validator written in Plutarch & a function that takes the redeemer format expected by the membership proof validator and outputs a Boolean 
        // Fields from fraud prover input (parameters of the Continue Redeemer) need to be matched to the redeemer arguments expected by the stake validator (withdraw_redeemer)
        // ----------------------------------------------------------------------------------------------------------------------------------------------------------------
        expect
          merkelized_validator.delegated_validation(
            function_input: MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: frauded_tx1_hash,
              tx_info_hash: frauded_tx1_info_hash,
              tx_to_root_proof_path: tx1_to_root_proof_path,
            },
            staking_validator: plutarch_midgard_merkle_validator,
            input_data_coercer: fn(withdraw_redeemer: Data) {
              expect coerced: MembershipProofInputs = withdraw_redeemer
              coerced
            },
            redeemers: self.redeemers,
          )

        // (3) The fraud prover field must match between the output state and the input datum
        expect Some(output_to_step_2) = list.at(self.outputs, index_output)

        // (3.2) Fraud prover field of the input datum matches output state fraud prover field
        expect InlineDatum(output_to_step_2_datum) = output_to_step_2.datum
        expect parsed_output_to_step_2_datum: StepDatum = output_to_step_2_datum
        expect
          parsed_output_to_step_2_datum.fraud_prover == step_datum.fraud_prover

        // (4) Verify the state transition from the previous state to the new state
        // At this point, tx1_hash inclusion proof is verified by the plutarch_midgard_merkle_validator -- tx_1 is a member of the block we are running the fraud proof on    
        // (4.1) Define state transition (input -> output)    
        let step1_state =
          DoubleSpendStep1State { verified_tx1_hash: frauded_tx1_hash }
        let step_1_datum =
          StepDatum {
            fraud_prover: parsed_output_to_step_2_datum.fraud_prover,
            data: step1_state,
          }
        // (4.2) State transition verified to be correct in the output
        expect step_1_datum == parsed_output_to_step_2_datum

        // (5) Verify that output destination matches the expected hash of the next step spending validator (double_spend_step_02_validator_script_hash)
        expect Script(hash) = output_to_step_2.address.payment_credential
        expect hash == double_spend_step_02_validator_script_hash

        // (6) Computation thread token (STT) is sent to the next step validator
        expect
          quantity_of(
            output_to_step_2.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1
        True
      }
    }
  }

  else(_) {
    fail
  }
}
