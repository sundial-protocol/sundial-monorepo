use aiken/collection/list
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/utils.{validate_output_to_fraud_prover}
use midgard/fraud_proof/double_spend/step_04.{Cancel, Continue, Step4Redeemer}

validator main(
  fraud_proof_token_policy: PolicyId,
  ct_token_policy_id: PolicyId,
) {
  spend(
    a_datum: Option<StepDatum>,
    redeemer: Step4Redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = a_datum
    when redeemer is {
      Cancel { index_output } -> {
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
        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        // (2.2) Validate output to fraud prover (output to fraud prover must match the fraud prover pubkey)
        expect
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          )

        // (3.1) Lovelace amount in output to return to the fraud prover
        expect quantity_of(output_to_fraud_prover.value, "", "") == lovelace
        True
      }
      // last step spending validator Continue redeemer for double-spend fraud proof handles:
      // 1) State transition to Void
      // 2) Maintaining fraud_prover field
      // 3) Verifying fraud proof token minting with correct asset name (computation thread token)
      Continue { index_output } -> {
        // 1) Verify state transition to Void while maintaining fraud_prover
        expect Some(output_to_next_step) = list.at(self.outputs, index_output)
        expect InlineDatum(output_to_next_step_datum) =
          output_to_next_step.datum
        expect parsed_output_to_next_step_datum: StepDatum =
          output_to_next_step_datum

        // 2) Maintain fraud_prover field
        let final_datum =
          StepDatum { fraud_prover: step_datum.fraud_prover, data: Void }
        expect final_datum == parsed_output_to_next_step_datum

        // 3) Verify minting of fraud proof token
        // Get computation thread token from inputs
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        // Store output reference to avoid multiple field accesses
        let output = own_input.output
        // (3.1) Verify the computation thread token by asset name in the output
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(output.value), 1)

        // (3.2) Mint fraud proof token
        expect
          self.mint == from_asset(
            policy_id: fraud_proof_token_policy,
            // As expected by the MintRedeemer, using computation thread tokens asset name which contains the fraudulent block hash
            asset_name: ct_token_asset_name,
            quantity: 1,
          )
        True
      }
    }
  }

  else(_) {
    fail
  }
}
// minting policy (token.ak) handles:
//    - Hub oracle validation
//    - Last step input validation
//    - Computation thread token burning
//    - Fraud proof token minting
//    - Output validation (correct address and fraud prover)

// The transaction building should involve both:
// - This last step spending validator (step-04)
// - The minting policy (token.ak) with its own MintRedeemer
// MintRedeemer provided separately to the minting validator by the transaction
