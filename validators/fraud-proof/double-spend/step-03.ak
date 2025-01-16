use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/address.{Script}
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MidgardTx, MidgardTxBody}
use midgard/fraud_proof/common/utils.{validate_output_to_fraud_prover}
use midgard/fraud_proof/double_spend/step_02.{Step2State}
use midgard/fraud_proof/double_spend/step_03.{
  Cancel, Continue, Step3Redeemer, Step3State,
}

validator main(
  double_spend_step_04_validator_script_hash: ScriptHash,
  ct_token_policy_id: PolicyId,
) {
  spend(
    a_datum: Option<StepDatum>,
    redeemer: Step3Redeemer,
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
      Continue {
        frauded_input,
        tx1_input_utxos,
        tx1_body,
        tx2_input_utxos,
        tx2_body,
        tx1,
        tx2,
        index_output,
      } -> {
        // get previously verified tx hashes, that are known to be members of the fraudulent block
        expect Step2State { verified_tx1_hash, verified_tx2_hash }: Step2State =
          step_datum.data

        // fraud prover provides a list of output references that are known to be part of the fraudulent block as the tx1, and tx2 full transactions,
        // once serialized and hashed match the verified_tx1_hash and verified_tx2_hash hashes we have verified to be a part of the supposedly 
        // "fraudulent" block in previous steps.
        // serialise(self: Data) -> ByteArray
        // blake2b_256(bytes: ByteArray) -> Hash<Blake2b_256, a>
        expect blake2b_256(cbor.serialise(tx1)) == verified_tx1_hash
        expect blake2b_256(cbor.serialise(tx2)) == verified_tx2_hash

        // tx bodies provided by fraud prover, once serialized and hashed, match the hashed body field part of the inputted transactions
        expect blake2b_256(cbor.serialise(tx1_body)) == tx1.body
        expect blake2b_256(cbor.serialise(tx2_body)) == tx2.body

        // input lists match tx bodies spend_inputs
        expect blake2b_256(tx1_input_utxos) == tx1_body.spend_inputs
        expect blake2b_256(tx2_input_utxos) == tx2_body.spend_inputs

        // both input lists contain the frauded input
        // un_list_data(data: Data) -> List<Data>
        expect list.has(builtin.un_list_data(tx1_input_utxos), frauded_input)
        expect list.has(builtin.un_list_data(tx2_input_utxos), frauded_input)
        expect parsed_frauded_input: OutputReference = frauded_input

        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        // fraud prover field must match between the output state and the input datum
        expect Some(output_to_step_4) = list.at(self.outputs, index_output)
        expect InlineDatum(output_to_step_4_datum) = output_to_step_4.datum
        expect parsed_output_to_step_4_datum: StepDatum = output_to_step_4_datum
        expect
          parsed_output_to_step_4_datum.fraud_prover == step_datum.fraud_prover

        // Verify the state transition from the previous state to the new state
        // (i) Define state transition (input -> output)    
        let step_3_state =
          Step3State {
            verified_tx1_hash,
            verified_tx2_hash,
            verified_common_input: parsed_frauded_input,
          }
        let step_3_datum =
          StepDatum {
            fraud_prover: parsed_output_to_step_4_datum.fraud_prover,
            data: step_3_state,
          }
        // (ii) State transition verified to be correct in the output
        expect step_3_datum == parsed_output_to_step_4_datum

        // Verify that output destination matches the expected hash of the next step spending validator
        expect Script(hash) = output_to_step_4.address.payment_credential
        expect hash == double_spend_step_04_validator_script_hash

        // Computation thread token sent to the next step validator
        expect
          quantity_of(
            output_to_step_4.value,
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
