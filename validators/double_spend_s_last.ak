use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}

use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}

use midgard/types/datum.{StepDatum}
use midgard/types/redeemer.{DS_Cancel, DS_Last_Continue, DoubleSpendStepLastRedeemer}

pub type DoubleSpendStepLastState {
  data: Void,
}

validator double_spend_s_last(
  ct_token_policy_id: PolicyId,
) {
  spend(
    maybe_datum: Option<StepDatum>, // StepDatum { fraud_prover, data }
    redeemer: DoubleSpendStepLastRedeemer, // DS_Cancel | DS_Continue
    input: OutputReference, // UTxO spent from the validator with the StepDatum { fraud_prover, Void } (Step Last - 1)
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

      // --------- PURPOSE - LastContinue Redeemer-------------------------------
      // (1) Mint the fraud token, which will implicitly burn the computation thread token with the Success redeemer. Let output state be that transaction output
      // (2) The fraud prover field must match between the output state and the input datum
      // (3) The output state datum type must be StepDatum(Void)
    
      DS_Last_Continue {
        frauded_state_queue_node_ref_input_index,
        // TODO: add other fields
      } -> {
        // (1.1) Mint the fraud token, which will implicitly burn the computation thread token with the Success redeemer. Let output state be that transaction output
        // todo ct_token_burnt,
        // todo fraud_proof_token_minted,

        // (2.1) The fraud prover field must match between the output state and the input datum
        expect Some(output_to_finish) = list.at(self.outputs, 0)
        expect InlineDatum(output_to_finish_datum) = output_to_finish.datum
        expect parsed_output_to_finish_datum: StepDatum = output_to_finish_datum
        
        // (3.1) Define the state transition        
        let step_last_state =
          DoubleSpendStepLastState { data: Void }
        let step_last_datum =
          StepDatum {
            fraud_prover: parsed_output_to_finish_datum.fraud_prover,
            data: step_last_state,
          }

        and {
          // todo ct_token_burnt(),
          // todo fraud_proof_token_minted(),
          parsed_output_to_finish_datum.fraud_prover == step_datum.fraud_prover, // (2.2) fraud prover field matches
          step_last_datum == parsed_output_to_finish_datum, // (3.2) state transition verified
        }
      }
    }
  }

  else(_) {
    fail
  }
}

