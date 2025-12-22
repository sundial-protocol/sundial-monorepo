use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{plutarch_phas, verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proofs/common.{cancel, validate_step_output}
use midgard/fraud_proofs/double_spend/step_02.{
  Cancel, Continue, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/double_spend/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      Cancel { input_index } ->
        cancel(
          computation_thread_token_policy_id,
          input_index,
          computation_thread_mint_redeemer_index,
          tx,
        )

      Continue {
        input_index,
        output_index,
        double_spent_input_key,
        double_spent_input_value,
        input_membership_proof,
      } -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        expect ct.StepDatum { data: Some(state), fraud_prover }: Datum =
          step_datum

        // 1. Provided input must be a member of the first transaction's inputs.
        expect
          plutarch_phas(
            mpf_root: state.verified_tx1_inputs_root,
            key: double_spent_input_key,
            value: double_spent_input_value,
            membership_proof: input_membership_proof,
          )

        let
          own_input,
          continued_output,
        <-
          singular_utxo_indexer.one_to_one(
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
            // Double satisfaction prevented by validating the output UTxO carries
            // the same computation thread token as the input's.
            True,
          )

        let
          _own_script_hash,
          computation_thread_token_asset_name,
        <-
          authenticate_input_and_get_info(
            own_input,
            computation_thread_token_policy_id,
          )

        // 2. The proven input, along with the unused second transaction must be
        //    carried over in the datum of the next step.
        let expected_output_state: Data =
          step_03.State {
            verified_tx2_inputs_root,
            double_spent_input_key,
            double_spent_input_value,
          }
        expect
          validate_step_output(
            continued_output,
            step_03_validator_script_hash,
            computation_thread_token_policy_id,
            computation_thread_token_asset_name,
            fraud_prover,
            expected_output_state,
            output_index,
          )

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
