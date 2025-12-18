//// ## Double Spend Fraud Proof
////
//// Proves 2 unique transactions exist such that both spend the same UTxO.
////
//// First step's contract is responsible for verifying inclusion of the two
//// transactions in a Midgard block, and passing them to the second step's
//// contract.
////
//// This is meant to take place in two transactions:
//// 1. First transaction spends the initialized fraud proof UTxO (which has no
////    `data`) and reproduces the UTxO at own address with `data` containing
////    the first verified bad transaction's body.
//// 2. Second transaction spends the UTxO produced in the first transaction,
////    and produces the UTxO at the next step's script address with `data`
////    containing both verified bad transactions' bodies.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/constants.{fraud_category_id_byte_count}
use midgard/common/utils.{plutarch_phas}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  MembershipProofInputs, cancel, get_singleton_ct_token,
  get_tx_root_validate_block_hash, validate_step_output,
}
use midgard/fraud_proofs/double_spend/step_01.{
  Cancel, Continue, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/double_spend/step_02
use midgard/hub_oracle as hub

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
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
        hub_ref_input_index,
        bad_state_queue_node_ref_input_index,
        bad_tx_id,
        bad_tx_body,
        bad_tx,
        bad_tx_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      } -> {
        // 1. Verify authenticity of the provided bad tx against its ID.
        let bad_tx_id_data: Data = bad_tx_id
        let bad_tx_data: Data = bad_tx
        expect verify_hash_32(data: bad_tx_data, data_hash: bad_tx_id_data)

        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
          tx

        expect Some(step_datum) = datum

        let ct.StepDatum { data: m_data, fraud_prover }: Datum = step_datum

        // 2. Hub oracle must be provided as a reference input.
        let hub.Datum { state_queue, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 3. The asset name of the computation thread token must match the
        //    state queue node key (after dropping the fraud category ID prefix).
        let
          state_queue_datum,
          state_queue_node_key,
        <-
          state_queue.get_block_datum(
            reference_inputs,
            state_queue_policy,
            state_queue_node_ref_input_index,
          )
        let
          own_input,
          computation_thread_token_asset_name,
        <-
          utils.get_authentic_input_and_asset_name_with_policy_at(
            inputs,
            computation_thread_token_policy_id,
            input_index,
          )
        let Input { output_reference, output: own_inputs_output } = own_input
        expect output_reference == own_out_ref
        // TODO: Do we need the computation thread token to be prefixed with
        //       the fraud category ID?
        expect
          state_queue_node_key == bytearray.drop(
            computation_thread_token_asset_name,
            fraud_category_id_byte_count,
          )

        // 4. Specified bad transaction must be proven to be included in the
        //    referenced state queue node.
        let bad_tx_data: Data = bad_tx
        expect
          plutarch_phas(
            mpf_root: state_queue_datum.transactions_root,
            key: builtin.b_data(bad_tx_id),
            value: bad_tx_data,
            membership_proof: bad_tx_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 5. Provided transaction body must match the hash retrieved from
        //    `bad_tx`.
        let bad_tx_body_data: Data = bad_tx_body
        let bad_tx_body_hash: Data = bad_tx.body
        expect
          utils.verify_hash_32(
            data: bad_tx_body_data,
            data_hash: bad_tx_body_hash,
          )

        when m_data is {
          None -> {
            expect Script(own_script_hash) = own_inputs_output.address.payment_credential

            // 6a. The UTxO must be produced at own script hash with the
            //     verified first bad transaction ID and its inputs included in
            //     the `data` field of its `StepDatum`.
            let expected_output_state: Data =
              State {
                verified_tx1_id: bad_tx_id,
                verified_tx1_inputs_root: bad_tx_body.spend_inputs,
              }
            expect
              validate_step_output(
                outputs,
                own_script_hash,
                computation_thread_token_policy_id,
                computation_thread_token_asset_name,
                fraud_prover,
                expected_output_state,
                output_index,
              )

            // Done.
            True
          }
          Some(step_1_state) -> {
            // 6b. The UTxO must be produced at next step's script hash with the
            //     verified bad transactions' inputs included in its datum.
            let State { verified_tx1_id, verified_tx1_inputs_root } =
              step_1_state
            let expected_output_state: Data =
              step_02.State {
                verified_tx1_inputs_root,
                verified_tx2_inputs_root: bad_tx_body.spend_inputs,
              }
            expect
              validate_step_output(
                outputs,
                step_02_validator_script_hash,
                computation_thread_token_policy_id,
                computation_thread_token_asset_name,
                fraud_prover,
                expected_output_state,
                output_index,
              )

            // 7b. The two transactions must be different.
            expect verified_tx1_id != bad_tx_id

            // Done.
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
