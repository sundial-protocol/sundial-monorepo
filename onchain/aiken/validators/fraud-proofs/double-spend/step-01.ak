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

// ----------------- DOUBLE SPEND FRAUD PROOF: DEFINITION --------------------------
// Proves membership Merkle proofs for 2 unique txs which spends the same input UTxO

// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2):
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

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
    // expect Some(own_input) =
    //   list.find(tx.inputs, fn(a_input) { a_input.output_reference == input })

    when redeemer is {
      Cancel { input_index } ->
        cancel(
          computation_thread_token_policy_id,
          input_index,
          computation_thread_mint_redeemer_index,
          tx,
        )
      // Step 1: Membership proof for tx1
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
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
          tx

        expect Some(step_datum) = datum

        let ct.StepDatum { data: m_data, fraud_prover }: Datum = step_datum

        // 1. Hub oracle must be provided as a reference input.
        let hub.Datum { state_queue, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 2. The asset name of the computation thread token must match the
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
        // TODO: Do we need the computation thread token to be prefixed with
        //       the fraud category ID?
        expect
          state_queue_node_key == bytearray.drop(
            computation_thread_token_asset_name,
            fraud_category_id_byte_count,
          )

        // 3. Specified bad transaction must be proven to be included in the
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

        // 4. Provided transaction body must match the hash retrieved from
        //    `bad_tx`.
        let bad_tx_body: Data = bad_tx_body
        expect utils.verify_hash_32(data: bad_tx_body, data_hash: bad_tx.body)

        when m_data is {
          None -> {
            expect Script(own_script_hash) = input.address.payment_credential

            // 5a. The UTxO must be produced at own script hash with the
            //     verified first bad transaction included in its datum.
            let expected_output_state: Data =
              State { verified_tx1_body: bad_tx }
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
            // 5b. The UTxO must be produced at next step's script hash with the
            //     verified first bad transaction included in its datum.
            let expected_output_state: Data =
              step_02.State {
                verified_tx1_body: step_1_state.verified_tx1_body,
                verified_tx2_body: bad_tx,
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
