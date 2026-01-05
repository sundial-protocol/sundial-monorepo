use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/unordered as unordered
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_bad_tx_in_state_queue_node_and_get_header}
// use midgard/fraud_proofs/common.{
//   MembershipProofInputs, get_singleton_ct_token, get_tx_root_validate_block_hash,
//   handle_cancel, validate_final_step_output, validate_membership_proof,
// }
use midgard/fraud_proofs/invalid_range/step.{Args, Datum, SpendRedeemer}
use midgard/hub_oracle as hub
use midgard/ledger_state.{Header}

validator main(
  fraud_proof_token_policy: PolicyId,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    input: OutputReference,
    tx: Transaction,
  ) {
    expect Some(step_datum) = datum
    expect Some(own_input) =
      list.find(tx.inputs, fn(a_input) { a_input.output_reference == input })

    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      // Step 1: Membership proof for tx1
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        bad_state_queue_node_ref_input_index,
        bad_tx_id,
        bad_tx,
        bad_tx_body,
        bad_tx_membership_proof,
        computation_thread_redeemer_index,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
          tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
          own_script_hash,
          computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
        <-
          continue(
            computation_thread_token_policy_id,
            step_datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
          )

        // 2. The provided transaction must be valid, and must be included in
        //    the specified state queue node.
        let header =
          verify_bad_tx_in_state_queue_node_and_get_header(
            bad_tx_id,
            bad_tx,
            bad_tx_body,
            bad_tx_membership_proof,
            computation_thread_token_asset_name,
            hub_oracle,
            hub_ref_input_index,
            bad_state_queue_node_ref_input_index,
            inclusion_proof_script_withdraw_redeemer_index,
            reference_inputs,
            redeemers,
          )
        let block_valid_from = header.start_time
        let block_valid_to = header.end_time

        expect Finite(valid_from) =
          bad_tx_body.validity_interval.lower_bound.bound_type

        expect Finite(valid_to) =
          bad_tx_body.validity_interval.upper_bound.bound_type

        //Validity range not completely contained in the block's validity range
        expect valid_from < block_valid_from || valid_to > block_valid_to

        // -------- Invalid range: validation-specific logic END --------
        expect
          validate_final_step_output(
            tx,
            index_output,
            fraud_proof_token_policy,
            ct_token_asset_name,
            step_datum,
            computation_thread_redeemer_index,
          )

        True
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn get_validity_range_validate_block_hash(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
) -> (Int, Int) {
  // Reference input with block_hash 
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  (parsed_state_queue_datum.start_time, parsed_state_queue_datum.end_time)
}
