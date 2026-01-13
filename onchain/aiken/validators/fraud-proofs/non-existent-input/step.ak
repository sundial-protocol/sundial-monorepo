//// ## Non-Existent Input UTxO Fraud Proof (Section 5.1.1)
////
//// Proves a given transaction spends an input that does not exist in the
//// initial ledger (i.e. `prev_utxos_root`) of its containing block.

use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, verify_tx_in_state_queue_node_and_get_header,
}
use midgard/fraud_proofs/non_existent_input/step.{Args, Datum, SpendRedeemer}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        bad_state_queue_node_ref_input_index,
        bad_tx_id,
        bad_tx,
        bad_tx_body,
        bad_tx_membership_proof,
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
          _m_input_state_data,
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

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified bad transactions' inputs included in its datum.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State { verified_tx_inputs_root: bad_tx_body.spend_inputs }
        expect output_state_data == expected_output_state

        // 3. The provided transaction must be valid, and must be included in
        //    the specified state queue node.
        expect _header =
          verify_tx_in_state_queue_node_and_get_header(
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

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
