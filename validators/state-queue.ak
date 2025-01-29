use aiken/collection/dict
use aiken/collection/list
use aiken/crypto
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/hub_oracle.{Datum as HubOracleDatum}
use midgard/scheduler.{Datum as SchedulerDatum}
use midgard/state_queue.{
  CommitBlockHeader, Datum, Deinit, Init, MergeToConfirmedState, MintRedeemer,
  RemoveFraudulentBlockHeader,
}

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)

    unordered.list_state_transition(tokens)
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_token_script_hash: PolicyId,
  maturity_duration: Int,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          unordered.init(node_in_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          unordered.deinit(node_outputs, node_mint, policy_id),
        }
      CommitBlockHeader(
        operator,
        scheduler_asset_name,
        hub_oracle_ref_input_index,
        scheduler_state_ref_input_index,
        header_node_output_index,
        previous_header_node_output_index,
        active_operators_redeemer_index,
      ) -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type

        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Some(scheduler_state_ref_input) =
          list.at(self.reference_inputs, scheduler_state_ref_input_index)
        expect InlineDatum(scheduler_state_datum) =
          scheduler_state_ref_input.output.datum
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect Some(header_node_output) =
          list.at(self.outputs, header_node_output_index)
        expect InlineDatum(header_node_datum) = header_node_output.datum
        expect parsed_header_node_datum: NodeDatum = header_node_datum
        expect header_node_block_data: Datum = parsed_header_node_datum.data
        expect parsed_header_node_block_data_as_bytearray: ByteArray =
          parsed_header_node_datum.data
        expect Some(previous_header_node_output) =
          list.at(self.outputs, previous_header_node_output_index)
        expect InlineDatum(previous_header_node_datum) =
          previous_header_node_output.datum
        expect parsed_previous_header_node_datum: NodeDatum =
          previous_header_node_datum
        expect previous_header_node_block_data: Datum =
          parsed_previous_header_node_datum.data
        expect Some(active_operators_redeemer_pair) =
          list.at(self.redeemers, active_operators_redeemer_index)
        expect Spend(..) = active_operators_redeemer_pair.1st
        expect active_operators.UpdateCommitmentTime {
          active_node_input_index,
          ..
        } = active_operators_redeemer_pair.2nd
        expect Some(active_node_input) =
          list.at(self.inputs, active_node_input_index)
        expect InlineDatum(active_node_datum) = active_node_input.output.datum
        expect parsed_active_node_datum: ordered.NodeDatum = active_node_datum
        and {
          list.has(self.extra_signatories, operator),
          unordered.append_unsafe(
            parsed_header_node_datum.key,
            header_node_output_index,
            previous_header_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          operator == header_node_block_data.operator,
          parsed_header_node_datum.key == Key(
            crypto.sha2_256(parsed_header_node_block_data_as_bytearray),
          ),
          quantity_of(
            scheduler_state_ref_input.output.value,
            parsed_hub_oracle_datum.scheduler,
            scheduler_asset_name,
          ) == 1,
          operator == parsed_scheduler_state_datum.operator,
          header_node_block_data.start_time == previous_header_node_block_data.end_time,
          header_node_block_data.end_time == valid_to,
          header_node_block_data.end_time >= parsed_scheduler_state_datum.start_time,
          header_node_block_data.end_time <= parsed_scheduler_state_datum.end_time,
          ordered.prove_is_member(
            parsed_hub_oracle_datum.active_operators,
            parsed_active_node_datum.key,
            active_node_input.output,
          ),
          ordered.Key(operator) == parsed_active_node_datum.key,
        }
      }
      MergeToConfirmedState(
        header_node_input_index,
        confirmed_state_node_input_index,
        confirmed_state_node_output_index,
      ) -> and {
          unordered.remove(
            header_node_key,
            header_node_input_index,
            confirmed_state_node_input_index,
            node_inputs,
            node_outputs,
            node_mint,
          ),
          unordered.is_root_node(confirmed_state_node_key),
          valid_from >= header_node_data.commitment_time + maturity_duration,
          //TODO should we check that key and next of confirmed_state_node_output are still the same?
          header_node_data == confirmed_state_node_output_data,
          //TODO create a settlement queue node here if the merged block contains deposits/withdrawals
          True,
        }
      RemoveFraudulentBlockHeader { fraudulent_operator } -> todo
    }
  }

  else(_) {
    fail
  }
}
