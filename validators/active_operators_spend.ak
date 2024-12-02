use aiken/collection/list
use aiken/interval.{Finite, Interval}
use cardano/assets.{PolicyId, Value, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use linked_list/data.{Key, NodeDatum}
use linked_list/list as linked_list
use midgard/types/datum.{ActiveOperatorData, HubOracleDatum}
use midgard/types/redeemer.{
  AOSR_List_State_Transition, AOSR_Update_Commitment_Time,
  ActiveOperatorsSpendRedeemer,
}
use midgard/utils as utils

validator active_operators(
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  spend(
    datumOpt: Option<NodeDatum>,
    redeemer: ActiveOperatorsSpendRedeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      AOSR_List_State_Transition ->
        linked_list.list_state_transition(
          tokens(self.mint, active_operators_mint_script_hash),
        )
      AOSR_Update_Commitment_Time(
        active_node_output_index,
        hub_oracle_ref_input_index,
      ) -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect Some(active_node_input_datum) = datumOpt
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Some(active_node_output) =
          list.at(self.outputs, active_node_output_index)
        expect InlineDatum(active_node_output_datum) = active_node_output.datum
        expect parsed_active_node_output_datum: NodeDatum =
          active_node_output_datum
        expect active_node_operator_output_data: ActiveOperatorData =
          parsed_active_node_output_datum.data

        and {
          no_active_operators_minted(
            self.mint,
            active_operators_mint_script_hash,
          ),
          linked_list.is_member(
            parsed_active_node_output_datum.key,
            active_node_input_datum.key,
            active_node_output.address,
            utils.get_own_address(input, self.inputs),
          ),
          //keys are already checked by is_member()
          parsed_active_node_output_datum.link == active_node_input_datum.link,
          Some(valid_to) == active_node_operator_output_data.commitment_time,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          //TODO Add these 2 checks after state queue implementation
          //   linked_list.ordered_append(
          //     key_to_append,
          //     appended_node_output_index,
          //     anchor_node_output_index,
          //     common,
          //   ),
          //   state_queue_redeemer.operator == active_node_output_datum.key,
          True,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn no_active_operators_minted(mint: Value, policy_id: PolicyId) {
  utils.quantity_of_policy_id(mint, policy_id) == 0
}
