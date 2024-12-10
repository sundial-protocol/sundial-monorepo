use aiken/collection/list
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use linked_list/ordered
use linked_list/types.{Key, NodeDatum}
use linked_list/unordered
use linked_list/utils.{get_node_in_outputs,
  get_node_outputs} as linked_list_utils
use midgard/active_operators.{
  Activate_Operator, Datum, Deinit, Init, List_State_Transition, MintRedeemer,
  Remove_Operator_Slash_Bond, Retire_Operator, SpendRedeemer,
  Update_Commitment_Time,
}
use midgard/common/utils as utils
use midgard/hub_oracle.{Datum as HubOracleDatum}
use midgard/retired_operators.{Datum as RetiredOperatorData}
use midgard/state_queue.{Commit_Block_Header, Remove_Fraudolent_Block_Header}

validator spend(
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  spend(
    datumOpt: Option<NodeDatum>,
    redeemer: SpendRedeemer,
    _input: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      List_State_Transition ->
        ordered.list_state_transition(
          tokens(self.mint, active_operators_mint_script_hash),
        )
      Update_Commitment_Time(
        active_node_output_index,
        hub_oracle_ref_input_index,
        state_queue_redeemer_index,
        state_queue_appended_node_output_index,
        state_queue_anchor_node_output_index,
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
        expect active_node_operator_output_data: Datum =
          parsed_active_node_output_datum.data

        expect Script(state_queue_cs) =
          parsed_hub_oracle_datum.state_queue_addr.payment_credential
        let state_queue_node_in_outputs =
          get_node_in_outputs(self.inputs, state_queue_cs)
        let state_queue_node_outputs =
          get_node_outputs(self.outputs, state_queue_cs)
        let state_queue_node_mint = tokens(self.mint, state_queue_cs)
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect Commit_Block_Header(operator) = state_queue_redeemer_pair.2nd
        and {
          no_active_operators_minted(
            self.mint,
            active_operators_mint_script_hash,
          ),
          ordered.is_member(
            parsed_active_node_output_datum.key,
            active_node_input_datum.key,
            active_node_output.address,
            parsed_hub_oracle_datum.active_operators_addr,
          ),
          //keys are already checked by is_member()
          parsed_active_node_output_datum.link == active_node_input_datum.link,
          Some(valid_to) == active_node_operator_output_data.commitment_time,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          state_queue_redeemer_pair.1st == Mint(state_queue_cs),
          unordered.append(
            Key(operator),
            state_queue_appended_node_output_index,
            state_queue_anchor_node_output_index,
            state_queue_node_in_outputs,
            state_queue_node_outputs,
            state_queue_node_mint,
          ),
          Key(operator) == parsed_active_node_output_datum.key,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  slashing_penalty: Int,
  init_utxo_id: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_in_outputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init(init_utxo_index) -> {
        expect Some(init_input) = list.at(self.inputs, init_utxo_index)
        let init_input_utxo_id =
          sha2_256(
            bytearray.push(
              init_input.output_reference.transaction_id,
              init_input.output_reference.output_index,
            ),
          )
        and {
          //TODO Should we call here ordered.list_state_transition()?
          init_input_utxo_id == init_utxo_id,
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == 1,
          ordered.init(node_outputs, node_mint, policy_id),
        }
      }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == -1,
          ordered.deinit(node_in_outputs, node_mint, policy_id),
        }
      Activate_Operator(
        new_active_operator_key,
        hub_oracle_ref_input_index,
        registered_operator_removed_node_input_index,
        registered_operator_anchor_node_input_index,
        active_operator_appended_node_output_index,
        active_operator_anchor_node_output_index,
      ) -> {
        expect Some(active_node_output) =
          list.at(self.outputs, active_operator_appended_node_output_index)
        expect InlineDatum(active_operator_datum) = active_node_output.datum
        expect parsed_active_operator_datum: NodeDatum = active_operator_datum
        expect active_operator_data: Datum = parsed_active_operator_datum.data
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        let registered_operators_node_in_outputs =
          get_node_in_outputs(
            self.inputs,
            parsed_hub_oracle_datum.registered_operators,
          )
        let registered_operators_node_outputs =
          get_node_outputs(
            self.outputs,
            parsed_hub_oracle_datum.registered_operators,
          )
        let registered_operators_node_mint =
          tokens(self.mint, parsed_hub_oracle_datum.registered_operators)

        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.append(
            Key(new_active_operator_key),
            active_operator_appended_node_output_index,
            active_operator_anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          active_operator_data.commitment_time == None,
          ordered.remove(
            Key(new_active_operator_key),
            registered_operator_removed_node_input_index,
            registered_operator_anchor_node_input_index,
            registered_operators_node_in_outputs,
            registered_operators_node_outputs,
            registered_operators_node_mint,
          ),
        }
      }
      Remove_Operator_Slash_Bond(
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_node_input_index,
        state_queue_redeemer_index,
        state_queue_removed_node_input_index,
        state_queue_anchor_node_input_index,
      ) -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Script(state_queue_cs) =
          parsed_hub_oracle_datum.state_queue_addr.payment_credential
        let state_queue_node_in_outputs =
          get_node_in_outputs(self.inputs, state_queue_cs)
        let state_queue_node_outputs =
          get_node_outputs(self.outputs, state_queue_cs)
        let state_queue_node_mint = tokens(self.mint, state_queue_cs)
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect Remove_Fraudolent_Block_Header(fraudolent_operator) =
          state_queue_redeemer_pair.2nd
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.remove(
            Key(slashed_active_operator_key),
            active_operator_slashed_node_input_index,
            active_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          self.fee >= slashing_penalty,
          state_queue_redeemer_pair.1st == Mint(state_queue_cs),
          ordered.remove(
            Key(fraudolent_operator),
            state_queue_removed_node_input_index,
            state_queue_anchor_node_input_index,
            state_queue_node_in_outputs,
            state_queue_node_outputs,
            state_queue_node_mint,
          ),
          fraudolent_operator == slashed_active_operator_key,
        }
      }
      Retire_Operator(
        active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_node_input_index,
        retired_operator_inserted_node_output_index,
        retired_operators_anchor_node_output_index,
      ) -> {
        expect Some(active_node_input) =
          list.at(self.inputs, active_operator_removed_node_input_index)
        expect InlineDatum(active_operator_datum) =
          active_node_input.output.datum
        expect parsed_active_operator_datum: NodeDatum = active_operator_datum
        expect active_operator_data: Datum = parsed_active_operator_datum.data
        expect Some(retired_node_output) =
          list.at(self.outputs, retired_operator_inserted_node_output_index)
        expect InlineDatum(retired_operator_datum) = retired_node_output.datum
        expect parsed_retired_operator_datum: NodeDatum = retired_operator_datum
        expect retired_operator_data: RetiredOperatorData =
          parsed_retired_operator_datum.data

        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        let retired_operators_node_in_outputs =
          get_node_in_outputs(
            self.inputs,
            parsed_hub_oracle_datum.retired_operators,
          )
        let retired_operators_node_outputs =
          get_node_outputs(
            self.outputs,
            parsed_hub_oracle_datum.retired_operators,
          )
        let retired_operators_node_mint =
          tokens(self.mint, parsed_hub_oracle_datum.retired_operators)
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.remove(
            Key(active_operator_key),
            active_operator_removed_node_input_index,
            active_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          ordered.insert(
            Key(active_operator_key),
            retired_operator_inserted_node_output_index,
            retired_operators_anchor_node_output_index,
            retired_operators_node_in_outputs,
            retired_operators_node_outputs,
            retired_operators_node_mint,
          ),
          active_operator_data.commitment_time == retired_operator_data.commitment_time,
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
