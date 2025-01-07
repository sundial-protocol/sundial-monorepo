use aiken/collection/list
use aiken/interval.{Finite, Interval}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use linked_list/ordered
use linked_list/types.{Key, NodeDatum}
use linked_list/utils.{get_node_in_outputs,
  get_node_outputs} as linked_list_utils
use midgard/active_operators.{
  ActivateOperator, Datum, Deinit, Init, ListStateTransition, MintRedeemer,
  RemoveOperatorSlashBond, RetireOperator, SpendRedeemer, UpdateCommitmentTime,
}
use midgard/common/utils as utils
use midgard/hub_oracle.{Datum as HubOracleDatum}
use midgard/registered_operators
use midgard/retired_operators.{Datum as RetiredOperatorData}
use midgard/state_queue.{CommitBlockHeader, RemoveFraudolentBlockHeader}

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
      ListStateTransition ->
        ordered.list_state_transition(
          tokens(self.mint, active_operators_mint_script_hash),
        )
      UpdateCommitmentTime(
        active_node_output_index,
        hub_oracle_ref_input_index,
        state_queue_redeemer_index,
      ) -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect Some(active_node_input_datum) = datumOpt
        expect Key(active_node_input_datum_key) = active_node_input_datum.key
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

        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect CommitBlockHeader(operator) = state_queue_redeemer_pair.2nd
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
          //active_node_output must hold the correct active_operators token
          quantity_of(
            active_node_output.value,
            active_operators_mint_script_hash,
            active_node_input_datum_key,
          ) > 0,
          //keys are already checked by is_member()
          parsed_active_node_output_datum.link == active_node_input_datum.link,
          Some(valid_to) == active_node_operator_output_data.commitment_time,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          state_queue_redeemer_pair.1st == Mint(state_queue_cs),
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
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_in_outputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          ordered.init(node_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          ordered.deinit(node_in_outputs, node_mint, policy_id),
        }
      ActivateOperator(
        new_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_appended_node_output_index,
        active_operator_anchor_node_output_index,
        registered_operators_redeemer_index,
      ) -> {
        expect Some(active_node_output) =
          list.at(node_outputs, active_operator_appended_node_output_index)
        expect InlineDatum(active_operator_datum) = active_node_output.datum
        expect parsed_active_operator_datum: NodeDatum = active_operator_datum
        expect active_operator_data: Datum = parsed_active_operator_datum.data
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Script(registered_operators_cs) =
          parsed_hub_oracle_datum.registered_operators_addr.payment_credential
        expect Some(registered_operators_redeemer_pair) =
          list.at(self.redeemers, registered_operators_redeemer_index)
        expect registered_operators.Activate { node_to_activate_key, .. } =
          registered_operators_redeemer_pair.2nd

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
          registered_operators_redeemer_pair.1st == Mint(
            registered_operators_cs,
          ),
          node_to_activate_key == new_active_operator_key,
        }
      }
      RemoveOperatorSlashBond(
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_node_input_index,
        state_queue_redeemer_index,
      ) -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Script(state_queue_cs) =
          parsed_hub_oracle_datum.state_queue_addr.payment_credential
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect RemoveFraudolentBlockHeader(fraudolent_operator) =
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
          fraudolent_operator == slashed_active_operator_key,
        }
      }
      RetireOperator(
        active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_node_input_index,
        retired_operator_inserted_node_output_index,
        retired_operators_redeemer_index,
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
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Script(retired_operators_cs) =
          parsed_hub_oracle_datum.retired_operators_addr.payment_credential
        expect Some(retired_operators_redeemer_pair) =
          list.at(self.redeemers, retired_operators_redeemer_index)
        expect retired_operators.RetireOperator { new_retire_operator_key, .. } =
          retired_operators_redeemer_pair.2nd
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
          retired_operators_redeemer_pair.1st == Mint(retired_operators_cs),
          new_retire_operator_key == active_operator_key,
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
