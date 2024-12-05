use aiken/collection/list
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{InlineDatum, Input, Mint, Output, Transaction}
use linked_list/data.{Key, NodeDatum}
use linked_list/list as linked_list
use midgard/types/datum.{HubOracleDatum, RetiredOperatorData}
use midgard/types/redeemer.{
  ROMR_Deinit, ROMR_Init, ROMR_Recover_Operator_Bond,
  ROMR_Remove_Operator_Slash_Bond, ROMR_Retire_Operator,
  RetiredOperatorsMintRedeemer, StateQueueMintRedeemer,
}

validator retired_operators(
  maturity_duration: Int,
  slashing_penalty: Int,
  init_utxo_id: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(
    redeemer: RetiredOperatorsMintRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let common = linked_list.make_common(self, policy_id)
    when redeemer is {
      ROMR_Init(init_utxo_index) -> {
        expect Some(init_input) = list.at(self.inputs, init_utxo_index)
        let init_input_utxo_id =
          sha2_256(
            bytearray.push(
              init_input.output_reference.transaction_id,
              init_input.output_reference.output_index,
            ),
          )
        and {
          //TODO Should we call here linked_list.list_state_transition()?
          init_input_utxo_id == init_utxo_id,
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == 1,
          linked_list.init(common),
        }
      }
      ROMR_Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == -1,
          linked_list.deinit(common),
        }
      ROMR_Retire_Operator(
        new_retired_operator_key,
        hub_oracle_ref_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_node_input_index,
        retired_operator_appended_node_output_index,
        retired_operator_anchor_node_output_index,
      ) -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        let active_operators_common =
          linked_list.make_common(
            self,
            parsed_hub_oracle_datum.active_operators,
          )
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          linked_list.ordered_append(
            Key(new_retired_operator_key),
            retired_operator_appended_node_output_index,
            retired_operator_anchor_node_output_index,
            common,
          ),
          linked_list.remove(
            Key(new_retired_operator_key),
            active_operator_removed_node_input_index,
            active_operator_anchor_node_input_index,
            active_operators_common,
          ),
        }
      }
      ROMR_Recover_Operator_Bond(
        retired_operator_key,
        removed_node_input_index,
        anchor_node_input_index,
      ) -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(retired_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect InlineDatum(retired_operator_datum) =
          retired_node_input.output.datum
        expect parsed_retired_operator_datum: NodeDatum = retired_operator_datum
        expect retired_operator_data: RetiredOperatorData =
          parsed_retired_operator_datum.data
        and {
          linked_list.remove(
            Key(retired_operator_key),
            removed_node_input_index,
            anchor_node_input_index,
            common,
          ),
          when retired_operator_data.commitment_time is {
            None -> True
            Some(commitment_time) ->
              valid_from >= commitment_time + maturity_duration
          },
        }
      }
      ROMR_Remove_Operator_Slash_Bond(
        slashed_retired_operator_key,
        hub_oracle_ref_input_index,
        retired_operator_slashed_node_input_index,
        retired_operator_anchor_node_input_index,
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
        let state_queue_common = linked_list.make_common(self, state_queue_cs)
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect state_queue_redeemer: StateQueueMintRedeemer =
          state_queue_redeemer_pair.2nd
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          linked_list.remove(
            Key(slashed_retired_operator_key),
            retired_operator_slashed_node_input_index,
            retired_operator_anchor_node_input_index,
            common,
          ),
          self.fee >= slashing_penalty,
          state_queue_redeemer_pair.1st == Mint(state_queue_cs),
          linked_list.remove(
            Key(state_queue_redeemer.operator),
            state_queue_removed_node_input_index,
            state_queue_anchor_node_input_index,
            state_queue_common,
          ),
          state_queue_redeemer.operator == slashed_retired_operator_key,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
