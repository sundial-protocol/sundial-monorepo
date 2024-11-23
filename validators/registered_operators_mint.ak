use aiken/collection/list.{length}
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, Value, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use linked_list/data.{Common, Empty, Key, NodeDatum, NodeKey}
use linked_list/list as linked_list
use linked_list/utils as linked_list_utils
use midgard/constants
use midgard/types/config.{PublisherSetConfig}
use midgard/types/datum.{
  FraudProofValidatorDatum, HubOracleDatum, PublisherDatum,
  RegisteredOperatorData,
}
use midgard/types/general.{POSIXTime, PubKeyHash}
use midgard/types/redeemer.{
  ROM_Activate_Operator, ROM_Deinit, ROM_Deregister_Operator, ROM_Init,
  ROM_Register_Operator, ROM_Remove_Duplicate_slash_Bond,
  RegisteredOperatorsMintRedeemer,
}
use midgard/utils

validator registered_operators(
  required_bond: Int,
  init_utxo_id: ByteArray,
  retired_operators_mint_script_hash: PolicyId,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(
    redeemer: RegisteredOperatorsMintRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let (common, inputs, outputs, sigs, vrange) =
      linked_list.make_common(self, policy_id)
    when redeemer is {
      ROM_Init(init_utxo_index) -> {
        expect Some(init_input) = list.at(inputs, init_utxo_index)
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
          init(common, self.mint, hub_oracle_script_hash, init_utxo_id),
        }
      }
      ROM_Deinit ->
        //TODO Should we call here linked_list.list_state_transition()?
        deinit(common, self.mint, hub_oracle_script_hash, init_utxo_id)
      ROM_Register_Operator(
        key_to_prepend,
        hub_oracle_ref_input_index,
        active_operator_ref_input_index,
        active_operator_asset_name,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        prepended_node_output_index,
        anchor_node_output_index,
      ) -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Some(active_operator_ref_input) =
          list.at(self.reference_inputs, active_operator_ref_input_index)
        let active_operator_raw_key_and_link =
          linked_list_utils.get_raw_key_and_link(
            active_operator_ref_input.output,
          )
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        let retired_operator_raw_key_and_link =
          linked_list_utils.get_raw_key_and_link(
            retired_operator_ref_input.output,
          )
        expect Some(registered_node_output) =
          list.at(self.outputs, prepended_node_output_index)
        expect InlineDatum(registered_operator_datum) =
          registered_node_output.datum
        expect parsed_registered_operator_datum: NodeDatum =
          registered_operator_datum
        expect registered_operator_data: RegisteredOperatorData =
          parsed_registered_operator_datum.data
        and {
          //TODO Should we call here linked_list.list_state_transition()?
          linked_list.unordered_prepend(
            Key(key_to_prepend),
            prepended_node_output_index,
            anchor_node_output_index,
            common,
          ),
          list.has(self.extra_signatories, key_to_prepend),
          registered_operator_data.registration_time == valid_to,
          quantity_of(registered_node_output.value, "", "") == required_bond,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          quantity_of(
            active_operator_ref_input.output.value,
            parsed_hub_oracle_datum.active_operators,
            active_operator_asset_name,
          ) == 1,
          linked_list.ordered_is_not_member(
            Key(key_to_prepend),
            linked_list_utils.get_key(active_operator_raw_key_and_link),
            linked_list_utils.get_link(active_operator_raw_key_and_link),
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          linked_list.ordered_is_not_member(
            Key(key_to_prepend),
            linked_list_utils.get_key(retired_operator_raw_key_and_link),
            linked_list_utils.get_link(retired_operator_raw_key_and_link),
          ),
        }
      }

      ROM_Activate_Operator(
        node_to_activate_key,
        hub_oracle_ref_input_index,
        retired_operator_ref_input_index,
        removed_node_input_index,
        anchor_node_input_index,
        active_operators_inserted_node_output_index,
        active_operators_anchor_node_output_index,
      ) -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        let retired_operator_raw_key_and_link =
          linked_list_utils.get_raw_key_and_link(
            retired_operator_ref_input.output,
          )
        and {
          //TODO Should we call here linked_list.list_state_transition()?
          linked_list.remove(
            Key(node_to_activate_key),
            removed_node_input_index,
            anchor_node_input_index,
            common,
          ),
          valid_from >= registration_time + registration_duration,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          linked_list.ordered_insert(
            Key(node_to_activate_key),
            active_operators_inserted_node_output_index,
            active_operators_anchor_node_output_index,
            common,
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          linked_list.ordered_is_not_member(
            Key(node_to_activate_key),
            linked_list_utils.get_key(retired_operator_raw_key_and_link),
            linked_list_utils.get_link(retired_operator_raw_key_and_link),
          ),
        }
      }
      ROM_Deregister_Operator -> True
      ROM_Remove_Duplicate_slash_Bond -> True
    }
  }

  else(_) {
    fail
  }
}

fn init(
  common: Common,
  mint: Value,
  hub_oracle_script_hash: ByteArray,
  init_utxo_id: ByteArray,
) -> Bool {
  and {
    quantity_of(mint, hub_oracle_script_hash, init_utxo_id) == 1,
    linked_list.init(common),
  }
}

fn deinit(
  common: Common,
  mint: Value,
  hub_oracle_script_hash: ByteArray,
  init_utxo_id: ByteArray,
) -> Bool {
  and {
    quantity_of(mint, hub_oracle_script_hash, init_utxo_id) == -1,
    linked_list.deinit(common),
  }
}
// fn insert(
//   common: Common<PublisherDatum>,
//   insert_key: PubKeyHash,
//   prev_node_key: PubKeyHash,
//   validTo: POSIXTime,
//   min_lovelace_bond: Int,
// ) -> Bool {
//   todo
// }

// fn slash(
//   common: Common<PublisherDatum>,
//   prev_node_key: PubKeyHash,
//   remove_node_key: PubKeyHash,
//   outputs: List<Output>,
//   fraud_token_ref_input: Input,
// ) -> Bool {
//   todo
// }

// fn init_refund(
//   common: Common<PublisherDatum>,
//   node_to_mark_key: PubKeyHash,
//   sigs: List<PubKeyHash>,
//   validTo: POSIXTime,
// ) -> Bool {
//   todo
// }

// fn complete_refund(
//   common: Common<PublisherDatum>,
//   prev_node_key: PubKeyHash,
//   remove_node_key: PubKeyHash,
//   sigs: List<PubKeyHash>,
//   validFrom: POSIXTime,
//   refund_waiting_period: Int,
// ) -> Bool {
//   todo
// }
