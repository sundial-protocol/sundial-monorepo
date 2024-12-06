use aiken/collection/list
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use linked_list/ordered
use linked_list/types.{Key, NodeDatum}
use linked_list/unordered
use linked_list/utils as linked_list_utils
use midgard/hub_oracle
use midgard/registered_operators.{
  Activate, Active, Datum, Deinit, Deregister, Init, MintRedeemer, Register,
  Registered, Remove_Duplicate_Slash_Bond, Retired,
}

validator spend(registered_operators_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    unordered.list_state_transition(
      tokens(self.mint, registered_operators_mint_script_hash),
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  required_bond: Int,
  registration_duration: Int,
  slashing_penalty: Int,
  init_utxo_id: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let common = unordered.make_common(self, policy_id)
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
          //TODO Should we call here unordered.list_state_transition()?
          init_input_utxo_id == init_utxo_id,
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == 1,
          unordered.init(common),
        }
      }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == -1,
          unordered.deinit(common),
        }
      Register(
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
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
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
        expect registered_operator_data: Datum =
          parsed_registered_operator_datum.data
        and {
          unordered.prepend(
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
          ordered.is_not_member(
            Key(key_to_prepend),
            linked_list_utils.get_key(active_operator_raw_key_and_link),
            linked_list_utils.get_link(active_operator_raw_key_and_link),
            registered_node_output.address,
            active_operator_ref_input.output.address,
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          ordered.is_not_member(
            Key(key_to_prepend),
            linked_list_utils.get_key(retired_operator_raw_key_and_link),
            linked_list_utils.get_link(retired_operator_raw_key_and_link),
            registered_node_output.address,
            retired_operator_ref_input.output.address,
          ),
        }
      }

      Activate(
        node_to_activate_key,
        hub_oracle_ref_input_index,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        removed_node_input_index,
        anchor_node_input_index,
        active_operators_inserted_node_output_index,
        active_operators_anchor_node_output_index,
      ) -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        let retired_operator_raw_key_and_link =
          linked_list_utils.get_raw_key_and_link(
            retired_operator_ref_input.output,
          )
        expect Some(removed_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect InlineDatum(removed_node_datum) = removed_node_input.output.datum
        expect parsed_removed_node_datum: NodeDatum = removed_node_datum
        expect registered_operator_data: Datum = parsed_removed_node_datum.data
        expect Some(active_operators_inserted_node_output) =
          list.at(self.outputs, active_operators_inserted_node_output_index)
        let active_operators_common =
          unordered.make_common(self, parsed_hub_oracle_datum.active_operators)
        and {
          unordered.remove(
            Key(node_to_activate_key),
            removed_node_input_index,
            anchor_node_input_index,
            common,
          ),
          valid_from >= registered_operator_data.registration_time + registration_duration,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.insert(
            Key(node_to_activate_key),
            active_operators_inserted_node_output_index,
            active_operators_anchor_node_output_index,
            active_operators_common,
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          parsed_hub_oracle_datum.retired_operators_addr == retired_operator_ref_input.output.address,
          ordered.is_not_member(
            Key(node_to_activate_key),
            linked_list_utils.get_key(retired_operator_raw_key_and_link),
            linked_list_utils.get_link(retired_operator_raw_key_and_link),
            active_operators_inserted_node_output.address,
            retired_operator_ref_input.output.address,
          ),
        }
      }
      Deregister(
        node_to_deregister_key,
        removed_node_input_index,
        anchor_node_input_index,
      ) -> and {
          unordered.remove(
            Key(node_to_deregister_key),
            removed_node_input_index,
            anchor_node_input_index,
            common,
          ),
          list.has(self.extra_signatories, node_to_deregister_key),
        }

      Remove_Duplicate_Slash_Bond(
        duplicate_node_key,
        hub_oracle_ref_input_index,
        duplicate_node_ref_input_index,
        removed_node_input_index,
        anchor_node_input_index,
        witness_status,
      ) -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Some(removed_node_input) =
          list.at(self.inputs, removed_node_input_index)
        and {
          unordered.remove(
            Key(duplicate_node_key),
            removed_node_input_index,
            anchor_node_input_index,
            common,
          ),
          self.fee >= slashing_penalty,
          when witness_status is {
            Registered -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              let duplicate_node_raw_key_and_link =
                linked_list_utils.get_raw_key_and_link(
                  duplicate_node_ref_input.output,
                )
              unordered.is_member(
                Key(duplicate_node_key),
                linked_list_utils.get_key(duplicate_node_raw_key_and_link),
                removed_node_input.output.address,
                parsed_hub_oracle_datum.registered_operators_addr,
              )
            }
            Active -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              let duplicate_node_raw_key_and_link =
                linked_list_utils.get_raw_key_and_link(
                  duplicate_node_ref_input.output,
                )
              unordered.is_member(
                Key(duplicate_node_key),
                linked_list_utils.get_key(duplicate_node_raw_key_and_link),
                removed_node_input.output.address,
                parsed_hub_oracle_datum.active_operators_addr,
              )
            }
            Retired -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              let duplicate_node_raw_key_and_link =
                linked_list_utils.get_raw_key_and_link(
                  duplicate_node_ref_input.output,
                )
              unordered.is_member(
                Key(duplicate_node_key),
                linked_list_utils.get_key(duplicate_node_raw_key_and_link),
                removed_node_input.output.address,
                parsed_hub_oracle_datum.retired_operators_addr,
              )
            }
          },
        }
      }
    }
  }

  else(_) {
    fail
  }
}
