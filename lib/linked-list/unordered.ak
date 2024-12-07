use aiken/collection/dict.{Dict}
use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, flatten, quantity_of, tokens}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use linked_list/constants
use linked_list/types.{Common, Empty, NodeDatum, NodeKey}
use linked_list/utils.{
  get_key, get_link, get_node_in_outputs, get_node_outputs, get_raw_key_and_link,
  serialize_key,
}

pub fn make_common(transaction: Transaction, own_cs: PolicyId) -> Common {
  let Transaction { inputs, outputs, mint, .. } = transaction
  let from_node_val = get_node_in_outputs(inputs, own_cs)
  let to_node_val = get_node_outputs(outputs, own_cs)
  expect Some(head) = list.head(list.concat(from_node_val, to_node_val))
  let Output { address: node_address, .. } = head
  //Check that all nodes (inputs and outputs) reside in the same address
  expect
    from_node_val
      |> list.concat(to_node_val)
      |> list.reduce(
          True,
          fn(acc, cur_node) {
            let Output { address: cur_address, .. } = cur_node
            and {
              cur_address == node_address,
              acc,
            }
          },
        )
  Common {
    node_cs: own_cs,
    node_mint: tokens(mint, own_cs),
    node_in_outputs: from_node_val,
    node_outputs: to_node_val,
  }
}

pub fn list_state_transition(node_mint: Dict<AssetName, Int>) {
  dict.is_empty(node_mint) == False
}

pub fn modify_data(
  node_mint: Dict<AssetName, Int>,
  own_input: Input,
  own_output: Output,
  node_nft_policy_id: ByteArray,
  node_nft_asset_name: ByteArray,
) -> Bool {
  let input_key_and_link = get_raw_key_and_link(own_input.output)
  let output_key_and_link = get_raw_key_and_link(own_output)

  and {
    input_key_and_link == output_key_and_link,
    dict.is_empty(node_mint),
    quantity_of(own_input.output.value, node_nft_policy_id, node_nft_asset_name) == 1,
    quantity_of(own_output.value, node_nft_policy_id, node_nft_asset_name) == 1,
  }
}

pub fn is_root_node(node_key: NodeKey) {
  node_key == Empty
}

pub fn is_last_node(node_link: NodeKey) {
  node_link == Empty
}

pub fn is_empty_list(node_key: NodeKey, node_link: NodeKey) {
  and {
    is_root_node(node_key),
    is_last_node(node_link),
  }
}

pub fn key_added(key: NodeKey, node_mint: Dict<AssetName, Int>) {
  and {
    dict.size(node_mint) == 1,
    dict.get(node_mint, serialize_key(key)) == Some(1),
  }
}

pub fn key_removed(key: NodeKey, node_mint: Dict<AssetName, Int>) {
  and {
    dict.size(node_mint) == 1,
    dict.get(node_mint, serialize_key(key)) == Some(-1),
  }
}

pub fn is_member(
  k: NodeKey,
  node_key: NodeKey,
  node_address: Address,
  linked_list_address: Address,
) {
  k == node_key && node_address == linked_list_address
}

//Application code must ensure that this action can happen only once
pub fn init(common: Common) {
  expect Some(output_root_node) = list.head(common.node_outputs)
  let raw_key_and_link = get_raw_key_and_link(output_root_node)
  and {
    key_added(Empty, common.node_mint),
    is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link)),
    quantity_of(output_root_node.value, common.node_cs, constants.node_prefix) == 1,
    list.length(flatten(output_root_node.value)) == 2,
  }
}

pub fn deinit(common: Common) {
  expect Some(input_root_node) = list.head(common.node_in_outputs)
  let raw_key_and_link = get_raw_key_and_link(input_root_node)
  and {
    key_removed(Empty, common.node_mint),
    is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link)),
    quantity_of(input_root_node.value, common.node_cs, constants.node_prefix) == 1,
  }
}

//The indexes are relative to utxos of this linkedlist, they are NOT absolute
pub fn prepend(
  key_to_prepend: NodeKey,
  prepended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  expect Some(anchor_node_input) = list.head(common.node_in_outputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  expect Some(prepended_node_output) =
    list.at(common.node_outputs, prepended_node_output_index)
  expect Some(anchor_node_output) =
    list.at(common.node_outputs, anchor_node_output_index)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  let prepended_node_output_raw_key_and_link =
    get_raw_key_and_link(prepended_node_output)

  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_prepend,
      data: parsed_anchor_node_input_datum.data,
    }
  and {
    key_added(key_to_prepend, common.node_mint),
    list.length(common.node_in_outputs) == 1,
    is_root_node(parsed_anchor_node_input_datum.key),
    list.length(common.node_outputs) == 2,
    is_member(
      key_to_prepend,
      get_key(prepended_node_output_raw_key_and_link),
      prepended_node_output.address,
      anchor_node_input.address,
    ),
    parsed_anchor_node_input_datum.link == get_link(
      prepended_node_output_raw_key_and_link,
    ),
    anchor_node_input.address == anchor_node_output.address,
    anchor_node_input.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
    //TODO This is already checked in is_member
    prepended_node_output.address == anchor_node_output.address,
    list.length(flatten(prepended_node_output.value)) == 2,
  }
}

pub fn append(
  key_to_append: NodeKey,
  appended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  expect Some(anchor_node_input) = list.head(common.node_in_outputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  let anchor_node_raw_key_and_link = get_raw_key_and_link(anchor_node_input)
  expect Some(appended_node_output) =
    list.at(common.node_outputs, appended_node_output_index)
  expect Some(anchor_node_output) =
    list.at(common.node_outputs, anchor_node_output_index)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum
  let appended_node_output_raw_key_and_link =
    get_raw_key_and_link(appended_node_output)

  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_append,
      data: parsed_anchor_node_input_datum.data,
    }
  and {
    key_added(key_to_append, common.node_mint),
    list.length(common.node_in_outputs) == 1,
    is_last_node(get_link(anchor_node_raw_key_and_link)),
    list.length(common.node_outputs) == 2,
    is_member(
      key_to_append,
      get_key(appended_node_output_raw_key_and_link),
      appended_node_output.address,
      anchor_node_input.address,
    ),
    is_last_node(get_link(appended_node_output_raw_key_and_link)),
    get_link(appended_node_output_raw_key_and_link) == key_to_append,
    anchor_node_input.address == anchor_node_output.address,
    anchor_node_input.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
    appended_node_output.address == anchor_node_output.address,
    list.length(flatten(appended_node_output.value)) == 2,
  }
}

pub fn remove(
  key_to_remove: NodeKey,
  removed_node_input_index: Int,
  anchor_node_input_index: Int,
  common: Common,
) {
  expect Some(removed_node_input) =
    list.at(common.node_in_outputs, removed_node_input_index)
  expect Some(anchor_node_input) =
    list.at(common.node_in_outputs, anchor_node_input_index)
  let removed_node_raw_key_and_link = get_raw_key_and_link(removed_node_input)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum
  expect Some(anchor_node_output) = list.head(common.node_outputs)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum

  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: get_link(removed_node_raw_key_and_link),
      data: parsed_anchor_node_input_datum.data,
    }
  and {
    key_removed(key_to_remove, common.node_mint),
    list.length(common.node_in_outputs) == 2,
    is_member(
      key_to_remove,
      get_key(removed_node_raw_key_and_link),
      removed_node_input.address,
      anchor_node_input.address,
    ),
    parsed_anchor_node_input_datum.link == key_to_remove,
    list.length(common.node_outputs) == 1,
    anchor_node_input.address == anchor_node_output.address,
    anchor_node_input.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
  }
}
