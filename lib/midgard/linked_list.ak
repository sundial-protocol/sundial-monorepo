use aiken/builtin
use aiken/collection/dict.{Dict, has_key}
use aiken/collection/list
use aiken/interval.{Interval}
use aiken/primitive/bytearray
use cardano/assets.{
  AssetName, PolicyId, Value, flatten, quantity_of, to_dict, tokens,
}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/constants
use midgard/types/general.{POSIXTime, PubKeyHash}

pub type NodeKey {
  Key { key: ByteArray }
  Empty
}

pub type NodeDatum {
  key: NodeKey,
  link: NodeKey,
  data: Data,
}

pub type Common {
  node_cs: PolicyId,
  node_mint: Dict<AssetName, Int>,
  node_in_outputs: List<Output>,
  node_outputs: List<Output>,
}

// Checks if a Currency Symbol is held within a Value
pub fn has_data_cs(cs: PolicyId, value: Value) -> Bool {
  value
    |> to_dict()
    |> has_key(cs)
}

pub fn get_outputs(inputs: List<Input>) -> List<Output> {
  list.map(
    inputs,
    fn(input) {
      let Input { output, .. } = input
      output
    },
  )
}

pub fn get_raw_key_and_link(node_output: Output) -> (Data, Data) {
  expect InlineDatum(node_output_datum) = node_output.datum
  let constr = builtin.un_constr_data(node_output_datum).2nd
  let key = builtin.head_list(constr)
  let link = builtin.head_list(builtin.tail_list(constr))
  (key, link)
}

pub fn get_key(raw_key_and_link: (Data, Data)) -> NodeKey {
  expect key: NodeKey = raw_key_and_link.1st
  key
}

pub fn get_link(raw_key_and_link: (Data, Data)) -> NodeKey {
  expect link: NodeKey = raw_key_and_link.2nd
  link
}

//TODO Is this the correct serialization of the key?
pub fn serialize_key(key: NodeKey) -> ByteArray {
  when key is {
    Empty -> constants.root_node_token_name
    Key(actual_key) ->
      bytearray.concat(constants.root_node_token_name, actual_key)
  }
}

pub fn make_common(
  transaction: Transaction,
  own_cs: PolicyId,
) -> (Common, List<Input>, List<Output>, List<PubKeyHash>, Interval<POSIXTime>) {
  let Transaction {
    inputs,
    outputs,
    mint,
    validity_range,
    extra_signatories,
    ..
  } = transaction
  let in_outputs = get_outputs(inputs)
  let from_node_val = only_at_node_val(in_outputs, own_cs)
  let to_node_val = only_at_node_val(outputs, own_cs)
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
  let common =
    Common {
      node_cs: own_cs,
      node_mint: tokens(mint, own_cs),
      node_in_outputs: from_node_val,
      node_outputs: to_node_val,
    }
  //TODO Do we need inputs and outputs here?
  (common, inputs, outputs, extra_signatories, validity_range)
}

pub fn only_at_node_val(outputs: List<Output>, cs: PolicyId) -> List<Output> {
  outputs
    |> list.filter(
        fn(output) {
          let Output { value, .. } = output
          has_data_cs(cs, value)
        },
      )
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

pub fn is_member(k: NodeKey, node_key: NodeKey) {
  k == node_key
}

pub fn ordered_is_not_member(k: NodeKey, node_key: NodeKey, node_link: NodeKey) {
  or {
    k == Empty,
    {
      expect Key(actual_key) = k
      and {
        or {
          is_root_node(node_key),
          {
            expect Key(actual_node_key) = node_key
            bytearray.compare(actual_node_key, actual_key) == Less
          },
        },
        or {
          is_last_node(node_link),
          {
            expect Key(actual_node_link) = node_link
            bytearray.compare(actual_key, actual_node_link) == Less
          },
        },
      }
    },
  }
}

//Application code must ensure that this action can happen only once
pub fn init(common: Common) {
  expect Some(output_root_node) = list.head(common.node_outputs)
  let raw_key_and_link = get_raw_key_and_link(output_root_node)
  and {
    key_added(Empty, common.node_mint),
    is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link)),
    quantity_of(
      output_root_node.value,
      common.node_cs,
      constants.root_node_token_name,
    ) == 1,
    list.length(flatten(output_root_node.value)) == 2,
  }
}

pub fn deinit(common: Common) {
  expect Some(input_root_node) = list.head(common.node_in_outputs)
  let raw_key_and_link = get_raw_key_and_link(input_root_node)
  and {
    key_removed(Empty, common.node_mint),
    is_empty_list(get_key(raw_key_and_link), get_link(raw_key_and_link)),
    quantity_of(
      input_root_node.value,
      common.node_cs,
      constants.root_node_token_name,
    ) == 1,
  }
}

//The indexes are relative to utxos of this linkedlist, they are NOT absolute
pub fn unordered_prepend(
  key_to_prepend: NodeKey,
  prepended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  expect Some(anchor_node_in_output) = list.head(common.node_in_outputs)
  expect InlineDatum(anchor_node_in_output_datum) = anchor_node_in_output.datum
  expect parsed_anchor_node_in_output_datum: NodeDatum =
    anchor_node_in_output_datum
  let anchor_node_raw_key_and_link = get_raw_key_and_link(anchor_node_in_output)
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
      key: parsed_anchor_node_in_output_datum.key,
      link: key_to_prepend,
      data: parsed_anchor_node_in_output_datum.data,
    }
  and {
    key_added(key_to_prepend, common.node_mint),
    list.length(common.node_in_outputs) == 1,
    is_root_node(get_key(anchor_node_raw_key_and_link)),
    list.length(common.node_outputs) == 2,
    is_member(key_to_prepend, get_key(prepended_node_output_raw_key_and_link)),
    get_link(anchor_node_raw_key_and_link) == get_link(
      prepended_node_output_raw_key_and_link,
    ),
    parsed_anchor_node_output_datum.link == key_to_prepend,
    anchor_node_in_output.address == anchor_node_output.address,
    anchor_node_in_output.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
    prepended_node_output.address == anchor_node_output.address,
  }
}

//The indexes are relative to utxos of this linkedlist, they are NOT absolute
pub fn ordered_prepend(
  key_to_prepend: NodeKey,
  prepended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  //We know it's only 1 from unordered_prepend()
  expect Some(anchor_node_in_output) = list.head(common.node_in_outputs)
  let anchor_node_in_output_raw_key_and_link =
    get_raw_key_and_link(anchor_node_in_output)
  and {
    unordered_prepend(
      key_to_prepend,
      prepended_node_output_index,
      anchor_node_output_index,
      common,
    ),
    ordered_is_not_member(
      key_to_prepend,
      node_key: get_key(anchor_node_in_output_raw_key_and_link),
      node_link: get_link(anchor_node_in_output_raw_key_and_link),
    ),
  }
}

pub fn unordered_append(
  key_to_append: NodeKey,
  appended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  expect Some(anchor_node_in_output) = list.head(common.node_in_outputs)
  expect InlineDatum(anchor_node_in_output_datum) = anchor_node_in_output.datum
  expect parsed_anchor_node_in_output_datum: NodeDatum =
    anchor_node_in_output_datum
  let anchor_node_raw_key_and_link = get_raw_key_and_link(anchor_node_in_output)
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
      key: parsed_anchor_node_in_output_datum.key,
      link: key_to_append,
      data: parsed_anchor_node_in_output_datum.data,
    }
  and {
    key_added(key_to_append, common.node_mint),
    list.length(common.node_in_outputs) == 1,
    is_last_node(get_link(anchor_node_raw_key_and_link)),
    list.length(common.node_outputs) == 2,
    is_member(key_to_append, get_key(appended_node_output_raw_key_and_link)),
    is_last_node(get_link(appended_node_output_raw_key_and_link)),
    get_link(appended_node_output_raw_key_and_link) == key_to_append,
    anchor_node_in_output.address == anchor_node_output.address,
    anchor_node_in_output.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
    appended_node_output.address == anchor_node_output.address,
  }
}

pub fn ordered_append(
  key_to_append: NodeKey,
  appended_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  //We know it's only 1 from unordered_prepend()
  expect Some(anchor_node_in_output) = list.head(common.node_in_outputs)
  let anchor_node_in_output_raw_key_and_link =
    get_raw_key_and_link(anchor_node_in_output)
  and {
    unordered_prepend(
      key_to_append,
      appended_node_output_index,
      anchor_node_output_index,
      common,
    ),
    ordered_is_not_member(
      key_to_append,
      node_key: get_key(anchor_node_in_output_raw_key_and_link),
      node_link: get_link(anchor_node_in_output_raw_key_and_link),
    ),
  }
}

pub fn ordered_insert(
  key_to_insert: NodeKey,
  inserted_node_output_index: Int,
  anchor_node_output_index: Int,
  common: Common,
) {
  expect Some(anchor_node_input) = list.head(common.node_in_outputs)
  expect InlineDatum(anchor_node_input_datum) = anchor_node_input.datum
  expect parsed_anchor_node_input_datum: NodeDatum = anchor_node_input_datum

  expect Some(inserted_node_output) =
    list.at(common.node_outputs, inserted_node_output_index)
  expect Some(anchor_node_output) =
    list.at(common.node_outputs, anchor_node_output_index)
  let inserted_node_output_raw_key_and_link =
    get_raw_key_and_link(inserted_node_output)
  expect InlineDatum(anchor_node_output_datum) = anchor_node_output.datum
  expect parsed_anchor_node_output_datum: NodeDatum = anchor_node_output_datum

  let anchor_node_new_datum =
    NodeDatum {
      key: parsed_anchor_node_input_datum.key,
      link: key_to_insert,
      data: parsed_anchor_node_input_datum.data,
    }
  and {
    key_added(key_to_insert, common.node_mint),
    list.length(common.node_in_outputs) == 1,
    ordered_is_not_member(
      key_to_insert,
      parsed_anchor_node_input_datum.key,
      parsed_anchor_node_input_datum.link,
    ),
    list.length(common.node_outputs) == 2,
    is_member(key_to_insert, get_key(inserted_node_output_raw_key_and_link)),
    parsed_anchor_node_output_datum.link == key_to_insert,
    parsed_anchor_node_input_datum.link == get_link(
      inserted_node_output_raw_key_and_link,
    ),
    anchor_node_input.address == anchor_node_output.address,
    anchor_node_input.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
    inserted_node_output.address == anchor_node_output.address,
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
    is_member(key_to_remove, get_key(removed_node_raw_key_and_link)),
    parsed_anchor_node_input_datum.link == key_to_remove,
    list.length(common.node_outputs) == 1,
    anchor_node_input.address == anchor_node_output.address,
    anchor_node_input.value == anchor_node_output.value,
    anchor_node_new_datum == parsed_anchor_node_output_datum,
  }
}
