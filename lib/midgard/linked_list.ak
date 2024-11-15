use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/interval.{Interval}
use aiken/primitive/bytearray
use cardano/assets.{
  PolicyId, Value, from_asset, quantity_of, tokens, without_lovelace,
}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/constants
use midgard/types/general.{POSIXTime, PubKeyHash}
use midgard/utils.{
  get_outputs, has_data_cs, parse_node_key, quantity_of_policy_id,
}

pub type NodeKey {
  Key { key: ByteArray }
  Empty
}

pub type NodeDatum<app_data> {
  key: NodeKey,
  link: NodeKey,
  data: app_data,
}

pub type Common<datum_type> {
  own_cs: PolicyId,
  mint: Value,
  node_in_outputs: List<Output>,
  node_outputs: List<Output>,
}

pub fn make_common(
  transaction: Transaction,
  own_cs: PolicyId,
  output_data_coercer: fn(Data) -> NodeDatum<app_data>,
) -> (
  Common<app_data>,
  List<Input>,
  List<Output>,
  List<PubKeyHash>,
  Interval<POSIXTime>,
) {
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
  let node_inputs =
    list.map(
      from_node_val,
      fn(node) { node_input_utxo_datum_unsafe(node, output_data_coercer) },
    )
  let node_outputs =
    list.map(
      to_node_val,
      fn(node) { parse_node_output_utxo(own_cs, node, output_data_coercer) },
    )
  //TODO Do we need the list of minted tokens with the list policy instead of the whole mint object?
  let common =
    Common {
      own_cs,
      mint,
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
          let Output { address, value, .. } = output
          has_data_cs(cs, value)
        },
      )
}

pub fn node_input_utxo_datum_unsafe(
  output: Output,
  output_data_coercer: fn(Data) -> NodeDatum<app_data>,
) -> NodeDatum<app_data> {
  expect Output { address, value, datum: InlineDatum(raw_node), .. } = output
  output_data_coercer(raw_node)
}

pub fn parse_node_output_utxo(
  cs: PolicyId,
  output: Output,
  output_data_coercer: fn(Data) -> NodeDatum<app_data>,
) -> NodeDatum<app_data> {
  expect Output { address, value, datum: InlineDatum(raw_node), .. } = output
  let node_datum = output_data_coercer(raw_node)
  expect [Pair(tn, amount)] =
    value
      |> tokens(cs)
      |> dict.to_pairs()
  expect amount == 1
  let node_key = parse_node_key(tn)
  let datum_key =
    when node_datum.key is {
      Empty -> None
      Key(key) -> Some(key)
    }
  expect node_key == datum_key
  expect without_lovelace(value) == from_asset(cs, tn, 1)
  expect valid_node(node_datum)
  node_datum
}

pub fn list_state_transition(mint_value: Value, node_nft_policy_id: ByteArray) {
  quantity_of_policy_id(mint_value, node_nft_policy_id) != 0
}

pub fn modify_data(
  mint_value: Value,
  own_input: Input,
  own_output: Output,
  node_nft_policy_id: ByteArray,
  node_nft_asset_name: ByteArray,
) -> Bool {
  expect InlineDatum(input_datum) = own_input.output.datum
  let input_constr = builtin.un_constr_data(input_datum).2nd
  let input_key = builtin.head_list(input_constr)
  let input_link = builtin.head_list(builtin.tail_list(input_constr))

  expect InlineDatum(output_datum) = own_output.datum
  let output_constr = builtin.un_constr_data(output_datum).2nd
  let output_key = builtin.head_list(output_constr)
  let output_link = builtin.head_list(builtin.tail_list(output_constr))

  and {
    input_key == output_key,
    input_link == output_link,
    quantity_of_policy_id(mint_value, node_nft_policy_id) == 0,
    quantity_of(own_input.output.value, node_nft_policy_id, node_nft_asset_name) == 1,
    quantity_of(own_output.value, node_nft_policy_id, node_nft_asset_name) == 1,
  }
}

pub fn is_root_node(node: NodeDatum<datum_type>) {
  node.key == Empty
}

pub fn is_last_node(node: NodeDatum<datum_type>) {
  node.link == Empty
}

pub fn is_empty_list(root_node: NodeDatum<datum_type>) {
  is_last_node(root_node)
}

pub fn is_member(k: NodeKey, node: NodeDatum<datum_type>) {
  k == node.key
}

//Only for ordered-list!
pub fn is_not_member(k: NodeKey, node: NodeDatum<datum_type>) {
  or {
    k == Empty,
    {
      expect Key(actual_key) = k
      expect Key(actual_node_key) = node.key
      expect Key(actual_node_link) = node.link
      and {
        or {
          is_root_node(node),
          bytearray.compare(actual_node_key, actual_key) == Less,
        },
        or {
          is_last_node(node),
          bytearray.compare(actual_key, actual_node_link) == Less,
        },
      }
    },
  }
}

//init_utxo must be constant
pub fn init(
  init_utxo: OutputReference,
  inputs: List<Input>,
  common: Common<app_data>,
) {
  and {
    //TODO Check that output to list contains root nft
    list.any(inputs, fn(input) { init_utxo == input.output_reference }),
    list.length(common.node_inputs) == 0,
    list.length(common.node_outputs) == 1,
    quantity_of(common.mint, common.own_cs, constants.root_node_token_name) == 1,
  }
}

pub fn deinit(common: Common<app_data>) {
  and {
    list.length(common.node_inputs) == 1,
    list.length(common.node_outputs) == 0,
    quantity_of(common.mint, common.own_cs, constants.root_node_token_name) == -1,
    quantity_of_policy_id(common.mint, common.own_cs) == 1,
  }
}

pub fn prepend() {
  todo
}

pub fn append() {
  todo
}

pub fn remove() {
  todo
}

//TODO CHECK FROM HERE

pub fn cover_key(data: NodeDatum<datum_type>, insert_key: PubKeyHash) -> Bool {
  let less_than_key =
    when data.key is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Less
    }
  let more_than_key =
    when data.link is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Greater
    }
  less_than_key? && more_than_key?
}

pub fn valid_node(node: NodeDatum<datum_type>) -> Bool {
  when node.key is {
    Empty -> True
    Key(node_key) ->
      when node.link is {
        Empty -> True
        Key(next_key) -> bytearray.compare(node_key, next_key) == Less
      }
  }
}

pub fn safe_get_key(k: NodeKey) {
  expect Key(the_key) = k
  the_key
}
