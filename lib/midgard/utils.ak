use aiken/builtin
use aiken/collection/dict.{has_key}
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Address, Script, Credential}
use cardano/assets.{AssetName, PolicyId, Value, flatten, to_dict, tokens}
use cardano/transaction.{Input, Output, OutputReference, Redeemer, Transaction, Withdraw}
use midgard/constants.{set_node_prefix}
use midgard/types/general.{
  Empty, Key, LinkedListDatum, NodeKey, POSIXTime, PubKeyHash,
}

pub fn validate_mint(
  mints: Value,
  expected_minting_policy: ByteArray,
  expected_minting_name: ByteArray,
  expected_minting_amt: Int,
) -> Bool {
  let mints_policy = dict.to_pairs(tokens(mints, expected_minting_policy))
  mints_policy == [Pair(expected_minting_name, expected_minting_amt)]
}

pub fn cover_key(
  data: LinkedListDatum<datum_type>,
  insert_key: PubKeyHash,
) -> Bool {
  let less_than_key =
    when data.key is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Less
    }
  let more_than_key =
    when data.next is {
      Empty -> True
      Key(key) -> bytearray.compare(key, insert_key) == Greater
    }
  less_than_key? && more_than_key?
}

pub fn div_ceil(a, b: Int) -> Int {
  let div = a / b
  let rem = a % b
  when rem is {
    0 -> div
    _ -> div + 1
  }
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

pub fn parse_node_key(tn: AssetName) -> Option<ByteArray> {
  let prefix_length = bytearray.length(set_node_prefix)
  let tn_length = bytearray.length(tn)
  let key = bytearray.drop(tn, prefix_length)
  expect set_node_prefix == bytearray.take(tn, prefix_length)
  when prefix_length < tn_length is {
    True -> Some(key)
    False -> None
  }
}

pub fn valid_node(node: LinkedListDatum<datum_type>) -> Bool {
  when node.key is {
    Empty -> True
    Key(node_key) ->
      when node.next is {
        Empty -> True
        Key(next_key) -> bytearray.compare(node_key, next_key) == Less
      }
  }
}

pub fn find_cs_by_token_prefix(
  value: Value,
  prefix: ByteArray,
) -> List<PolicyId> {
  value
    |> flatten
    |> list.filter_map(
        fn(input: (PolicyId, ByteArray, Int)) -> Option<PolicyId> {
          let (cs, tn, _amt) = input
          if is_prefix_of(prefix, tn) {
            Some(cs)
          } else {
            None
          }
        },
      )
}

pub fn is_prefix_of(prefix: ByteArray, src: ByteArray) -> Bool {
  let prefix_length = bytearray.length(prefix)
  let src_length = bytearray.length(src)
  when prefix_length <= src_length is {
    True -> bytearray.take(src, prefix_length) == prefix
    False -> False
  }
}

pub fn safe_list_last(elems: List<a>) -> a {
  expect Some(last_elem) = list.last(elems)
  last_elem
}

pub fn safe_list_head(elems: List<a>) -> a {
  expect Some(last_elem) = list.head(elems)
  last_elem
}

pub fn safe_get_key(k: NodeKey) {
  expect Key(the_key) = k
  the_key
}

pub fn safe_list_init(elems: List<a>) {
  expect Some(elems_except_last) = list.init(elems)
  elems_except_last
}

pub fn is_posix_time_none(time: POSIXTime) {
  time == constants.posix_time_none
}

/// Returns the quantity of all tokens with the policyId
pub fn quantity_of_policy_id(value: Value, policyId: PolicyId) {
  dict.foldl(tokens(value, policyId), 0, fn(_k, v, result) { v + result })
}

pub fn get_own_hash(output_reference: OutputReference, txInputs: List<Input>) {
  expect Some(input) =
    list.find(txInputs, fn(x) { x.output_reference == output_reference })

  expect Script(hash) = input.output.address.payment_credential
  hash
}

pub fn is_output_to_sc(output: Output, scHash: ByteArray) -> Bool {
  when output.address.payment_credential is {
    Script(hash) -> hash == scHash
    _ -> False
  }
}

/// Originated from [Aiken Design Patterns - Stake Validator](https://github.com/Anastasia-Labs/aiken-design-patterns)
//  As withdraw_amount as a parameter is irrelevant to Midgard, customize the function to avoid further costs on `Transaction` for `withdrawals`

pub fn spend_custom(
  withdraw_script_hash: ScriptHash,
  withdraw_redeemer_validator: fn(Redeemer) -> Bool,
  tx: Transaction,
) -> Bool {
  let Transaction { redeemers, .. } = tx

  let withdraw_script = Script(withdraw_script_hash)

  let withdraw_purpose = Withdraw(withdraw_script)

  let withdraw_redeemer: Redeemer =
    redeemers |> pairs.get_first(withdraw_purpose)

  withdraw_redeemer_validator(withdraw_redeemer)
}
