use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value, flatten, tokens}
use cardano/transaction.{Input, Output, OutputReference}
use midgard/common/constants.{posix_time_none}
use midgard/common/types.{POSIXTime}

pub fn validate_mint(
  mints: Value,
  expected_minting_policy: ByteArray,
  expected_minting_name: ByteArray,
  expected_minting_amt: Int,
) -> Bool {
  let mints_policy = dict.to_pairs(tokens(mints, expected_minting_policy))
  mints_policy == [Pair(expected_minting_name, expected_minting_amt)]
}

pub fn div_ceil(a, b: Int) -> Int {
  let div = a / b
  let rem = a % b
  when rem is {
    0 -> div
    _ -> div + 1
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

pub fn safe_list_init(elems: List<a>) {
  expect Some(elems_except_last) = list.init(elems)
  elems_except_last
}

pub fn is_posix_time_none(time: POSIXTime) {
  time == posix_time_none
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

pub fn get_own_address(output_reference: OutputReference, txInputs: List<Input>) {
  expect Some(input) =
    list.find(txInputs, fn(x) { x.output_reference == output_reference })

  input.output.address
}

pub fn is_output_to_sc(output: Output, scHash: ByteArray) -> Bool {
  when output.address.payment_credential is {
    Script(hash) -> hash == scHash
    _ -> False
  }
}
