//// A partial clone from `aiken-design-patterns` for a custom wrapper and
//// parameter application, for cases where the first parameter is a hash with
//// 28 bytes, while the second parameter has 32 bytes.

use aiken/builtin
use aiken/crypto.{Blake2b_224, Blake2b_256, Hash, ScriptHash, blake2b_224}
use aiken/primitive/bytearray
use cardano/transaction.{Transaction}

const param_header_32_bytes = #"004c01225820"

const postfix: ByteArray = #"0001"

/// Similar to [`apply_prehashed_param`](#apply_prehashed_param), but for
/// scripts with 2 parameters.
pub fn apply_prehashed_param_2(
  version: Int,
  prefix: ByteArray,
  param_0: ByteArray,
  param_1: ByteArray,
) -> ScriptHash {
  builtin.integer_to_bytearray(True, 1, version)
    |> bytearray.concat(prefix)
    |> bytearray.concat(param_0)
    |> bytearray.concat(param_header_32_bytes)
    |> bytearray.concat(param_1)
    |> bytearray.concat(postfix)
    |> blake2b_224
}

pub fn prehashed_param_no_datum_wrapper_2(
  validator_function: fn(
    Hash<Blake2b_224, a>,
    Hash<Blake2b_256, b>,
    redeemer,
    endpoint_specific,
    Transaction,
  ) ->
    Bool,
  parameter_0: Hash<Blake2b_224, a>,
  parameter_1: Hash<Blake2b_256, b>,
  redeemer: redeemer,
  variable_arg: endpoint_specific,
  tx: Transaction,
) -> Bool {
  validator_function(parameter_0, parameter_1, redeemer, variable_arg, tx)
}
