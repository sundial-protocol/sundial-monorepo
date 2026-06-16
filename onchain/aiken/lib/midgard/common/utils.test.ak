use aiken/builtin
use aiken/fuzz
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/common/utils

/// A trivial Sampler that always produces Void, regardless of size.
/// Used to benchmark fixed-input functions where no fuzzed data is needed.
fn void_sampler(_: Int) -> Fuzzer<Void> {
  fuzz.constant(Void)
}

test zip_foldl() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list =
    utils.zip_foldl(
      list_a,
      list_b,
      [],
      fn(a, b, acc) {
        let new_list = [(a, b), ..acc]
        new_list
      },
    )
  final_list == [(3, 3), (2, 2), (1, 1)]
}

test zip_foldr() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list =
    utils.zip_foldr(
      list_a,
      list_b,
      [],
      fn(a, b, acc) {
        let new_list = [(a, b), ..acc]
        new_list
      },
    )
  final_list == [(1, 1), (2, 2), (3, 3)]
}

/// Benchmark: execution units for zip_foldl over 10-element lists.
bench zip_foldl_10(_: Void via void_sampler) {
  let list_a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let list_b = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  utils.zip_foldl(list_a, list_b, [], fn(a, b, acc) { [(a, b), ..acc] })
}

/// Benchmark: execution units for zip_foldr over 10-element lists.
bench zip_foldr_10(_: Void via void_sampler) {
  let list_a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let list_b = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  utils.zip_foldr(list_a, list_b, [], fn(a, b, acc) { [(a, b), ..acc] })
}

/// 28-byte script hash used as payment credential in synthetic inputs.
const bench_script_hash: ByteArray =
  #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

/// Minimal synthetic Input whose payment credential is Script(bench_script_hash).
fn make_bench_input(tx_id: ByteArray) -> Input {
  Input {
    output_reference: OutputReference { transaction_id: tx_id, output_index: 0 },
    output: Output {
      address: Address {
        payment_credential: Script(bench_script_hash),
        stake_credential: None,
      },
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.i_data(0)),
      reference_script: None,
    },
  }
}

fn bench_inputs_10() -> List<Input> {
  [
    make_bench_input(
      #"0000000000000000000000000000000000000000000000000000000000000000",
    ),
    make_bench_input(
      #"1111111111111111111111111111111111111111111111111111111111111111",
    ),
    make_bench_input(
      #"2222222222222222222222222222222222222222222222222222222222222222",
    ),
    make_bench_input(
      #"3333333333333333333333333333333333333333333333333333333333333333",
    ),
    make_bench_input(
      #"4444444444444444444444444444444444444444444444444444444444444444",
    ),
    make_bench_input(
      #"5555555555555555555555555555555555555555555555555555555555555555",
    ),
    make_bench_input(
      #"6666666666666666666666666666666666666666666666666666666666666666",
    ),
    make_bench_input(
      #"7777777777777777777777777777777777777777777777777777777777777777",
    ),
    make_bench_input(
      #"8888888888888888888888888888888888888888888888888888888888888888",
    ),
    make_bench_input(
      #"9999999999999999999999999999999999999999999999999999999999999999",
    ),
  ]
}

/// get_own_hash when target is the FIRST of 10 inputs (best case, index 0).
bench get_own_hash_front_of_10(_: Void via void_sampler) {
  utils.get_own_hash(0, bench_inputs_10())
}

/// get_own_hash when target is the LAST of 10 inputs (worst case, index 9).
bench get_own_hash_back_of_10(_: Void via void_sampler) {
  utils.get_own_hash(9, bench_inputs_10())
}
