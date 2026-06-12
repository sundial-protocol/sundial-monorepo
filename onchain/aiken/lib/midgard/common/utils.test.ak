use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/fuzz
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/common/utils
use midgard/ledger_state.{Header, genesis_header_hash}

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

// ============================================================
// Benchmark helpers
// ============================================================

/// 28-byte script hash used as payment credential in synthetic inputs.
const bench_script_hash: ByteArray =
  #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

/// 28-byte policy ID for NFTs in authentic-input helper benchmarks (opt C).
const bench_nft_policy_id: ByteArray =
  #"cafecafecafecafecafecafecafecafecafecafecafecafecafecafe"

/// Minimal synthetic Input whose payment credential is Script(bench_script_hash).
fn make_bench_input(tx_id: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: 0,
    },
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

/// Minimal synthetic Input carrying exactly one NFT with bench_nft_policy_id.
/// Used by the authentic-input helper benchmarks (opt C).
fn make_bench_authentic_input(tx_id: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: 0,
    },
    output: Output {
      address: Address {
        payment_credential: Script(bench_nft_policy_id),
        stake_credential: None,
      },
      value: assets.from_lovelace(2_000_000)
        |> assets.add(bench_nft_policy_id, "", 1),
      datum: InlineDatum(builtin.i_data(0)),
      reference_script: None,
    },
  }
}

/// Representative block Header used in hash-verification benchmarks.
fn bench_header() -> Header {
  Header {
    prev_utxos_root: #"0000000000000000000000000000000000000000000000000000000000000000",
    utxos_root: #"1111111111111111111111111111111111111111111111111111111111111111",
    transactions_root: #"2222222222222222222222222222222222222222222222222222222222222222",
    deposits_root: #"3333333333333333333333333333333333333333333333333333333333333333",
    withdrawals_root: #"4444444444444444444444444444444444444444444444444444444444444444",
    start_time: 1_700_000_000_000,
    end_time: 1_700_000_060_000,
    prev_header_hash: genesis_header_hash,
    operator_vkey: #"aabbccddeeff00112233445566778899aabbccddeeff001122334455",
    protocol_version: 0,
  }
}

// ============================================================
// Optimization E — verify_hash_32 / verify_hash_28
// Current impl: blake2b_*(cbor.serialise(data))
// After E:      blake2b_*(builtin.serialise_data(data))  [cheaper builtin]
// The gap between *_current and *_expected benchmarks is the per-call saving.
// ============================================================

/// Baseline: current cost of verify_hash_32 (cbor.serialise path).
/// After optimization E this function will use builtin.serialise_data and its
/// numbers will drop to match verify_hash_32_expected.
bench verify_hash_32_current(_: Void via void_sampler) {
  let data: Data = bench_header()
  let hash = blake2b_256(cbor.serialise(data))
  utils.verify_hash_32(data, hash)
}

/// Expected: simulates verify_hash_32 after optimization E.
/// The delta between this and verify_hash_32_current is the saving from E.
bench verify_hash_32_expected(_: Void via void_sampler) {
  let data: Data = bench_header()
  let hash = blake2b_256(builtin.serialise_data(data))
  blake2b_256(builtin.serialise_data(data)) == hash
}

/// Baseline: current cost of verify_hash_28 (cbor.serialise path).
bench verify_hash_28_current(_: Void via void_sampler) {
  let data: Data = bench_header()
  let hash = blake2b_224(cbor.serialise(data))
  utils.verify_hash_28(data, hash)
}

/// Expected: simulates verify_hash_28 after optimization E.
bench verify_hash_28_expected(_: Void via void_sampler) {
  let data: Data = bench_header()
  let hash = blake2b_224(builtin.serialise_data(data))
  blake2b_224(builtin.serialise_data(data)) == hash
}

// ============================================================
// Optimization D — get_own_hash / get_own_address via list.find
// Current impl: list.find with OutputReference equality at every step.
// After D:      list.at with explicit index (integer counting only).
// The gap between *_back_of_10 and *_expected_back_of_10 is the saving.
// ============================================================

/// Baseline: get_own_hash when target is the FIRST of 10 inputs (best case).
/// list.find exits after one OutputReference equality check.
bench get_own_hash_front_of_10(_: Void via void_sampler) {
  let target_id =
    #"0000000000000000000000000000000000000000000000000000000000000000"
  let target = OutputReference { transaction_id: target_id, output_index: 0 }
  let inputs =
    [
      make_bench_input(target_id),
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
  utils.get_own_hash(target, inputs)
}

/// Baseline: get_own_hash when target is the LAST of 10 inputs (worst case).
/// list.find performs 10 full OutputReference equality comparisons.
/// After optimization D this cost will fall to match get_own_hash_expected_back_of_10.
bench get_own_hash_back_of_10(_: Void via void_sampler) {
  let target_id =
    #"9999999999999999999999999999999999999999999999999999999999999999"
  let target = OutputReference { transaction_id: target_id, output_index: 0 }
  let inputs =
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
      make_bench_input(target_id),
    ]
  utils.get_own_hash(target, inputs)
}

/// Expected: simulates the optimized get_own_hash using list.at by index.
/// Traverses 10 inputs by counter only — no OutputReference comparisons.
/// The delta between this and get_own_hash_back_of_10 is the saving from D.
bench get_own_hash_expected_back_of_10(_: Void via void_sampler) {
  let inputs =
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
  expect Some(found) = list.at(inputs, 9)
  expect Script(hash) = found.output.address.payment_credential
  hash
}

// ============================================================
// Optimization C — authentic-input helper consolidation
// These are regression guards: CPU/mem per call should not change after
// refactoring (the saving is script-size, not per-call budget).
// ============================================================

/// Regression guard: get_authentic_input_with_policy_at on a single-element list.
bench get_authentic_input_with_policy_at_1(_: Void via void_sampler) {
  let tx_id =
    #"abababababababababababababababababababababababababababababababababab"
  let inputs = [make_bench_authentic_input(tx_id)]
  utils.get_authentic_input_with_policy_at(inputs, bench_nft_policy_id, 0)
}

/// Regression guard: get_authentic_input_asset_name_with_policy_at on a single-element list.
bench get_authentic_input_asset_name_with_policy_at_1(_: Void via void_sampler) {
  let tx_id =
    #"abababababababababababababababababababababababababababababababababab"
  let inputs = [make_bench_authentic_input(tx_id)]
  utils.get_authentic_input_asset_name_with_policy_at(
    inputs,
    bench_nft_policy_id,
    0,
  )
}

/// Regression guard: get_authentic_input_datum_with_policy_at on a single-element list.
bench get_authentic_input_datum_with_policy_at_1(_: Void via void_sampler) {
  let tx_id =
    #"abababababababababababababababababababababababababababababababababab"
  let inputs = [make_bench_authentic_input(tx_id)]
  utils.get_authentic_input_datum_with_policy_at(inputs, bench_nft_policy_id, 0)
}
