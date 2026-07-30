use aiken/builtin
use aiken/fuzz
use cardano/assets
use midgard/state_queue.{confirmed_state_asset_name}

/// A trivial Sampler that always produces Void, regardless of size.
fn void_sampler(_: Int) -> Fuzzer<Void> {
  fuzz.constant(Void)
}

// 28-byte synthetic policy ID (matches the real state-queue script hash shape).
const bench_policy_id: ByteArray =
  #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

// ============================================================
// Optimization A — confirmed_state_asset_name length
// The current name "MIDGARD_CONFIRMED_STATE" (21 bytes) is embedded in every
// state-queue UTxO value.  Shortening it (e.g. to "") reduces:
//   • the CBOR-encoded value size stored on-chain (lower min-ADA)
//   • the cost of builtin.serialise_data on any Value containing this token
//   • the cost of asset-name equality checks inside linked-list verification
// The gap between *_current_name and *_empty_name shows the per-operation
// saving once A is applied.
// ============================================================

/// Baseline: cost of serialising a Value that contains the current 21-byte
/// confirmed_state_asset_name.  This reflects what every CommitBlockHeader,
/// MergeToConfirmedState, Init, and Deinit redeemer pays today.
bench serialise_value_with_current_asset_name(_: Void via void_sampler) {
  let value =
    assets.from_lovelace(2_000_000)
      |> assets.add(bench_policy_id, confirmed_state_asset_name, 1)
  builtin.serialise_data(value)
}

/// Expected: same serialisation after A — asset name shortened to "".
/// Running this alongside the baseline makes the saving immediately visible.
bench serialise_value_with_empty_asset_name(_: Void via void_sampler) {
  let value =
    assets.from_lovelace(2_000_000)
      |> assets.add(bench_policy_id, "", 1)
  builtin.serialise_data(value)
}

/// Baseline: cost of an assets.quantity_of lookup with the current long name.
/// linked_list.run_element_with performs this check on every state-queue UTxO.
bench quantity_of_with_current_asset_name(_: Void via void_sampler) {
  let value =
    assets.from_lovelace(2_000_000)
      |> assets.add(bench_policy_id, confirmed_state_asset_name, 1)
  assets.quantity_of(value, bench_policy_id, confirmed_state_asset_name)
}

/// Expected: same lookup after A with the empty name.
bench quantity_of_with_empty_asset_name(_: Void via void_sampler) {
  let value =
    assets.from_lovelace(2_000_000)
      |> assets.add(bench_policy_id, "", 1)
  assets.quantity_of(value, bench_policy_id, "")
}
