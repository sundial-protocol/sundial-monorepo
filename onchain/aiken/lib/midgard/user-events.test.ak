use aiken/fuzz
use cardano/transaction.{OutputReference}
use midgard/user_events.{out_ref_to_nonce}

/// A trivial Sampler that always produces Void, regardless of size.
fn void_sampler(_: Int) -> Fuzzer<Void> {
  fuzz.constant(Void)
}

// ============================================================
// Optimization F — out_ref_to_nonce caching
// out_ref_to_nonce = cbor.serialise(out_ref) |> blake2b_256
// authenticate_new_event currently calls this once per invocation.
// This benchmark establishes the per-call cost so the saving is quantifiable
// if the function is ever called more than once in a branch and cached.
// ============================================================

/// Baseline: cost of one out_ref_to_nonce call (cbor.serialise + blake2b_256).
/// If authenticate_new_event ever needs the nonce a second time, caching it
/// saves this full amount per redundant call.
bench out_ref_to_nonce_cost(_: Void via void_sampler) {
  let out_ref =
    OutputReference {
      transaction_id: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
      output_index: 3,
    }
  out_ref_to_nonce(out_ref)
}

/// Two sequential calls to out_ref_to_nonce on the same OutputReference.
/// The difference between this and 2x out_ref_to_nonce_cost would be zero
/// (no UPLC-level CSE), which confirms the saving from binding the result once.
bench out_ref_to_nonce_called_twice(_: Void via void_sampler) {
  let out_ref =
    OutputReference {
      transaction_id: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
      output_index: 3,
    }
  let nonce1 = out_ref_to_nonce(out_ref)
  let nonce2 = out_ref_to_nonce(out_ref)
  nonce1 == nonce2
}
