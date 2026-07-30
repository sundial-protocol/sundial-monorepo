use aiken/fuzz
use cardano/address.{Address, VerificationKey}
use cardano/transaction.{NoDatum, OutputReference}
use midgard/ledger_state
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

// ============================================================
// Optimization: use already-typed event datum fields instead of re-parsing.
//
// The user-event spend validators receive their datum as a fully-decoded
// `Option<Datum>`, so the nested event record is already typed on entry. The
// old code upcast the event back to `Data`, split it with
// `unsafe_event_to_key_value_pair`, and then re-ran a structural `expect` to
// recover the typed `info`/`tx` field --- validating the same bytes twice.
//
// Each pair of benchmarks below builds the identical sample event and produces
// the identical overridden `Data`; the only difference is whether the middle
// step re-parses from `Data` (old) or reads the typed field directly (new).
// The delta isolates the redundant split + re-validation that the fix removes.
// ============================================================

const sample_out_ref: OutputReference =
  OutputReference {
    transaction_id: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
    output_index: 0,
  }

fn sample_withdrawal_event() -> ledger_state.WithdrawalEvent {
  let l1_address =
    Address {
      payment_credential: VerificationKey(
        #"aabbccddeeff00112233445566778899aabbccddeeff001122334455",
      ),
      stake_credential: None,
    }
  let l2_value =
    [
      Pair(
        #"aabbccddeeff00112233445566778899aabbccddeeff001122334455",
        [Pair(#"4d494447", 1_000_000), Pair(#"544f4b32", 5)],
      ),
    ]
  let body =
    ledger_state.WithdrawalBody {
      l2_outref: sample_out_ref,
      l2_owner: #"aabbccddeeff00112233445566778899aabbccddeeff001122334455",
      l2_value,
      l1_address,
      l1_datum: NoDatum,
    }
  let info =
    ledger_state.WithdrawalInfo {
      body,
      signature: Pair(
        #"1111111111111111111111111111111111111111111111111111111111111111",
        #"22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
      ),
      validity: ledger_state.WithdrawalIsValid,
    }
  ledger_state.WithdrawalEvent { id: sample_out_ref, info }
}

/// Old path: upcast the typed event, split it, then re-parse `WithdrawalInfo`
/// from `Data` before applying the validity override.
bench withdrawal_info_reparse(_: Void via void_sampler) {
  let event_data: Data = sample_withdrawal_event()
  let Pair(_id, info_data) =
    ledger_state.unsafe_event_to_key_value_pair(event_data)
  expect coerced: ledger_state.WithdrawalInfo = info_data
  let overridden: Data =
    ledger_state.WithdrawalInfo {
      ..coerced,
      validity: ledger_state.IncorrectWithdrawalValue,
    }
  overridden
}

/// New path: read the already-typed `info` field directly, then apply the
/// override. No `Data` split and no re-validation.
bench withdrawal_info_typed(_: Void via void_sampler) {
  let ledger_state.WithdrawalEvent { info: coerced, .. } =
    sample_withdrawal_event()
  let overridden: Data =
    ledger_state.WithdrawalInfo {
      ..coerced,
      validity: ledger_state.IncorrectWithdrawalValue,
    }
  overridden
}

fn sample_tx_order_event() -> ledger_state.TxOrderEvent {
  let tx =
    ledger_state.MidgardTxCompact {
      body: #"3333333333333333333333333333333333333333333333333333333333333333",
      wits: #"4444444444444444444444444444444444444444444444444444444444444444",
      validity: ledger_state.TxIsValid,
    }
  ledger_state.TxOrderEvent { id: sample_out_ref, tx }
}

/// Old path: upcast the typed event, split it, then re-parse
/// `MidgardTxCompact` from `Data` before applying the validity override.
bench tx_order_reparse(_: Void via void_sampler) {
  let event_data: Data = sample_tx_order_event()
  let Pair(_id, tx_data) =
    ledger_state.unsafe_event_to_key_value_pair(event_data)
  expect tx: ledger_state.MidgardTxCompact = tx_data
  let overridden: Data =
    ledger_state.MidgardTxCompact { ..tx, validity: ledger_state.FeeTooLow }
  overridden
}

/// New path: read the already-typed `tx` field directly, then apply the
/// override. No `Data` split and no re-validation.
bench tx_order_typed(_: Void via void_sampler) {
  let ledger_state.TxOrderEvent { tx, .. } = sample_tx_order_event()
  let overridden: Data =
    ledger_state.MidgardTxCompact { ..tx, validity: ledger_state.FeeTooLow }
  overridden
}
