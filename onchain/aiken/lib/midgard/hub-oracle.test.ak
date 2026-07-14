use aiken/builtin
use aiken/fuzz
use cardano/address.{Address, Script}
use midgard/hub_oracle as hub

/// A trivial Sampler that always produces Void, regardless of size.
/// Used to benchmark fixed-input functions where no fuzzed data is needed.
fn void_sampler(_: Int) -> Fuzzer<Void> {
  fuzz.constant(Void)
}

const sample_policy: ByteArray =
  #"aabbccddeeff00112233445566778899aabbccddeeff001122334455"

fn sample_address(tag: ByteArray) -> Address {
  Address { payment_credential: Script(tag), stake_credential: None }
}

/// A representative, fully-populated hub oracle datum. All 26 fields are set:
/// 12 policy IDs, 14 addresses (each a nested credential record), and the
/// reserve observer script hash.
fn sample_hub_datum() -> hub.Datum {
  let addr = sample_address(sample_policy)
  hub.Datum {
    registered_operators: sample_policy,
    active_operators: sample_policy,
    retired_operators: sample_policy,
    scheduler: sample_policy,
    state_queue: sample_policy,
    fraud_proof_catalogue: sample_policy,
    fraud_proof: sample_policy,
    deposit: sample_policy,
    withdrawal: sample_policy,
    tx_order: sample_policy,
    settlement: sample_policy,
    payout: sample_policy,
    registered_operators_addr: addr,
    active_operators_addr: addr,
    retired_operators_addr: addr,
    scheduler_addr: addr,
    state_queue_addr: addr,
    fraud_proof_catalogue_addr: addr,
    fraud_proof_addr: addr,
    deposit_addr: addr,
    withdrawal_addr: addr,
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: sample_policy,
  }
}

/// Benchmark: cost of the old read path --- a full structural `expect` of the
/// 26-field hub datum, of which only one policy field is actually consumed.
/// This forces recursive validation of all 12 policy IDs and all 14 nested
/// `Address` records.
bench hub_datum_full_parse(_: Void via void_sampler) {
  let hub_data: Data = sample_hub_datum()
  expect parsed: hub.Datum = hub_data
  parsed.state_queue
}

/// Benchmark: cost of the new read path for a single low-index field. Splits
/// the constructor once and validates only the one field consumed.
bench hub_datum_lean_state_queue(_: Void via void_sampler) {
  let hub_data: Data = sample_hub_datum()
  builtin.unconstr_fields(hub_data) |> hub.state_queue_of
}

/// Benchmark: worst-case lean read (a high-index address field). Even walking
/// to index 23 avoids validating the other 25 fields.
bench hub_datum_lean_reserve_addr(_: Void via void_sampler) {
  let hub_data: Data = sample_hub_datum()
  builtin.unconstr_fields(hub_data) |> hub.reserve_addr_of
}
