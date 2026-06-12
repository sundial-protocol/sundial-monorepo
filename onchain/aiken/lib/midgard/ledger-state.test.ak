use aiken/builtin
use aiken/crypto
use aiken/fuzz
use midgard/ledger_state.{
  ConfirmedState, Header, genesis_header_hash, genesis_utxo_root,
}

/// A trivial Sampler that always produces Void, regardless of size.
/// Used to benchmark fixed-input functions where no fuzzed data is needed.
fn void_sampler(_: Int) -> Fuzzer<Void> {
  fuzz.constant(Void)
}

/// Benchmark: cost of serialising a full block Header and hashing it with
/// Blake2b-224.  This is the core on-chain operation performed by the
/// CommitBlockHeader redeemer of the state-queue validator: the new block's
/// linked-list key is derived as blake2b_224(serialise_data(header)).
bench header_hash_computation(_: Void via void_sampler) {
  let header =
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
  crypto.blake2b_224(builtin.serialise_data(header))
}

/// Benchmark: cost of serialising a ConfirmedState record and hashing it.
/// ConfirmedState is the root element of the state-queue linked list; it is
/// read or reproduced in the Init, Deinit, and MergeToConfirmedState
/// redeemers.
bench confirmed_state_hash_computation(_: Void via void_sampler) {
  let confirmed_state =
    ConfirmedState {
      header_hash: genesis_header_hash,
      prev_header_hash: genesis_header_hash,
      utxo_root: genesis_utxo_root,
      start_time: 1_700_000_000_000,
      end_time: 1_700_000_060_000,
      protocol_version: 0,
    }
  crypto.blake2b_224(builtin.serialise_data(confirmed_state))
}
