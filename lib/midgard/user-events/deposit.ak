use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/certificate.{RegisterCredential, UnregisterCredential}
use cardano/transaction.{OutputReference, Publish, Redeemer, ScriptPurpose}
use midgard/common/parameter_validation
use midgard/common/types.{MerkleRoot, POSIXTime, Tokens}
use midgard/user_events

pub type Event =
  user_events.Event<Info>

pub type Info {
  l2_address: Address,
  l2_datum: Option<Data>,
  tokens: Tokens,
}

pub type Datum {
  event: Event,
  inclusion_time: POSIXTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: Option<Data>,
}

pub type SpendRedeemer {
  input_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  settlement_node_asset_name: AssetName,
  mint_redeemer_index: Int,
  merkle_proof: mpf.Proof,
  purpose: SpendPurpose,
}

pub type SpendPurpose {
  FundWithdrawal
  TransferToReserve
  RefundOfSkippedDeposit { insertion_root: MerkleRoot }
  RefundOfDepositInTimeGap { settlement_link_ref_input_index: Int }
}

pub type MintRedeemer {
  AuthenticateDeposit {
    nonce_input_index: Int,
    deposit_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnDepositNFT {
    l2_id: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}

/// Witness script's prefix from its single CBOR encoded applied bytearray.
/// TODO: This should probably be moved to `env`.
pub const witness_script_prefix: ByteArray =
  #"59016101010033232323232323223225333004323232323253330093370e900318051baa00113232323232323253330103370e900000209998081801998011bab30013012375401400a941288a99980819b87480080104ccc040c00ccc008dd5980098091baa00a0054a09445281180a980b180b180b180b0009119299980899b8748008c048dd50008a5eb7bdb1804dd5980b18099baa001323300100100322533301500114c103d87a800013233322253330163372200e0062a66602c66e3c01c00c4cdd2a4000660346e980092f5c02980103d87a8000133006006001375c60280026eacc054004c064008c05c00494ccc0440045288a50300d3754602260240066eb8010dd6980780098059baa00116300d300e003300c002300b002300b0013006375400229309b2b1bae0015734aae7555cf2ab9f5740ae855d12601225820"

pub fn nonce_out_ref_to_l2_id(nonce_out_ref: OutputReference) -> AssetName {
  cbor.serialise(nonce_out_ref) |> blake2b_256
}

pub fn validate_witness_redeemer(
  expected_witness_script_hash: ScriptHash,
  witness_redeemer_index: Int,
  for_registration: Bool,
  event_policy_id: PolicyId,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  // Grab redeemer pair of witness script registration.
  expect Some(witness_redeemer_pair) =
    redeemers |> list.at(witness_redeemer_index)

  // Grab the included script hash (no validation on `certificate` index,
  // TODO?).
  let present_witness_script_hash =
    if for_registration {
      expect Publish {
        certificate: RegisterCredential {
          credential: Script(witness_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_script_hash
    } else {
      expect Publish {
        certificate: UnregisterCredential {
          credential: Script(witness_unregistration_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_unregistration_script_hash
    }

  // Type cast its redeemer to `PolicyId`.
  expect witness_redeemer: PolicyId = witness_redeemer_pair.2nd

  and {
    // 1. Registered script must match the expected witness.
    present_witness_script_hash == expected_witness_script_hash,
    // 2. Deposit policy ID must be the registration redeemer.
    witness_redeemer == event_policy_id,
  }
}
