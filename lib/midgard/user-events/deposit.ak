use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
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
  BurnDepositNFT { l2_id: AssetName }
}

pub fn nonce_out_ref_to_l2_id(nonce_out_ref: OutputReference) -> AssetName {
  cbor.serialise(nonce_out_ref) |> blake2b_256
}
