use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{AssetName}
use midgard/common/types.{POSIXTime, PubKeyHash}

pub type Datum {
  mpf_root: ByteArray,
  time_of_claimed_as_processed: Option<POSIXTime>,
  operator_vkey: PubKeyHash,
  tree_content: EventType,
}

pub type EventType {
  Deposit
  Withdrawal
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  purpose: Purpose,
}

pub type Purpose {
  ClaimAsProcessed
  RevokeProcessedFlag {
    unprocessed_event_ref_input_index: Int,
    unprocessed_event_asset_name: AssetName,
    event_type: EventType,
    membership_proof: mpf.Proof,
  }
  ConcludeSettlement
}

pub type MintRedeemer {
  MintNFTs {
    state_queue_input_index: Int,
    hub_ref_input_index: Int,
    confirmed_state_output_index: Int,
  }
  BurnNFT
}
