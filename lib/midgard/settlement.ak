use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{AssetName}
use midgard/common/types.{POSIXTime, PubKeyHash}

/// The `operator_vkey` is optional to model UTxOs subject to fraudulent claims.
pub type Datum {
  mpf_root: ByteArray,
  time_of_claimed_as_processed: Option<POSIXTime>,
  operator: Option<PubKeyHash>,
  tree_content: EventType,
}

pub type EventType {
  Deposit
  Withdrawal
}

pub type SpendRedeemer {
  UpdateProcessedFlag {
    input_index: Int,
    output_index: Int,
    hub_ref_input_index: Int,
    active_operators_node_input_index: Int,
    active_operators_spend_redeemer_index: Int,
    purpose: Purpose,
  }
  ConcludeSettlement
}

pub type Purpose {
  ClaimAsProcessed
  RevokeProcessedFlag {
    unprocessed_event_ref_input_index: Int,
    unprocessed_event_asset_name: AssetName,
    event_type: EventType,
    membership_proof: mpf.Proof,
  }
}

pub type MintRedeemer {
  MintNFTs {
    state_queue_input_index: Int,
    hub_ref_input_index: Int,
    confirmed_state_output_index: Int,
  }
  BurnNFT
}
