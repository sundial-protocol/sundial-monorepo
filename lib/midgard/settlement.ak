use midgard/common/types.{POSIXTime, PubKeyHash}

pub type Datum {
  mpf_root: ByteArray,
  time_of_claimed_as_processed: Option<POSIXTime>,
  operator_vkey: PubKeyHash,
  tree_content: TreeContent,
}

pub type TreeContent {
  Deposits
  Withdrawals
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  purpose: Purpose,
}

pub type Purpose {
  ClaimAsProcessed
  RevokeProcessedFlag
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
