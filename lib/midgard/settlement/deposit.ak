pub type Datum {
  mpf_root: ByteArray,
  processed: Bool,
  publisher_key: ByteArray,
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
  MintNFT { state_queue_input_index: Int, hub_ref_input_index: Int }
  BurnNFT
}
