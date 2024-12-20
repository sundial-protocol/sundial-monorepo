use cardano/address.{Address}

pub type Datum {
  mpf_root: ByteArray,
  processed: Bool,
  publisher: Address,
}

pub type Redeemer {
  ClaimAsProcessed
  RevokeProcessedFlag
  ConcludeSettlement
}
