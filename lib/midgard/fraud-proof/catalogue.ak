use midgard/common/types.{POSIXTime}

pub type Datum {
  insert_time: POSIXTime,
  init_step_script_hash: ByteArray,
}

pub type MintRedeemer {
  Init
  Deinit
  NewFraudCategory
  RemoveFraudCategory
}

pub type SpendRedeemer {
  fraud_proof_catalogue_asset_name: ByteArray,
}
