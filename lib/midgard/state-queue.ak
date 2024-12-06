use cardano/transaction.{OutputReference}
use midgard/common/types.{POSIXTime}

pub type Config {
  init_utxo: OutputReference,
  refund_waiting_period: Int,
}

pub type Datum {
  state_time: POSIXTime,
  publisher_key: ByteArray,
  block_hash: ByteArray,
}

pub type MintRedeemer {
  Remove_Fraudolent_Block_Header { fraudolent_operator: ByteArray }
  Commit_Block_Header { operator: ByteArray }
}
