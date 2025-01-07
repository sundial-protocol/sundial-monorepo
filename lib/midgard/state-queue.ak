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
  RemoveFraudolentBlockHeader { fraudolent_operator: ByteArray }
  CommitBlockHeader { operator: ByteArray }
}
