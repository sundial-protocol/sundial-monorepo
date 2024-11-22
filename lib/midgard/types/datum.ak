use cardano/address.{Address}
use cardano/assets.{PolicyId}
use midgard/types/general.{POSIXTime}

pub type HubOracleDatum {
  registered_operators: PolicyId,
  active_operators: PolicyId,
  retired_operators: PolicyId,
  scheduler: PolicyId,
  state_queue: PolicyId,
  fraud_proof_catalogue: PolicyId,
  fraud_proof: PolicyId,
  registered_operators_addr: Address,
  active_operators_addr: Address,
  retired_operators_addr: Address,
  scheduler_addr: Address,
  state_queue_addr: Address,
  fraud_proof_catalogue_addr: Address,
  fraud_proof_addr: Address,
}

pub type PublisherDatum {
  insert_time: POSIXTime,
  refund_process_init_time: POSIXTime,
}

pub type StateDatum {
  state_time: POSIXTime,
  publisher_key: ByteArray,
  block_hash: ByteArray,
}

pub type FraudProofValidatorDatum {
  insert_time: POSIXTime,
  init_step_script_hash: ByteArray,
}

pub type StepDatum {
  fraud_prover: ByteArray,
  data: Data,
}

pub type FraudProofSpendDatum {
  fraud_prover: ByteArray,
}

pub type RegisteredOperatorData {
  registration_time: POSIXTime,
}

pub type ActiveOperatorData {
  commitment_time: Option<POSIXTime>,
}

pub type RetiredOperatorData {
  commitment_time: Option<POSIXTime>,
}
