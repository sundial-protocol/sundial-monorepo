use cardano/address.{Address}
use cardano/assets.{PolicyId}

pub type Datum {
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
