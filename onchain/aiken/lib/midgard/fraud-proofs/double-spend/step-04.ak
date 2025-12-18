use cardano/transaction.{OutputReference, TransactionId}
use midgard/computation_thread as ct

pub type State {
  verified_tx1_hash: TransactionId,
  verified_tx2_hash: TransactionId,
  verified_common_input: OutputReference,
}

pub type Datum =
  ct.StepDatum<State>

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue { index_output: Int, computation_thread_redeemer_index: Int }
}
