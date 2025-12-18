use cardano/transaction.{OutputReference, TransactionId}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact}

pub type State {
  verified_tx1_hash: TransactionId,
  verified_tx2_hash: TransactionId,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer {
  Cancel { index_output: Int }
  Continue {
    fraudulent_input: Data,
    tx1_input_utxos: ByteArray,
    tx1_body: MidgardTxBodyCompact,
    tx2_input_utxos: ByteArray,
    tx2_body: MidgardTxBodyCompact,
    tx1: MidgardTxCompact,
    tx2: MidgardTxCompact,
    index_output: Int,
  }
}
