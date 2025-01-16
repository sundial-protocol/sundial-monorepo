use cardano/transaction.{OutputReference}
use midgard/fraud_proof/common/types.{MidgardTx, MidgardTxBody, Proof}

// ----- STEP REDEEMER TYPES -----
pub type Step01Redeemer {
  Cancel_1 { index_output: Int }
  Continue_1 {
    frauded_state_queue_node_ref_input_index: Int,
    frauded_tx1_hash: ByteArray,
    frauded_tx1_info_hash: ByteArray,
    tx1_to_root_proof_path: Proof,
    index_output: Int,
  }
}

pub type Step02Redeemer {
  Cancel_2 { index_output: Int }
  Continue_2 {
    frauded_state_queue_node_ref_input_index: Int,
    frauded_tx2_hash: ByteArray,
    frauded_tx2_info_hash: ByteArray,
    tx2_to_root_proof_path: Proof,
    index_output: Int,
  }
}

pub type Step03Redeemer {
  Cancel_3 { index_output: Int }
  Continue_3 {
    frauded_input: Data,
    tx1_input_utxos: ByteArray,
    tx1_body: MidgardTxBody,
    tx2_input_utxos: ByteArray,
    tx2_body: MidgardTxBody,
    tx1: MidgardTx,
    tx2: MidgardTx,
    index_output: Int,
  }
}

pub type Step04Redeemer {
  Cancel_4 { index_output: Int }
  Continue_4 { index_output: Int }
}

// ----- STEP STATE TYPES -----
pub type DoubleSpendStep1State {
  verified_tx1_hash: ByteArray,
}

pub type DoubleSpendStep2State {
  verified_tx1_hash: ByteArray,
  verified_tx2_hash: ByteArray,
}

pub type DoubleSpendStep3State {
  verified_tx1_hash: ByteArray,
  verified_tx2_hash: ByteArray,
  verified_common_input: OutputReference,
}
