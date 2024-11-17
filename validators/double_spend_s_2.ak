

// ------- COMPUTATION STEP 2 -------
// verify Merkle inclusion proof for tx2 (tx2 exists in block with a given tx_root)
// ----------------------------------


// StepDatum contains verified tx1 (state_step1) 
// verify uniqueness tx2 ≠ tx1 
// outgoing StepDatum contains verified tx1 and tx2
// fraud_prover field matches between the input and output datum

// pub type DoubleSpendStep2Redeemer {
//   DS2_Cancel
//   DS2_Continue {
//     frauded_state_queue_node_ref_input_index: Int,
//     unverified_frauded_state_queue_tx2_hash: ByteArray,
//     unverified_frauded_state_queue_tx2_info_hash: ByteArray,
//     unverified_frauded_state_queue_tx2_to_root_proof_path: Proof,
//   }
// }

// verifiedTx1.tx_hash /= verifiedTx2.tx_hash

// data Step2State = {
//   verifiedTx1 = TxHash,
//   verifiedTx2 = TxHash
//   }


