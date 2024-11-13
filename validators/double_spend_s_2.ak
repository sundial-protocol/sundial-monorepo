// spend validator -> double_spend_s2: verify membership Merkle proof, that tx2 exists in tx_root 

// StepDatum contains verified tx1 (state_step1) 
// verify uniqueness tx2 ≠ tx1 

// ------- COMPUTATION STEP 2 -------
// verify Merkle inclusion proof for tx2 (tx2 exists in block with a given tx_root)
// ----------------------------------

// outgoing StepDatum contains verified tx1 and tx2
// fraud_prover field matches between the input and output datum

