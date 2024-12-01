// Step - 3: Input: tx_input UTxO | Prove  input UTxO, that same tx_input UTxO is both member of tx1 and tx2

// Merkle proof of membership: i ∈ tx_1.inputs_root ∧ i ∈ tx_2.inputs_root

// ; Transaction Body
// ; Compact Representation

// transaction_body_compact =
//   { 0 : inputs_hash    ; inputs_hash
//   , 1 : outputs_hash   ; outputs_hash
//   , 2 : uint           ; fee
//   , ? 3 : uint         ; validity_interval_end (ttl)
//   , ? 4 : $hash32      ; certificates_hash
//   , ? 5 : $hash32      ; withdrawals_hash
//   , ? 7 : $hash32      ; auxiliary_data_hash
//   , ? 8 : uint         ; validity_interval_start
//   , ? 9 : $hash32      ; mint_hash
//   , ? 11 : $hash32     ; script_data_hash
//   , ? 14 : $hash32     ; required_signers_hash
//   , ? 15 : uint        ; network_id
//   , ? 18 : $hash32     ; reference_inputs_hash
//   , ? 23 : $hash32     ; required_observers_hash
//   }

// ; Transaction Input
// ; Compact Representation
// transaction_input_compact = $hash32

// inputs_set_hash = non_empty_set<transaction_input_compact>
// inputs_hash = $hash32 ; hash of inputs_set_hash

// data Step3Outcome = {
//   verifiedTx1 = TxHash,
//   verifiedTx2 = TxHash,
//   verifiedInputTx1 = TxInput
//   }