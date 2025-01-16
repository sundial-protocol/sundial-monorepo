// TODO
pub type Proof =
  Void

// TODO
pub type MembershipProofInputs {
  tx_root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  tx_to_root_proof_path: Proof,
}

// TODO:
// Some helper types for handling transaction inputs in the fraud proof
// pub type TxInput =
//   Data

// pub type TxInputs =
//   List<TxInput>

// TODO:
pub type MidgardTx {
  body: ByteArray,
  wits: ByteArray,
  is_valid: Bool,
}

// TODO:
pub type MidgardTxBody {
  spend_inputs: ByteArray,
}
