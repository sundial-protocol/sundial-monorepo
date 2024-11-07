use midgard/types/general.{
  Empty, Key, LinkedListDatum, NodeKey, POSIXTime, PubKeyHash,
}

pub type PublisherSetRedeemer {
  PS_Init
  PS_Deinit
  PS_Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  PS_Slash {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
  }
  PS_InitRefund { node_to_mark_key: PubKeyHash }
  PS_CompleteRefund { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}

pub type FraudProofValidatorSetRedeemer {
  FPVS_Init
  FPVS_Deinit
  FPVS_Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  FPVS_Remove { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}

pub type ComputationThreadRedeemer {
  CT_Mint {
    fraud_proof_set_node_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    output_to_init_step_index: Int,
    fraud_prover_hash: ByteArray,
    fraud_proof_catalogue_asset_name: ByteArray,
    hub_oracle_asset_name: ByteArray,
    state_queue_asset_name: ByteArray,
  }
  CT_Success { token_to_burn_asset_name: ByteArray }
  CT_Cancel { token_to_burn_asset_name: ByteArray }
}

pub type FraudProofMintRedeemer {
  FP_Mint {
    hub_oracle_ref_input_index: Int,
    computation_thread_token_asset_name: ByteArray,
    output_with_fraud_proof_index: Int,
  }
  FP_Burn {
    hub_oracle_ref_input_index: Int,
    frauded_state_queue_node_ref_input_index: Int,
    fraud_proof_token_asset_name: ByteArray,
  }
}

pub type FraudProofSpendRedeemer {
  fraud_proof_token_asset_name: ByteArray,
}
