use midgard/types/general.{PubKeyHash, SetNode}

pub type FraudProofValidatorSetRedeemer {
  FPVS_Init
  FPVS_Deinit
  FPVS_Insert { key_to_insert: PubKeyHash, covering_node: SetNode }
  FPVS_Remove { key_to_remove: PubKeyHash, covering_node: SetNode }
}
