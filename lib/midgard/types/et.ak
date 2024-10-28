use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs.{get_first}
use aiken/interval.{Interval}
use cardano/address.{Address, VerificationKey}
use cardano/assets.{AssetName, PolicyId, Value, flatten, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/constants.{set_node_prefix}
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime, PubKeyHash,
}
use midgard/types/ps.{PS_Slash}
use midgard/utils.{
  cover_key, find_cs_by_token_prefix, get_outputs, has_data_cs, parse_node_key,
  valid_node,
}

pub type EvaluationThreadConfig {
  fraud_proof_validator_set_script_hash: ByteArray,
}

pub type EvaluationThreadDatum {
  prover_pub_key_hash: ByteArray,
  frauded_state_queue_node: ByteArray,
  first_carrier_validator_script_hash: ByteArray,
}

pub type EvaluationThreadRedeemer {
  ET_ABORT
  ET_CONTINUE_PROOF { frauded_state_queue_node_ref_input_index: Int }
  ET_FINISH_PROOF { frauded_state_queue_node_ref_input_index: Int }
}

pub fn etTokenBurnt(value: Value, policy_id: PolicyId, asset_name: AssetName) {
  quantity_of(value, policy_id, asset_name) == -1
}
