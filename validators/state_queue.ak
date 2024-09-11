use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, Output, Transaction}
use midgard/constants
use midgard/types/general.{
  Common, Empty, Key, NodePair, POSIXTime, PubKeyHash, SetNode,
}
use midgard/types/sq.{
  SQ_Append, SQ_Deinit, SQ_Init, SQ_Merge, SQ_Remove, StateDatum,
  StateQueueConfig, StateQueueRedeemer, make_common,
}
use midgard/utils

validator state_queue(cfg: StateQueueConfig) {
  mint(redeemer: StateQueueRedeemer, policy_id: PolicyId, self: Transaction) {
    let (common, inputs, outputs, sigs, vrange) = make_common(self, policy_id)
    when redeemer is {
      SQ_Init -> {
        expect
          list.any(
            inputs,
            fn(input) { cfg.init_utxo == input.output_reference },
          )
        init(common)
      }
      SQ_Deinit -> deinit(common)
      SQ_Append { key_to_insert, prev_node } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        expect list.any(sigs, fn(sig) { sig == key_to_insert })
        append(common, key_to_insert, prev_node, validTo, cfg.min_lovelace_bond)
      }
      SQ_Merge { prev_node, remove_node } ->
        merge(common, prev_node, remove_node, outputs)
      SQ_Remove { prev_node, remove_node } ->
        remove(common, prev_node, remove_node, outputs)
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common<StateDatum>) -> Bool {
  //TODO
  False
}

fn deinit(common: Common<StateDatum>) -> Bool {
  //TODO
  False
}

fn append(
  common: Common<StateDatum>,
  insert_key: PubKeyHash,
  prev_set_node: SetNode,
  validTo: POSIXTime,
  min_lovelace_bond: Int,
) -> Bool {
  //TODO
  False
}

fn merge(
  common: Common<StateDatum>,
  prev_set_node: SetNode,
  remove_set_node: SetNode,
  outputs: List<Output>,
) -> Bool {
  //TODO
  False
}

fn remove(
  common: Common<StateDatum>,
  prev_set_node: SetNode,
  remove_set_node: SetNode,
  outputs: List<Output>,
) -> Bool {
  //TODO
  False
}
