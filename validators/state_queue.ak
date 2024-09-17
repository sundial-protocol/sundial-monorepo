use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, Transaction}
use midgard/constants
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime,
}
use midgard/types/sq.{
  SQ_Append, SQ_Deinit, SQ_Init, SQ_Merge, SQ_Remove, StateDatum,
  StateQueueConfig, StateQueueRedeemer, make_common,
}
use midgard/utils

validator state_queue(cfg: StateQueueConfig) {
  mint(redeemer: StateQueueRedeemer, policy_id: PolicyId, self: Transaction) {
    let (common, inputs, _outputs, _sigs, vrange) = make_common(self, policy_id)
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
      SQ_Append { key_to_insert, prev_node_key } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        append(common, key_to_insert, prev_node_key, validTo)
      }
      SQ_Merge { root_node_key, mergeable_node_key } ->
        merge(common, root_node_key, mergeable_node_key)
      SQ_Remove { prev_node_key, remove_node_key } ->
        remove(common, prev_node_key, remove_node_key)
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common<StateDatum>) -> Bool {
  let must_spend_nodes = list.length(common.node_inputs) == 0
  let must_exactly_one_node_output = list.length(common.node_outputs) == 1
  let must_mint_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.root_node_token_name,
      1,
    )
  and {
    must_spend_nodes,
    must_exactly_one_node_output,
    must_mint_correctly,
  }
}

fn deinit(common: Common<StateDatum>) -> Bool {
  expect [root_node_key] = common.node_inputs
  expect Empty = root_node_key.data.next
  let must_not_produce_node_output = list.length(common.node_outputs) == 0
  let must_burn_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.root_node_token_name,
      -1,
    )
  and {
    must_not_produce_node_output,
    must_burn_correctly,
  }
}

fn append(
  common: Common<StateDatum>,
  insert_key: ByteArray,
  prev_node_key: ByteArray,
  validTo: POSIXTime,
) -> Bool {
  expect [prev_node] = common.node_inputs
  expect prev_node.data.key == Key { key: prev_node_key }
  let must_cover_inserting_key = utils.cover_key(prev_node.data, insert_key)
  //TODO
  let signed_by_publisher = False
  let is_prev_node_last = prev_node.data.next == Empty
  let prev_node_new_datum =
    LinkedListDatum {
      key: prev_node.data.key,
      next: Key { key: insert_key },
      extra_data: prev_node.data.extra_data,
    }
  let new_node_datum =
    LinkedListDatum {
      key: Key { key: insert_key },
      next: prev_node.data.next,
      extra_data: StateDatum { state_time: validTo },
    }
  let new_token_name =
    bytearray.concat(constants.root_node_token_name, insert_key)
  let correct_new_node_output =
    list.any(common.node_outputs, fn(node_pair) { and {
          new_node_datum == node_pair.data,
          quantity_of(node_pair.val, common.own_cs, new_token_name) == 1,
          length(flatten(node_pair.val)) <= 3,
        } })
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && node_pair.data == prev_node_new_datum
      },
    )

  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, new_token_name, 1)
  and {
    must_cover_inserting_key,
    signed_by_publisher,
    is_prev_node_last,
    correct_new_node_output,
    correct_prev_node_output,
    must_mint_correct,
  }
}

fn merge(
  common: Common<StateDatum>,
  root_node_key: ByteArray,
  mergeable_node_key: ByteArray,
) -> Bool {
  expect Some(root_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: root_node_key } },
    )
  expect Some(mergeable_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: mergeable_node_key } },
    )
  let is_root =
    quantity_of(root_node.val, common.own_cs, constants.root_node_token_name) > 0
  //TODO Is the following safe enough?
  let is_mergeable_node_key_oldest =
    root_node.data.next == mergeable_node.data.key
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  let must_cover_remove_key =
    utils.cover_key(root_node.data, mergeable_node_key)

  let root_node_new_datum =
    LinkedListDatum {
      key: root_node.data.key,
      next: mergeable_node.data.next,
      extra_data: mergeable_node.data.extra_data,
    }
  let remove_token_name =
    bytearray.concat(constants.root_node_token_name, mergeable_node_key)
  let correct_root_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        root_node.val == node_pair.val && root_node_new_datum == node_pair.data
      },
    )
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  and {
    must_cover_remove_key,
    is_root,
    is_mergeable_node_key_oldest,
    must_spend_two_nodes,
    correct_root_node_output,
    must_mint_correct,
  }
}

fn remove(
  common: Common<StateDatum>,
  prev_node_key: ByteArray,
  remove_node_key: ByteArray,
) -> Bool {
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  expect Some(prev_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: prev_node_key } },
    )
  let must_cover_remove_key = utils.cover_key(prev_node.data, remove_node_key)
  let prev_node_new_datum =
    LinkedListDatum {
      key: prev_node.data.key,
      next: Empty,
      extra_data: prev_node.data.extra_data,
    }
  let remove_token_name =
    bytearray.concat(constants.root_node_token_name, remove_node_key)
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && prev_node_new_datum == node_pair.data
      },
    )
  //TODO
  let user_has_fraud_proof_token = False
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_prev_node_output,
    must_mint_correct,
    user_has_fraud_proof_token,
  }
}
