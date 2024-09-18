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
  SQ_Append, SQ_Deinit, SQ_Fraud_Fold, SQ_Init, SQ_Invalidate, SQ_Merge,
  StateDatum, StateQueueConfig, StateQueueRedeemer, make_common,
}
use midgard/utils

validator state_queue(
  cfg: StateQueueConfig,
  publisher_set_hash: ByteArray,
  publisher_set_asset_name: ByteArray,
) {
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
      SQ_Merge { root_node_key, mergeable_node_key } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        merge(common, root_node_key, mergeable_node_key, validTo)
      }
      SQ_Invalidate { prev_node_key, invalidate_node_key } ->
        invalidate(common, prev_node_key, invalidate_node_key)
      SQ_Fraud_Fold { prev_valid_node_key, fraud_node_keys } ->
        fraud_fold(common, prev_valid_node_key, fraud_node_keys)
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common<StateDatum>) -> Bool {
  let must_spend_nodes = list.length(common.node_inputs) == 0
  expect [root_node] = common.node_outputs
  let root_node_datum =
    LinkedListDatum {
      key: Key { key: constants.root_node_token_name },
      next: Empty,
      extra_data: StateDatum { state_time: -1 },
    }
  let must_mint_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.root_node_token_name,
      1,
    )
  and {
    root_node.data == root_node_datum,
    must_spend_nodes,
    must_mint_correctly,
  }
}

fn deinit(_common: Common<StateDatum>) -> Bool {
  True
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
  validTo: POSIXTime,
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
  let is_mergeable_node_key_oldest =
    root_node.data.next == mergeable_node.data.key
  let enough_time_passed =
    validTo - mergeable_node.data.extra_data.state_time > cfg.refund_waiting_period
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
    enough_time_passed,
    must_spend_two_nodes,
    correct_root_node_output,
    must_mint_correct,
  }
}

fn invalidate(
  common: Common<StateDatum>,
  prev_node_key: ByteArray,
  invalidate_node_key: ByteArray,
) -> Bool {
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  expect Some(prev_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: prev_node_key } },
    )
  expect Some(invalidate_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: invalidate_node_key } },
    )
  let must_cover_remove_key =
    utils.cover_key(prev_node.data, invalidate_node_key)
  let prev_node_new_datum =
    LinkedListDatum {
      key: prev_node.data.key,
      next: Empty,
      extra_data: prev_node.data.extra_data,
    }
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && prev_node_new_datum == node_pair.data
      },
    )
  //TODO
  let user_has_fraud_proof_token = False
  let must_mint_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.invalidation_token_name,
      1,
    )
  expect Key(invalidate_node_key) = invalidate_node.data.key
  let invalidated_state_token_name =
    bytearray.concat(constants.root_node_token_name, invalidate_node_key)
  let correct_invalidated_node_output =
    list.any(common.node_outputs, fn(node_pair) { and {
          invalidate_node.data == node_pair.data,
          quantity_of(
            node_pair.val,
            common.own_cs,
            invalidated_state_token_name,
          ) == 1,
          quantity_of(
            node_pair.val,
            common.own_cs,
            constants.invalidation_token_name,
          ) == 1,
          length(flatten(node_pair.val)) <= 3,
        } })
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_prev_node_output,
    correct_invalidated_node_output,
    user_has_fraud_proof_token,
    must_mint_correctly,
  }
}

fn fraud_fold(
  common: Common<StateDatum>,
  prev_valid_node_key: ByteArray,
  fraud_node_keys: List<ByteArray>,
) {
  let nodes =
    list.map(
      fraud_node_keys,
      fn(node_key) {
        expect Some(node) =
          list.find(
            common.node_inputs,
            fn(input) { input.data.key == Key { key: node_key } },
          )
        node
      },
    )

  expect Some(prev_valid_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: prev_valid_node_key } },
    )

  let first_node_has_invalidation_token =
    quantity_of(
      utils.safe_list_head(nodes).val,
      common.own_cs,
      constants.invalidation_token_name,
    ) == 1
  //TODO We need the expect in the fold, we don't need the result...
  let all_nodes_connected =
    list.foldl(
      nodes,
      prev_valid_node.data.next,
      fn(node, result) {
        expect result == node.data.key
        node.data.next
      },
    ) != prev_valid_node.data.next
  let is_last_of_the_queue = utils.safe_list_last(nodes).data.next == Empty

  let correct_outputs =
    if is_last_of_the_queue {
      length(common.node_outputs) == 0
    } else {
      expect [output] = common.node_outputs
      let correct_output_datum = utils.safe_list_last(nodes).data == output.data
      let invalidated_state_token_name =
        bytearray.concat(
          constants.root_node_token_name,
          utils.safe_list_last(fraud_node_keys),
        )
      and {
        correct_output_datum,
        quantity_of(output.val, common.own_cs, invalidated_state_token_name) == 1,
        quantity_of(
          output.val,
          common.own_cs,
          constants.invalidation_token_name,
        ) == 1,
      }
    }

  let correct_burning =
    if is_last_of_the_queue {
      and {
        utils.validate_mint(
          common.mint,
          common.own_cs,
          constants.invalidation_token_name,
          -1,
        ),
        //TODO We need the expect in the fold, we don't need the result...
        list.foldl(
          nodes,
          True,
          fn(node, _result) {
            let state_token_name =
              bytearray.concat(
                constants.root_node_token_name,
                utils.safe_get_key(node.data.key),
              )
            expect
              utils.validate_mint(
                common.mint,
                common.own_cs,
                state_token_name,
                -1,
              )
            True
          },
        ),
      }
    } else {
      //TODO We need the expect in the fold, we don't need the result...
      list.foldl(
        utils.safe_list_init(nodes),
        True,
        fn(node, _result) {
          let state_token_name =
            bytearray.concat(
              constants.root_node_token_name,
              utils.safe_get_key(node.data.key),
            )
          expect
            utils.validate_mint(
              common.mint,
              common.own_cs,
              state_token_name,
              -1,
            )
          True
        },
      )
    }

  and {
    first_node_has_invalidation_token,
    all_nodes_connected,
    is_last_of_the_queue,
    correct_outputs,
    correct_burning,
  }
}
