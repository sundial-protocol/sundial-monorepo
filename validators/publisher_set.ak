use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, Output, Transaction}
use midgard/constants
use midgard/types/general.{Empty, Key, POSIXTime, PubKeyHash, SetNode}
use midgard/types/ps.{
  Common, NodePair, PS_CompleteRefund, PS_Deinit, PS_Init, PS_InitRefund,
  PS_Insert, PS_Slash, PublisherDatum, PublisherSetConfig, PublisherSetRedeemer,
}
use midgard/utils

validator publisher_set(cfg: PublisherSetConfig) {
  mint(redeemer: PublisherSetRedeemer, policy_id: PolicyId, self: Transaction) {
    let (common, inputs, outputs, sigs, vrange) =
      utils.make_common(self, policy_id)
    when redeemer is {
      PS_Init -> {
        expect
          list.any(
            inputs,
            fn(input) { cfg.init_utxo == input.output_reference },
          )
        init(common)
      }
      PS_Deinit -> deinit(common)
      PS_Insert { key_to_insert, prev_node } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        expect list.any(sigs, fn(sig) { sig == key_to_insert })
        insert(common, key_to_insert, prev_node, validTo, cfg.min_lovelace_bond)
      }
      PS_Slash { prev_node, remove_node } ->
        slash(common, prev_node, remove_node, outputs)
      PS_InitRefund { node_to_mark } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        init_refund(common, node_to_mark, sigs, validTo)
      }
      PS_CompleteRefund { prev_node, remove_node } -> {
        expect Finite(validFrom) = vrange.lower_bound.bound_type
        complete_refund(
          common,
          prev_node,
          remove_node,
          sigs,
          validFrom,
          cfg.refund_waiting_period,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common) -> Bool {
  let must_spend_nodes = list.length(common.node_inputs) == 0
  let must_exactly_one_node_output = list.length(common.node_outputs) == 1
  let must_mint_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.origin_node_token_name,
      1,
    )
  and {
    must_spend_nodes,
    must_exactly_one_node_output,
    must_mint_correctly,
  }
}

fn deinit(common: Common) -> Bool {
  expect [head_node] = common.node_inputs
  expect Empty = head_node.publisher_data.node.next
  let must_not_produce_node_output = list.length(common.node_outputs) == 0
  let must_burn_correctly =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      constants.origin_node_token_name,
      -1,
    )
  and {
    must_not_produce_node_output,
    must_burn_correctly,
  }
}

fn insert(
  common: Common,
  insert_key: PubKeyHash,
  prev_set_node: SetNode,
  validTo: POSIXTime,
  min_lovelace_bond: Int,
) -> Bool {
  expect [prev_node] = common.node_inputs
  let prev_node_datum = prev_node.publisher_data
  expect prev_node_datum.node == prev_set_node
  let must_cover_inserting_key = utils.cover_key(prev_set_node, insert_key)
  let prev_node_new_datum =
    PublisherDatum {
      insert_time: prev_node_datum.insert_time,
      refund_process_init_time: prev_node_datum.refund_process_init_time,
      node: SetNode { key: prev_set_node.key, next: Key { key: insert_key } },
    }
  let new_node_datum =
    PublisherDatum {
      insert_time: validTo,
      refund_process_init_time: -1,
      node: SetNode { key: Key { key: insert_key }, next: prev_set_node.next },
    }
  let correct_new_node_output =
    list.any(common.node_outputs, fn(node_pair) { and {
          new_node_datum == node_pair.publisher_data,
          quantity_of(node_pair.val, "", "") >= min_lovelace_bond,
          length(flatten(node_pair.val)) <= 3,
        } })
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && node_pair.publisher_data == prev_node_new_datum
      },
    )

  let must_mint_correct =
    utils.validate_mint(
      common.mint,
      common.own_cs,
      bytearray.concat(constants.origin_node_token_name, insert_key),
      1,
    )
  and {
    must_cover_inserting_key,
    correct_new_node_output,
    correct_prev_node_output,
    must_mint_correct,
  }
}

fn slash(
  common: Common,
  prev_set_node: SetNode,
  remove_set_node: SetNode,
  outputs: List<Output>,
) -> Bool {
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  expect Key(remove_key) = remove_set_node.key
  let must_cover_remove_key = utils.cover_key(prev_set_node, remove_key)
  expect Some(stay_node) =
    list.find(
      common.node_inputs,
      fn(input) { prev_set_node == input.publisher_data.node },
    )
  expect Some(remove_node) =
    list.find(
      common.node_inputs,
      fn(input) { remove_set_node == input.publisher_data.node },
    )
  let prev_node_new_datum =
    PublisherDatum {
      insert_time: stay_node.publisher_data.insert_time,
      refund_process_init_time: stay_node.publisher_data.refund_process_init_time,
      node: SetNode {
        key: prev_set_node.key,
        next: remove_node.publisher_data.node.next,
      },
    }
  let remove_token_name =
    bytearray.concat(constants.origin_node_token_name, remove_key)
  let correct_stay_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        stay_node.val == node_pair.val && prev_node_new_datum == node_pair.publisher_data
      },
    )
  //TODO
  let user_must_have_fraud_proof_token = True
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  let bond_goes_to_treasury_hash =
    list.any(
      outputs,
      fn(output) {
        quantity_of(output.value, "", "") >= cfg.min_lovelace_bond && output.address == cfg.midgard_treasury
      },
    )
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_stay_node_output,
    must_mint_correct,
    user_must_have_fraud_proof_token,
    bond_goes_to_treasury_hash,
  }
}

fn init_refund(
  common: Common,
  set_node_to_mark: SetNode,
  sigs: List<PubKeyHash>,
  validTo: POSIXTime,
) -> Bool {
  expect Some(node_to_mark) =
    list.find(
      common.node_inputs,
      fn(input) { set_node_to_mark == input.publisher_data.node },
    )
  expect Key(node_key) = node_to_mark.publisher_data.node.key
  let must_be_signed_by_user = list.has(sigs, node_key)
  let node_to_mark_new_datum =
    PublisherDatum {
      insert_time: node_to_mark.publisher_data.insert_time,
      refund_process_init_time: validTo,
      node: node_to_mark.publisher_data.node,
    }
  let correct_node_to_mark_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        node_to_mark.val == node_pair.val && node_to_mark_new_datum == node_pair.publisher_data
      },
    )
  and {
    must_be_signed_by_user,
    correct_node_to_mark_output,
  }
}

fn complete_refund(
  common: Common,
  prev_set_node: SetNode,
  remove_set_node: SetNode,
  sigs: List<PubKeyHash>,
  validFrom: POSIXTime,
  refund_waiting_period: Int,
) -> Bool {
  expect Key(remove_key) = remove_set_node.key
  let must_cover_remove_key = utils.cover_key(prev_set_node, remove_key)
  expect Some(stay_node) =
    list.find(
      common.node_inputs,
      fn(input) { prev_set_node == input.publisher_data.node },
    )
  expect Some(remove_node) =
    list.find(
      common.node_inputs,
      fn(input) { remove_set_node == input.publisher_data.node },
    )
  let prev_node_new_datum =
    PublisherDatum {
      insert_time: stay_node.publisher_data.insert_time,
      refund_process_init_time: stay_node.publisher_data.refund_process_init_time,
      node: SetNode {
        key: prev_set_node.key,
        next: remove_node.publisher_data.node.next,
      },
    }
  let remove_token_name =
    bytearray.concat(constants.origin_node_token_name, remove_key)
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  let correct_stay_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        stay_node.val == node_pair.val && prev_node_new_datum == node_pair.publisher_data
      },
    )
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  let must_be_signed_by_user = list.has(sigs, remove_key)
  let is_refund_init = remove_node.publisher_data.refund_process_init_time > 0
  let enough_time_passed =
    validFrom - remove_node.publisher_data.refund_process_init_time > refund_waiting_period
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_stay_node_output,
    must_mint_correct,
    must_be_signed_by_user,
    is_refund_init,
    enough_time_passed,
  }
}
