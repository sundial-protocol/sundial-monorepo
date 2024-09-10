use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval, is_entirely_before}
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
      PS_Insert { key_to_insert, covering_node } -> {
        expect is_entirely_before(vrange, cfg.deadline)
        expect Finite(validTo) = self.validity_range.upper_bound.bound_type
        expect list.any(sigs, fn(sig) { sig == key_to_insert })
        insert(
          common,
          key_to_insert,
          covering_node,
          validTo,
          cfg.min_lovelace_bond,
        )
      }
      PS_Slash { key_to_remove, covering_node, remove_set_node } -> {
        expect is_entirely_before(vrange, cfg.deadline)
        slash(common, key_to_remove, covering_node, remove_set_node)
      }
      PS_InitRefund { key_to_mark } -> init_refund(common, key_to_mark)
      PS_CompleteRefund { key_to_remove, covering_node } -> {
        expect is_entirely_before(vrange, cfg.deadline)
        complete_refund(
          common,
          vrange,
          cfg,
          outputs,
          sigs,
          key_to_remove,
          covering_node,
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
  node: SetNode,
  validTo: POSIXTime,
  min_lovelace_bond: Int,
) -> Bool {
  let must_cover_inserting_key = utils.cover_key(node, insert_key)
  expect [covering_node] = common.node_inputs
  let covering_node_datum = covering_node.publisher_data
  let prev_node_new_datum =
    PublisherDatum {
      insert_time: covering_node_datum.insert_time,
      refund_process_init_time: covering_node_datum.refund_process_init_time,
      node: SetNode { key: node.key, next: Key { key: insert_key } },
    }
  let new_node_datum =
    PublisherDatum {
      insert_time: validTo,
      refund_process_init_time: -1,
      node: SetNode { key: Key { key: insert_key }, next: node.next },
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
        covering_node.val == node_pair.val && node_pair.publisher_data == prev_node_new_datum
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
  remove_key: PubKeyHash,
  prev_set_node: SetNode,
  remove_set_node: SetNode,
) -> Bool {
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
  let stay_node_new_datum =
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
        stay_node.val == node_pair.val && stay_node_new_datum == node_pair.publisher_data
      },
    )
  //TODO
  let user_must_have_fraud_proof_token = True
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  //TODO
  let bond_goes_to_treasury_hash = True
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_stay_node_output,
    must_mint_correct,
    user_must_have_fraud_proof_token,
    bond_goes_to_treasury_hash,
  }
}

fn init_refund(common: Common, key_to_mark: PubKeyHash) -> Bool {
  False
  //TODO
}

fn complete_refund(
  common: Common,
  range: Interval<POSIXTime>,
  disc_config: PublisherSetConfig,
  outs: List<Output>,
  sigs: List<PubKeyHash>,
  remove_key: PubKeyHash,
  node: SetNode,
) -> Bool {
  False
  //TODO
  //let must_be_signed_by_user = list.has(sigs, remove_key)
}
