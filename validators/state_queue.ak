use aiken/cbor.{serialise}
use aiken/collection/list.{length}
use aiken/crypto.{blake2b_224}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/constants
use midgard/types/fpvs.{FraudProofValidatorDatum}
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime,
}
use midgard/types/sq.{
  SQ_Append, SQ_Deinit, SQ_Init, SQ_Merge, SQ_Remove_Fraud_Child,
  SQ_Remove_Fraud_Node, StateDatum, StateQueueConfig, StateQueueRedeemer,
  is_publisher_slashed_now_or_before, make_common,
}
use midgard/utils

validator state_queue(
  cfg: StateQueueConfig,
  publisher_set_hash: ByteArray,
  fraud_proof_hash: ByteArray,
) {
  mint(redeemer: StateQueueRedeemer, policy_id: PolicyId, self: Transaction) {
    let (common, inputs, _outputs, sigs, vrange) = make_common(self, policy_id)
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
      SQ_Append { prev_node_key, publisher_key, publisher_ref_input_index } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        expect Some(publisher_input) =
          list.at(self.reference_inputs, publisher_ref_input_index)
        expect list.any(sigs, fn(sig) { sig == publisher_key })
        let publisher_token_name =
          bytearray.concat(constants.root_node_token_name, publisher_key)
        and {
          //We don't need to check that the publisher_input comes from the PS contract as the publisher NFTs cannot leave the contract
          quantity_of(
            publisher_input.output.value,
            publisher_set_hash,
            publisher_token_name,
          ) == 1,
          //TODO check that publisher_turn_to_publish
          True,
          append(
            common,
            blake2b_224(
              serialise(StateDatum { state_time: validTo, publisher_key }),
            ),
            prev_node_key,
            publisher_key,
            validTo,
          ),
        }
      }
      SQ_Merge { root_node_key, mergeable_node_key } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        merge(common, root_node_key, mergeable_node_key, validTo)
      }
      SQ_Remove_Fraud_Child {
        prev_node_key,
        remove_node_key,
        publisher_set_prev_node_key,
        publisher_set_remove_node_key,
        fraud_token_ref_input_index,
        slash_publisher_node_input_index,
      } -> {
        expect Some(fraud_token_ref_input) =
          list.at(self.reference_inputs, fraud_token_ref_input_index)
        and {
          is_publisher_slashed_now_or_before(
            self,
            publisher_set_hash,
            publisher_set_prev_node_key,
            publisher_set_remove_node_key,
            slash_publisher_node_input_index,
          ),
          remove_fraud_child(
            common,
            prev_node_key,
            remove_node_key,
            fraud_token_ref_input,
            fraud_proof_hash,
          ),
        }
      }
      SQ_Remove_Fraud_Node {
        prev_valid_node_key,
        remove_node_key,
        publisher_set_prev_node_key,
        publisher_set_remove_node_key,
        fraud_token_input_index,
        slash_publisher_node_input_index,
      } -> {
        expect Some(fraud_token_input) =
          list.at(self.inputs, fraud_token_input_index)
        and {
          is_publisher_slashed_now_or_before(
            self,
            publisher_set_hash,
            publisher_set_prev_node_key,
            publisher_set_remove_node_key,
            slash_publisher_node_input_index,
          ),
          remove_fraud_node(
            common,
            prev_valid_node_key,
            remove_node_key,
            fraud_token_input,
            fraud_proof_hash,
          ),
        }
      }
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
      extra_data: StateDatum { state_time: 0, publisher_key: "NONE" },
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
  publisher_key: ByteArray,
  validTo: POSIXTime,
) -> Bool {
  expect [prev_node] = common.node_inputs
  expect prev_node.data.key == Key { key: prev_node_key }
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
      next: Empty,
      extra_data: StateDatum { state_time: validTo, publisher_key },
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

  let must_mint_correctly =
    utils.validate_mint(common.mint, common.own_cs, new_token_name, 1)
  and {
    is_prev_node_last,
    correct_new_node_output,
    correct_prev_node_output,
    must_mint_correctly,
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
  let must_mint_correctly =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  and {
    is_root,
    is_mergeable_node_key_oldest,
    enough_time_passed,
    must_spend_two_nodes,
    correct_root_node_output,
    must_mint_correctly,
  }
}

fn remove_fraud_child(
  common: Common<StateDatum>,
  prev_node_key: ByteArray,
  remove_node_key: ByteArray,
  fraud_token_input: Input,
  fraud_proof_hash: ByteArray,
) -> Bool {
  expect Output { value, datum: InlineDatum(raw_fraud_datum), .. } =
    fraud_token_input.output
  let correct_input_fraud_token =
    quantity_of(value, fraud_proof_hash, constants.invalidation_token_name) == 1
  expect fraud_input_datum: LinkedListDatum<FraudProofValidatorDatum> =
    raw_fraud_datum
  expect Key(fraud_input_key) = fraud_input_datum.key
  let fraud_input_key_is_prev_node_key = fraud_input_key == prev_node_key
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  expect Some(prev_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: prev_node_key } },
    )
  expect Some(remove_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: remove_node_key } },
    )
  let prev_node_new_datum =
    LinkedListDatum {
      key: prev_node.data.key,
      next: remove_node.data.key,
      extra_data: prev_node.data.extra_data,
    }
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && prev_node_new_datum == node_pair.data
      },
    )
  let remove_token_name =
    bytearray.concat(constants.root_node_token_name, remove_node_key)
  let must_burn_node_nft =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  and {
    correct_input_fraud_token,
    fraud_input_key_is_prev_node_key,
    must_spend_two_nodes,
    correct_prev_node_output,
    must_burn_node_nft,
  }
}

fn remove_fraud_node(
  common: Common<StateDatum>,
  prev_valid_node_key: ByteArray,
  remove_node_key: ByteArray,
  fraud_token_input: Input,
  fraud_proof_hash: ByteArray,
) {
  expect Output { value, datum: InlineDatum(raw_fraud_datum), .. } =
    fraud_token_input.output
  let correct_input_fraud_token =
    quantity_of(value, fraud_proof_hash, constants.invalidation_token_name) == 1
  expect fraud_input_datum: LinkedListDatum<FraudProofValidatorDatum> =
    raw_fraud_datum
  expect Key(fraud_input_key) = fraud_input_datum.key
  let fraud_input_key_is_remove_node_key = fraud_input_key == remove_node_key
  let must_spend_two_nodes = list.length(common.node_inputs) == 2
  expect Some(prev_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: prev_valid_node_key } },
    )
  expect Some(remove_node) =
    list.find(
      common.node_inputs,
      fn(input) { input.data.key == Key { key: remove_node_key } },
    )
  let prev_node_new_datum =
    LinkedListDatum {
      key: prev_node.data.key,
      next: remove_node.data.key,
      extra_data: prev_node.data.extra_data,
    }
  let correct_prev_node_output =
    list.any(
      common.node_outputs,
      fn(node_pair) {
        prev_node.val == node_pair.val && prev_node_new_datum == node_pair.data
      },
    )
  let remove_token_name =
    bytearray.concat(constants.root_node_token_name, remove_node_key)
  let must_burn_node_nft =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  let must_burn_fraud_proof_token =
    utils.validate_mint(
      common.mint,
      fraud_proof_hash,
      constants.invalidation_token_name,
      -1,
    )
  and {
    correct_input_fraud_token,
    fraud_input_key_is_remove_node_key,
    must_spend_two_nodes,
    correct_prev_node_output,
    must_burn_node_nft,
    must_burn_fraud_proof_token,
  }
}
