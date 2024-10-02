use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, Transaction}
use midgard/constants
use midgard/types/fpvs.{
  FPVS_Deinit, FPVS_Init, FPVS_Insert, FPVS_Remove, FraudProofValidatorDatum,
  FraudProofValidatorSetConfig, FraudProofValidatorSetRedeemer, make_common,
}
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime, PubKeyHash,
}
use midgard/utils

validator fraud_proof_validator_set(cfg: FraudProofValidatorSetConfig) {
  mint(
    redeemer: FraudProofValidatorSetRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let (common, inputs, _outputs, _sigs, vrange) = make_common(self, policy_id)
    when redeemer is {
      FPVS_Init -> {
        expect
          list.any(
            inputs,
            fn(input) { cfg.init_utxo == input.output_reference },
          )
        init(common)
      }
      FPVS_Deinit -> deinit(common)
      FPVS_Insert { key_to_insert, prev_node_key } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        insert(common, key_to_insert, prev_node_key, validTo)
      }
      FPVS_Remove { prev_node_key, remove_node_key } ->
        remove(common, prev_node_key, remove_node_key)
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common<FraudProofValidatorDatum>) -> Bool {
  let must_spend_nodes = list.length(common.node_inputs) == 0
  expect [root_node] = common.node_outputs
  let root_node_datum =
    LinkedListDatum {
      key: Key { key: constants.root_node_token_name },
      next: Empty,
      extra_data: FraudProofValidatorDatum {
        insert_time: -1,
        fraud_proof_first_step: "TODO",
      },
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

fn deinit(_common: Common<FraudProofValidatorDatum>) -> Bool {
  True
}

fn insert(
  common: Common<FraudProofValidatorDatum>,
  insert_key: PubKeyHash,
  prev_node_key: PubKeyHash,
  validTo: POSIXTime,
) -> Bool {
  expect [prev_node] = common.node_inputs
  expect prev_node.data.key == Key { key: prev_node_key }
  let must_cover_inserting_key = utils.cover_key(prev_node.data, insert_key)
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
      extra_data: FraudProofValidatorDatum {
        insert_time: validTo,
        fraud_proof_first_step: "TODO",
      },
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
  //TODO
  let user_must_have_governance_token = True
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, new_token_name, 1)
  and {
    must_cover_inserting_key,
    correct_new_node_output,
    correct_prev_node_output,
    user_must_have_governance_token,
    must_mint_correct,
  }
}

fn remove(
  common: Common<FraudProofValidatorDatum>,
  prev_node_key: PubKeyHash,
  remove_node_key: PubKeyHash,
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
  let user_must_have_governance_token = True
  let must_mint_correct =
    utils.validate_mint(common.mint, common.own_cs, remove_token_name, -1)
  and {
    must_cover_remove_key,
    must_spend_two_nodes,
    correct_prev_node_output,
    must_mint_correct,
    user_must_have_governance_token,
  }
}
