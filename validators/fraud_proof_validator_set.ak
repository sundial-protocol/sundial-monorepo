use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, Output, Transaction}
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
    let (common, inputs, outputs, sigs, vrange) = make_common(self, policy_id)
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
        expect list.any(sigs, fn(sig) { sig == key_to_insert })
        insert(
          common,
          key_to_insert,
          prev_node_key,
          validTo,
          cfg.min_lovelace_bond,
        )
      }
      FPVS_Remove { prev_node_key, remove_node_key } ->
        remove(common, prev_node_key, remove_node_key, outputs)
    }
  }

  else(_) {
    fail
  }
}

fn init(common: Common<FraudProofValidatorDatum>) -> Bool {
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

fn deinit(common: Common<FraudProofValidatorDatum>) -> Bool {
  expect [root_node] = common.node_inputs
  expect Empty = root_node.data.next
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

fn insert(
  common: Common<FraudProofValidatorDatum>,
  insert_key: PubKeyHash,
  prev_node_key: PubKeyHash,
  validTo: POSIXTime,
  min_lovelace_bond: Int,
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
      extra_data: FraudProofValidatorDatum { insert_time: validTo },
    }
  let correct_new_node_output =
    list.any(common.node_outputs, fn(node_pair) { and {
          new_node_datum == node_pair.data,
          quantity_of(node_pair.val, "", "") >= min_lovelace_bond,
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
    utils.validate_mint(
      common.mint,
      common.own_cs,
      bytearray.concat(constants.root_node_token_name, insert_key),
      1,
    )
  and {
    must_cover_inserting_key,
    correct_new_node_output,
    correct_prev_node_output,
    must_mint_correct,
  }
}

fn remove(
  common: Common<FraudProofValidatorDatum>,
  prev_node_key: PubKeyHash,
  remove_node_key: PubKeyHash,
  outputs: List<Output>,
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
    correct_prev_node_output,
    must_mint_correct,
    user_must_have_fraud_proof_token,
    bond_goes_to_treasury_hash,
  }
}
