use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/constants
use midgard/helpers/ps.{make_common}
use midgard/linked_list.{Common, Empty, Key, NodeDatum, NodeKey}
use midgard/types/config.{PublisherSetConfig}
use midgard/types/datum.{FraudProofValidatorDatum, PublisherDatum}
use midgard/types/general.{POSIXTime, PubKeyHash}
use midgard/types/redeemer.{
  PS_CompleteRefund, PS_Deinit, PS_Init, PS_InitRefund, PS_Insert, PS_Slash,
  PublisherSetRedeemer,
}
use midgard/utils

validator publisher_set(
  initial_utxo: Output,
  cfg: PublisherSetConfig,
  fraud_proof_hash: ByteArray,
) {
  mint(redeemer: PublisherSetRedeemer, policy_id: PolicyId, self: Transaction) {
    let (common, inputs, outputs, sigs, vrange) = make_common(self, policy_id)
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
      PS_Insert { key_to_insert, prev_node_key } -> {
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
      PS_Slash { prev_node_key, remove_node_key, fraud_token_ref_input_index } -> {
        expect Some(fraud_token_input) =
          list.at(self.reference_inputs, fraud_token_ref_input_index)
        slash(
          common,
          prev_node_key,
          remove_node_key,
          outputs,
          fraud_token_input,
        )
      }
      PS_InitRefund { node_to_mark_key } -> {
        expect Finite(validTo) = vrange.upper_bound.bound_type
        init_refund(common, node_to_mark_key, sigs, validTo)
      }
      PS_CompleteRefund { prev_node_key, remove_node_key } -> {
        expect Finite(validFrom) = vrange.lower_bound.bound_type
        complete_refund(
          common,
          prev_node_key,
          remove_node_key,
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

fn init(common: Common<PublisherDatum>) -> Bool {
  let must_spend_nodes = list.length(common.node_inputs) == 0
  expect [root_node] = common.node_outputs
  let root_node_datum =
    NodeDatum {
      key: Key { key: constants.root_node_token_name },
      link: Empty,
      data: PublisherDatum {
        insert_time: constants.posix_time_none,
        refund_process_init_time: constants.posix_time_none,
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
    root_node == root_node_datum,
    must_spend_nodes,
    must_mint_correctly,
  }
}

fn deinit(_common: Common<PublisherDatum>) -> Bool {
  True
}

fn insert(
  common: Common<PublisherDatum>,
  insert_key: PubKeyHash,
  prev_node_key: PubKeyHash,
  validTo: POSIXTime,
  min_lovelace_bond: Int,
) -> Bool {
  todo
}

fn slash(
  common: Common<PublisherDatum>,
  prev_node_key: PubKeyHash,
  remove_node_key: PubKeyHash,
  outputs: List<Output>,
  fraud_token_ref_input: Input,
) -> Bool {
  todo
}

fn init_refund(
  common: Common<PublisherDatum>,
  node_to_mark_key: PubKeyHash,
  sigs: List<PubKeyHash>,
  validTo: POSIXTime,
) -> Bool {
  todo
}

fn complete_refund(
  common: Common<PublisherDatum>,
  prev_node_key: PubKeyHash,
  remove_node_key: PubKeyHash,
  sigs: List<PubKeyHash>,
  validFrom: POSIXTime,
  refund_waiting_period: Int,
) -> Bool {
  todo
}
