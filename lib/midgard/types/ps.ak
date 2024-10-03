use aiken/collection/dict
use aiken/collection/list
use aiken/interval.{Interval}
use cardano/address.{Address}
use cardano/assets.{PolicyId, from_asset, tokens, without_lovelace}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime, PubKeyHash,
}
use midgard/utils.{get_outputs, has_data_cs, parse_node_key, valid_node}

pub type PublisherSetConfig {
  init_utxo: OutputReference,
  midgard_treasury: Address,
  refund_waiting_period: Int,
  min_lovelace_bond: Int,
}

pub type PublisherDatum {
  insert_time: POSIXTime,
  refund_process_init_time: POSIXTime,
}

pub type PublisherSetRedeemer {
  PS_Init
  PS_Deinit
  PS_Insert { key_to_insert: PubKeyHash, prev_node_key: PubKeyHash }
  PS_Slash {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
  }
  PS_InitRefund { node_to_mark_key: PubKeyHash }
  PS_CompleteRefund { prev_node_key: PubKeyHash, remove_node_key: PubKeyHash }
}

pub fn make_common(
  transaction: Transaction,
  own_cs: PolicyId,
) -> (
  Common<PublisherDatum>,
  List<Input>,
  List<Output>,
  List<PubKeyHash>,
  Interval<POSIXTime>,
) {
  let Transaction {
    inputs,
    outputs,
    mint,
    validity_range,
    extra_signatories,
    ..
  } = transaction
  let in_outputs = get_outputs(inputs)
  let from_node_val = only_at_node_val(in_outputs, own_cs)
  let to_node_val = only_at_node_val(outputs, own_cs)
  expect Some(head) = list.head(list.concat(from_node_val, to_node_val))
  let Output { address: node_address, .. } = head
  expect
    from_node_val
      |> list.concat(to_node_val)
      |> list.reduce(
          True,
          fn(acc, cur_node) {
            let Output { address: cur_address, .. } = cur_node
            and {
              cur_address == node_address,
              acc,
            }
          },
        )
  let node_inputs = list.map(from_node_val, node_input_utxo_datum_unsafe)
  let node_outputs =
    list.map(to_node_val, fn(node) { parse_node_output_utxo(own_cs, node) })
  let common = Common { own_cs, mint, node_inputs, node_outputs }
  (common, inputs, outputs, extra_signatories, validity_range)
}

pub fn only_at_node_val(outputs: List<Output>, cs: PolicyId) -> List<Output> {
  outputs
    |> list.filter(
        fn(output) {
          let Output { value, .. } = output
          has_data_cs(cs, value)
        },
      )
}

pub fn node_input_utxo_datum_unsafe(output: Output) -> NodePair<PublisherDatum> {
  expect Output { value, datum: InlineDatum(raw_node), .. } = output
  expect list_datum: LinkedListDatum<PublisherDatum> = raw_node
  NodePair { val: value, data: list_datum }
}

pub fn parse_node_output_utxo(
  cs: PolicyId,
  output: Output,
) -> NodePair<PublisherDatum> {
  expect Output { value, datum: InlineDatum(raw_node), .. } = output
  expect publisher_datum: LinkedListDatum<PublisherDatum> = raw_node
  expect [Pair(tn, amount)] =
    value
      |> tokens(cs)
      |> dict.to_pairs()
  expect amount == 1
  let node_key = parse_node_key(tn)
  let datum_key =
    when publisher_datum.key is {
      Empty -> None
      Key(key) -> Some(key)
    }
  expect node_key == datum_key
  expect without_lovelace(value) == from_asset(cs, tn, 1)
  expect valid_node(publisher_datum)
  NodePair { val: value, data: publisher_datum }
}
