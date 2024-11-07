use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs.{get_first}
use aiken/interval.{Interval}
use cardano/address.{Address, VerificationKey}
use cardano/assets.{PolicyId, flatten, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/constants.{set_node_prefix}
use midgard/types/datum.{StateDatum}
use midgard/types/general.{
  Common, Empty, Key, LinkedListDatum, NodePair, POSIXTime, PubKeyHash,
}
use midgard/types/ps
use midgard/types/redeemer.{PS_Slash}
use midgard/utils.{
  cover_key, find_cs_by_token_prefix, get_outputs, has_data_cs, parse_node_key,
  valid_node,
}

pub type StateQueueRedeemer {
  SQ_Init
  SQ_Deinit
  SQ_Append {
    prev_node_key: PubKeyHash,
    publisher_key: PubKeyHash,
    publisher_ref_input_index: Int,
  }
  SQ_Merge { root_node_key: PubKeyHash, mergeable_node_key: PubKeyHash }
  SQ_Remove_Fraud_Child {
    prev_node_key: PubKeyHash,
    remove_node_key: PubKeyHash,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_ref_input_index: Int,
    node_input_index: Int,
  }
  SQ_Remove_Fraud_Node {
    prev_valid_node_key: ByteArray,
    remove_node_key: ByteArray,
    publisher_set_prev_node_key: PubKeyHash,
    publisher_set_remove_node_key: PubKeyHash,
    fraud_token_input_index: Int,
    node_input_index: Int,
  }
}

pub fn make_common(
  transaction: Transaction,
  own_cs: PolicyId,
) -> (
  Common<StateDatum>,
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

pub fn node_input_utxo_datum_unsafe(output: Output) -> NodePair<StateDatum> {
  expect Output { value, datum: InlineDatum(raw_node), .. } = output
  expect list_datum: LinkedListDatum<StateDatum> = raw_node
  NodePair { val: value, data: list_datum }
}

pub fn parse_node_output_utxo(
  cs: PolicyId,
  output: Output,
) -> NodePair<StateDatum> {
  expect Output { value, datum: InlineDatum(raw_node), .. } = output
  expect list_datum: LinkedListDatum<StateDatum> = raw_node
  expect [Pair(tn, amount)] =
    value
      |> tokens(cs)
      |> dict.to_pairs()
  expect amount == 1
  let node_key = parse_node_key(tn)
  let datum_key =
    when list_datum.key is {
      Empty -> None
      Key(key) -> Some(key)
    }
  expect node_key == datum_key
  expect list.length(flatten(value)) == 2
  expect valid_node(list_datum)
  expect find_cs_by_token_prefix(value, set_node_prefix) == [cs]
  NodePair { val: value, data: list_datum }
}

pub fn is_publisher_slashed_now_or_before(
  self: Transaction,
  publisher_set_hash: ByteArray,
  prev_node_key: PubKeyHash,
  publisher_node_key: PubKeyHash,
  node_input_index: Int,
) -> Bool {
  let maybe_ps_redeemer = get_first(self.redeemers, Mint(publisher_set_hash))
  expect Some(publisher_input) = list.at(self.inputs, node_input_index)
  if maybe_ps_redeemer == None {
    //In this case we passed as input node the prev_node and we check that covers the slashed key
    let prev_node_datum =
      ps.node_input_utxo_datum_unsafe(publisher_input.output).data
    and {
      prev_node_datum.key == Key { key: prev_node_key },
      valid_publisher_node_input(
        publisher_input,
        publisher_set_hash,
        prev_node_key,
      ),
      cover_key(prev_node_datum, publisher_node_key),
    }
  } else {
    //In this case we passed as input node the slashed node and we check it
    expect Some(ps_redeemer) = maybe_ps_redeemer
    expect PS_Slash {
      prev_node_key: _prev_node_key,
      remove_node_key,
      fraud_token_ref_input_index: _fraud_token_ref_input_index,
    } = ps_redeemer
    and {
      valid_publisher_node_input(
        publisher_input,
        publisher_set_hash,
        publisher_node_key,
      ),
      remove_node_key == publisher_node_key,
    }
  }
}

fn valid_publisher_node_input(
  publisher_input: Input,
  publisher_set_hash: ByteArray,
  publisher_node_key: ByteArray,
) {
  expect VerificationKey(hash) =
    publisher_input.output.address.payment_credential
  and {
    hash == publisher_set_hash,
    quantity_of(
      publisher_input.output.value,
      publisher_set_hash,
      publisher_node_key,
    ) == 1,
  }
}
