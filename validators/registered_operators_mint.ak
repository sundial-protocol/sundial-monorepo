use aiken/collection/list.{length}
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, Value, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use linked_list/data.{Common, Empty, Key, NodeDatum, NodeKey}
use linked_list/list as linked_list
use midgard/constants
use midgard/types/config.{PublisherSetConfig}
use midgard/types/datum.{FraudProofValidatorDatum, PublisherDatum}
use midgard/types/general.{POSIXTime, PubKeyHash}
use midgard/types/redeemer.{
  ROM_Activate_Operator, ROM_Deinit, ROM_Deregister_Operator, ROM_Init,
  ROM_Register_Operator, ROM_Remove_Duplicate_slash_Bond,
  RegisteredOperatorsMintRedeemer,
}
use midgard/utils

validator registered_operators(
  init_utxo_id: ByteArray,
  retired_operators_mint_script_hash: PolicyId,
  hub_oracle_script_hash: PolicyId,
) {
  mint(
    redeemer: RegisteredOperatorsMintRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let (common, inputs, outputs, sigs, vrange) =
      linked_list.make_common(self, policy_id)
    when redeemer is {
      ROM_Init(init_utxo_index) -> {
        expect Some(init_input) = list.at(inputs, init_utxo_index)
        let init_input_utxo_id =
          sha2_256(
            bytearray.push(
              init_input.output_reference.transaction_id,
              init_input.output_reference.output_index,
            ),
          )
        and {
          init_input_utxo_id == init_utxo_id,
          init(common, self.mint, hub_oracle_script_hash, init_utxo_id),
        }
      }
      ROM_Deinit ->
        deinit(common, self.mint, hub_oracle_script_hash, init_utxo_id)
      ROM_Register_Operator -> True
      ROM_Activate_Operator -> True
      ROM_Deregister_Operator -> True
      ROM_Remove_Duplicate_slash_Bond -> True
    }
  }

  else(_) {
    fail
  }
}

fn init(
  common: Common,
  mint: Value,
  hub_oracle_script_hash: ByteArray,
  init_utxo_id: ByteArray,
) -> Bool {
  and {
    quantity_of(mint, hub_oracle_script_hash, init_utxo_id) == 1,
    linked_list.init(common),
  }
}

fn deinit(
  common: Common,
  mint: Value,
  hub_oracle_script_hash: ByteArray,
  init_utxo_id: ByteArray,
) -> Bool {
  and {
    quantity_of(mint, hub_oracle_script_hash, init_utxo_id) == -1,
    linked_list.deinit(common),
  }
}
// fn insert(
//   common: Common<PublisherDatum>,
//   insert_key: PubKeyHash,
//   prev_node_key: PubKeyHash,
//   validTo: POSIXTime,
//   min_lovelace_bond: Int,
// ) -> Bool {
//   todo
// }

// fn slash(
//   common: Common<PublisherDatum>,
//   prev_node_key: PubKeyHash,
//   remove_node_key: PubKeyHash,
//   outputs: List<Output>,
//   fraud_token_ref_input: Input,
// ) -> Bool {
//   todo
// }

// fn init_refund(
//   common: Common<PublisherDatum>,
//   node_to_mark_key: PubKeyHash,
//   sigs: List<PubKeyHash>,
//   validTo: POSIXTime,
// ) -> Bool {
//   todo
// }

// fn complete_refund(
//   common: Common<PublisherDatum>,
//   prev_node_key: PubKeyHash,
//   remove_node_key: PubKeyHash,
//   sigs: List<PubKeyHash>,
//   validFrom: POSIXTime,
//   refund_waiting_period: Int,
// ) -> Bool {
//   todo
// }
