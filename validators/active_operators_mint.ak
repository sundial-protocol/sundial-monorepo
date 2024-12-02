use aiken/collection/list
use aiken/crypto.{sha2_256}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use linked_list/data.{Key, NodeDatum}
use linked_list/list as linked_list
use linked_list/utils as linked_list_utils
use midgard/types/datum.{HubOracleDatum, RegisteredOperatorData}
use midgard/types/redeemer.{ActiveOperatorsMintRedeemer}

validator active_operators(
  required_bond: Int,
  registration_duration: Int,
  slashing_penalty: Int,
  init_utxo_id: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(
    redeemer: ActiveOperatorsMintRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let common = linked_list.make_common(self, policy_id)
    when redeemer is {
      _ -> False
    }
  }

  else(_) {
    fail
  }
}
