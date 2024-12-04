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
use midgard/types/redeemer.{ROMR_Deinit,
  ROMR_Init, RetiredOperatorsMintRedeemer}

validator retired_operators(
  required_bond: Int,
  registration_duration: Int,
  slashing_penalty: Int,
  init_utxo_id: ByteArray,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(
    redeemer: RetiredOperatorsMintRedeemer,
    policy_id: PolicyId,
    self: Transaction,
  ) {
    let common = linked_list.make_common(self, policy_id)
    when redeemer is {
      ROMR_Init(init_utxo_index) -> {
        expect Some(init_input) = list.at(self.inputs, init_utxo_index)
        let init_input_utxo_id =
          sha2_256(
            bytearray.push(
              init_input.output_reference.transaction_id,
              init_input.output_reference.output_index,
            ),
          )
        and {
          //TODO Should we call here linked_list.list_state_transition()?
          init_input_utxo_id == init_utxo_id,
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == 1,
          linked_list.init(common),
        }
      }
      ROMR_Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, init_utxo_id) == -1,
          linked_list.deinit(common),
        }
      _ -> False
    }
  }

  else(_) {
    fail
  }
}
