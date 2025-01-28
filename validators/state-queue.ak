use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered.{
  Key, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/fraud_proof/catalogue.{
  Deinit, Init, MintRedeemer, NewFraudCategory, RemoveFraudCategory,
  SpendRedeemer,
}

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)

    ordered.list_state_transition(tokens)
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_token_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          ordered.init(node_in_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          ordered.deinit(node_outputs, node_mint, policy_id),
        }
      _ -> todo
    }
  }

  else(_) {
    fail
  }
}
