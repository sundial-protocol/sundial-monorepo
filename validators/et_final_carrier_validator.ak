use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, OutputReference, Redeemer, Transaction}
use midgard/types/et.{
  ET_ABORT, ET_FINISH_PROOF, EvaluationThreadRedeemer, etTokenBurnt,
}
use midgard/utils

validator et_final_carrier_validator(
  partial_validator_script_hash: ByteArray,
  et_token_policy_id: PolicyId,
) {
  spend(
    _datumOpt: Option<Data>,
    redeemer: EvaluationThreadRedeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      ET_ABORT ->
        etTokenBurnt(
          input.output.value,
          et_token_policy_id,
          et_token_asset_name,
        )
      ET_FINISH_PROOF(frauded_state_queue_node_ref_input_index) -> {
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        and {
          validate_frauded_state_queue_node_input(),
          etTokenBurnt(
            input.output.value,
            et_token_policy_id,
            et_token_asset_name,
          ),
          fpvsTokenMinted(),
          list.any(
            self.withdrawals,
            fn(withdrawal) {
              when withdrawal is {
                Pair(Script(scriptHash), _amnt) ->
                  scriptHash == partial_validator_script_hash
                _ -> False
              }
            },
          ),
        }
      }
      _ -> False
    }
  }

  else(_) {
    fail
  }
}

fn fpvsTokenMinted() {
  todo
}
