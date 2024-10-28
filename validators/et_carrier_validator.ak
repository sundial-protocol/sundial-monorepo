use aiken/collection/list.{length}
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{Input, OutputReference, Redeemer, Transaction}
use midgard/constants
use midgard/types/et.{
  ET_ABORT, ET_CONTINUE_PROOF, EvaluationThreadRedeemer, etTokenBurnt,
}
use midgard/utils

validator et_carrier_validator(
  partial_validator_script_hash: ByteArray,
  et_token_policy_id: PolicyId,
  next_carrier_validator_script_hash: ByteArray,
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
      ET_CONTINUE_PROOF(frauded_state_queue_node_ref_input_index) -> {
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        and {
          validate_frauded_state_queue_node_input(),
          correct_output_to_next_carrier_validator(),
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

fn correctOutputToNextCarrierValidator() {
  todo
}
