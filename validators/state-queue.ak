use aiken/collection/list
use aiken/crypto
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/hub_oracle.{Datum as HubOracleDatum}
use midgard/registered_operators
use midgard/retired_operators
use midgard/scheduler.{Datum as SchedulerDatum}
use midgard/state_queue.{
  CommitBlockHeader, Datum, Deinit, Init, MergeToConfirmedState, MintRedeemer,
  RemoveFraudulentBlockHeader,
}

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)

    unordered.list_state_transition(tokens)
  }

  else(_) {
    fail
  }
}

//TODO Decide if using hub_oracle datum or hardcoded minting policies, not both!
validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  maturity_duration: Int,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          unordered.init(node_in_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          unordered.deinit(node_outputs, node_mint, policy_id),
        }
      CommitBlockHeader(
        operator,
        scheduler_asset_name,
        hub_oracle_ref_input_index,
        scheduler_state_ref_input_index,
        active_node_input_index,
        header_node_output_index,
        previous_header_node_output_index,
        active_operators_redeemer_index,
      ) -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type

        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: HubOracleDatum = hub_oracle_datum
        expect Some(scheduler_state_ref_input) =
          list.at(self.reference_inputs, scheduler_state_ref_input_index)
        expect InlineDatum(scheduler_state_datum) =
          scheduler_state_ref_input.output.datum
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect Some(header_node_output) =
          list.at(self.outputs, header_node_output_index)
        expect InlineDatum(header_node_datum) = header_node_output.datum
        expect parsed_header_node_datum: NodeDatum = header_node_datum
        expect header_node_block_data: Datum = parsed_header_node_datum.data
        expect parsed_header_node_block_data_as_bytearray: ByteArray =
          parsed_header_node_datum.data
        expect Some(previous_header_node_output) =
          list.at(self.outputs, previous_header_node_output_index)
        expect InlineDatum(previous_header_node_datum) =
          previous_header_node_output.datum
        expect parsed_previous_header_node_datum: NodeDatum =
          previous_header_node_datum
        expect previous_header_node_block_data: Datum =
          parsed_previous_header_node_datum.data
        expect Some(active_operators_redeemer_pair) =
          list.at(self.redeemers, active_operators_redeemer_index)
        expect Spend(active_node_input_output_reference) =
          active_operators_redeemer_pair.1st
        expect active_operators.UpdateCommitmentTime { .. } =
          active_operators_redeemer_pair.2nd
        expect Some(active_node_input) =
          list.at(self.inputs, active_node_input_index)
        expect InlineDatum(active_node_datum) = active_node_input.output.datum
        expect parsed_active_node_datum: ordered.NodeDatum = active_node_datum
        and {
          list.has(self.extra_signatories, operator),
          unordered.append_unsafe(
            parsed_header_node_datum.key,
            header_node_output_index,
            previous_header_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          operator == header_node_block_data.operator,
          parsed_header_node_datum.key == Key(
            crypto.sha2_256(parsed_header_node_block_data_as_bytearray),
          ),
          quantity_of(
            scheduler_state_ref_input.output.value,
            parsed_hub_oracle_datum.scheduler,
            scheduler_asset_name,
          ) == 1,
          operator == parsed_scheduler_state_datum.operator,
          header_node_block_data.start_time == previous_header_node_block_data.end_time,
          header_node_block_data.end_time == valid_to,
          header_node_block_data.end_time >= parsed_scheduler_state_datum.start_time,
          header_node_block_data.end_time <= parsed_scheduler_state_datum.end_time,
          active_node_input_output_reference == active_node_input.output_reference,
          ordered.prove_is_member(
            parsed_hub_oracle_datum.active_operators,
            parsed_active_node_datum.key,
            active_node_input.output,
          ),
          ordered.Key(operator) == parsed_active_node_datum.key,
        }
      }
      MergeToConfirmedState(
        header_node_key,
        header_node_input_index,
        confirmed_state_node_input_index,
      ) -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(confirmed_state_node_input) =
          list.at(self.inputs, confirmed_state_node_input_index)
        expect InlineDatum(confirmed_state_node_input_datum) =
          confirmed_state_node_input.output.datum
        expect parsed_confirmed_state_node_input_datum: NodeDatum =
          confirmed_state_node_input_datum
        expect Some(header_node_input) =
          list.at(self.outputs, header_node_input_index)
        expect InlineDatum(header_node_input_datum) = header_node_input.datum
        expect parsed_header_node_input_datum: NodeDatum =
          header_node_input_datum
        expect header_node_block_data: Datum =
          parsed_header_node_input_datum.data
        and {
          unordered.remove(
            Key(header_node_key),
            header_node_input_index,
            confirmed_state_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          unordered.prove_is_root_node(
            policy_id,
            confirmed_state_node_input.output,
          ),
          valid_from >= header_node_block_data.end_time + maturity_duration,
          //TODO should we check that key and link of confirmed_state_node_output are still the same?
          parsed_header_node_input_datum.data == parsed_confirmed_state_node_input_datum.data,
          //TODO create a settlement queue node here if the merged block contains deposits/withdrawals
          True,
        }
      }
      RemoveFraudulentBlockHeader {
        removed_node_input_index,
        predecessor_node_input_index,
        operators_redeemer_index,
        fraud_proof_ref_input_index,
        fraud_proof_asset_name,
        fraudulent_operator,
        operator_status,
      } -> {
        expect Some(fraud_proof_ref_input) =
          list.at(self.reference_inputs, fraud_proof_ref_input_index)
        expect Some(removed_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect Some(predecessor_node_input) =
          list.at(self.inputs, predecessor_node_input_index)
        let predecessor_node_key =
          unordered.get_key(
            unordered.get_raw_key_and_link(predecessor_node_input.output),
          )

        //Get last 28 bytes of fraud_proof_asset_name
        let fraud_proof_block_hash =
          bytearray.drop(
            fraud_proof_asset_name,
            bytearray.length(fraud_proof_asset_name) - 28,
          )
        and {
          unordered.remove(
            Key(fraudulent_operator),
            removed_node_input_index,
            predecessor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          when operator_status is {
            registered_operators.Active -> {
              expect Some(active_operators_redeemer_pair) =
                list.at(self.redeemers, operators_redeemer_index)
              expect Mint(mint_script_hash) = active_operators_redeemer_pair.1st
              expect active_operators.RemoveOperatorSlashBond {
                slashed_active_operator_key,
                ..
              } = active_operators_redeemer_pair.2nd
              and {
                mint_script_hash == active_operators_script_hash,
                fraudulent_operator == slashed_active_operator_key,
              }
            }
            registered_operators.Retired -> {
              expect Some(retired_operators_redeemer_pair) =
                list.at(self.redeemers, operators_redeemer_index)
              expect Mint(mint_script_hash) =
                retired_operators_redeemer_pair.1st
              expect retired_operators.RemoveOperatorSlashBond {
                slashed_retired_operator_key,
                ..
              } = retired_operators_redeemer_pair.2nd
              and {
                mint_script_hash == retired_operators_script_hash,
                fraudulent_operator == slashed_retired_operator_key,
              }
            }
            _ -> False
          },
          quantity_of(
            fraud_proof_ref_input.output.value,
            fraud_proof_script_hash,
            fraud_proof_asset_name,
          ) == 1,
          or {
            Key(fraud_proof_block_hash) == predecessor_node_key,
            and {
              fraud_proof_block_hash == fraudulent_operator,
              unordered.prove_is_last_node(policy_id, removed_node_input.output),
            },
          },
        }
      }
    }
  }

  else(_) {
    fail
  }
}
